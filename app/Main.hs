{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, void, when, zipWithM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set qualified as S
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core
import Life
  ( Universe,
    beacon,
    blinker,
    block,
    fumador,
    generaciones,
    generacionesToroidales,
    glider,
    gliderGun,
    lwss,
    stepUniverseToroidal,
    toMatrix,
    toad,
    trasladar,
  )
import System.Random (randomRIO)

-- | Punto de entrada de la aplicación.
-- Levanta un pequeño servidor web local para mostrar la interfaz.
main :: IO ()
main = startGUI defaultConfig {jsPort = Just 8023} setup

-- | Configura la ventana y todos los elementos de la interfaz.
-- Aquí creamos el estado del "Juego de la Vida", los controles y el bucle de actualización.
setup :: Window -> UI ()
setup ventana = do
  -- Título de la pestaña/navegador
  void $ return ventana # set UI.title "El juego de la vida"

  -- Estado de la simulación: guardamos el universo actual en un IORef
  refUniverso <- liftIO (newIORef universoInicial)

  -- Rejilla de visualización como una grilla de bloques coloreados

  rejilla <-
    UI.div
      # set
        style
        [ ("display", "grid"),
          ("grid-template-columns", "repeat(" ++ show anchoVista ++ ", " ++ show tamCelda ++ "px)"),
          ("grid-auto-rows", show tamCelda ++ "px"),
          ("gap", "0px"),
          ("background-color", "#000"),
          ("padding", "0px"),
          ("box-shadow", "0 0 4px rgba(0,0,0,0.2)")
        ]

  -- Pre-creamos todas las celdas una sola vez y luego sólo actualizamos el color
  let totalCeldas = anchoVista * altoVista
      px n = show n ++ "px"
  celdas <-
    mapM
      ( \_ ->
          UI.div
            # set
              style
              [ ("width", px tamCelda),
                ("height", px tamCelda),
                ("box-sizing", "border-box"),
                ("border", "0.2px solid #f2f0e7"),
                ("background-color", colorMuerta)
              ]
      )
      [1 .. totalCeldas]
  void $ element rejilla # set UI.children celdas

  -- Renderiza el universo coloreando las celdas ya existentes
  let renderizar u = do
        let matriz =
              toMatrix
                (minXVista, minXVista + anchoVista - 1)
                (minYVista, minYVista + altoVista - 1)
                u
            vivos = concat matriz
        zipWithM_
          ( \cel viva ->
              void $
                element cel
                  # set
                    style
                    [ ("width", px tamCelda),
                      ("height", px tamCelda),
                      ("box-sizing", "border-box"),
                      ("border", "0.2px solid #f2f0e7"),
                      ("background-color", if viva then colorViva else colorMuerta)
                    ]
          )
          celdas
          vivos

  -- Lista de patrones disponibles
  let patronesDisponibles =
        [ ("Glider", glider),
          ("Block", block),
          ("Blinker", blinker),
          ("Toad", toad),
          ("Beacon", beacon),
          ("Fumador (Puffer)", fumador),
          ("Nave Ligera", lwss),
          ("Cañón de Planeadores", gliderGun)
        ]

  -- Temporizador para avanzar automáticamente cada cierto intervalo (ms)
  let intervaloInicial = 150 -- valor por defecto del intervalo en milisegundos

  -- Referencia para el stream de generaciones
  refGeneraciones <- liftIO (newIORef (generacionesToroidales anchoVista altoVista universoInicial))
  refPausado <- liftIO (newIORef True) -- Comenzar pausado

  -- Función para avanzar a la siguiente generación
  let avanzarGeneracion = do
        gs <- liftIO (readIORef refGeneraciones)
        case gs of
          (u : us) -> do
            liftIO (writeIORef refGeneraciones us)
            liftIO (writeIORef refUniverso u)
            renderizar u
          [] -> return () -- No debería ocurrir

  -- Referencia para el temporizador actual (para poder cambiar su intervalo)
  refTemporizador <- liftIO (newIORef Nothing)

  -- Función para crear un nuevo temporizador con un intervalo dado
  let crearTemporizador ms = do
        t <- UI.timer # set UI.interval ms
        liftIO $ writeIORef refTemporizador (Just t)
        -- Configurar el evento de tick para el nuevo temporizador
        void $ t # on UI.tick $ \_ -> do
          pausado <- liftIO $ readIORef refPausado
          unless pausado $ do
            avanzarGeneracion
        return t

  -- Crear el temporizador inicial
  temporizadorInicial <- crearTemporizador intervaloInicial
  btnIniciar <- UI.button #+ [string "Iniciar"]
  btnPausar <- UI.button #+ [string "Pausar"]
  btnPaso <- UI.button #+ [string "Paso"]
  btnReiniciar <- UI.button #+ [string "Reiniciar"]
  btnAleatorio <- UI.button #+ [string "Aleatorio"]

  -- Selector de patrones
  lblPatron <- UI.span # set text "Patrón: "
  selectorPatron <-
    UI.select
      # set (UI.attr "style") "padding: 5px; margin: 5px;"

  -- Llenar el selector con los patrones disponibles
  opciones <-
    mapM
      ( \(i, (nombre, _)) ->
          UI.option # set text nombre # set (UI.attr "value") (show i)
      )
      (zip [0 ..] patronesDisponibles)
  void $ element selectorPatron # set UI.children opciones

  -- Slider para controlar la velocidad (intervalo). Mientras más pequeño el valor, más rápido.
  etiquetaVelocidad <- UI.span # set text ("Intervalo: " ++ show intervaloInicial ++ " ms")
  sliderVelocidad <-
    UI.input
      # set UI.type_ "range"
      # set (UI.attr "min") "30" -- mínimo 30ms
      # set (UI.attr "max") "1000" -- máximo 1000ms
      # set (UI.attr "step") "10"
      # set (UI.value) (show intervaloInicial)
      # set style [("width", "250px")]

  -- Contenedor centrado usando Flexbox de CSS para ubicar todo en el medio de la pantalla
  contenedor <-
    UI.div
      # set
        style
        [ ("display", "flex"),
          ("flex-direction", "column"),
          ("justify-content", "center"), -- centra verticalmente
          ("align-items", "center"), -- centra horizontalmente
          ("height", "100vh"),
          ("gap", "12px")
        ]

  -- Construcción del layout: título, fila con botones y el área de visualización
  titulo <- UI.h1 # set text "Juego de la Vida de Conway"
  filaPatron <-
    UI.div
      # set style [("display", "flex"), ("gap", "8px"), ("align-items", "center")]
      #+ [ element lblPatron,
           element selectorPatron
         ]
  filaBotones <-
    UI.div
      # set style [("display", "flex"), ("gap", "8px")]
      #+ [ element btnIniciar,
           element btnPausar,
           element btnPaso,
           element btnReiniciar,
           element btnAleatorio
         ]
  filaVelocidad <-
    UI.div
      # set style [("display", "flex"), ("align-items", "center"), ("gap", "10px")]
      #+ [ element etiquetaVelocidad,
           element sliderVelocidad
         ]
  void $
    element contenedor
      #+ [ element titulo,
           element filaPatron,
           element filaBotones,
           element filaVelocidad,
           element rejilla
         ]

  -- Agregamos el contenedor centrado al body de la ventana
  void $ getBody ventana #+ [element contenedor]

  -- Render inicial (pinta el universo actual)
  void $ levantarUIRef refUniverso renderizar

  -- Función auxiliar para aplicar un nuevo intervalo al temporizador y actualizar la etiqueta
  -- Recrear el temporizador con el nuevo intervalo
  let aplicarIntervalo ms = do
        void $ element etiquetaVelocidad # set text ("Intervalo: " ++ show ms ++ " ms")
        -- Detener el temporizador actual si existe
        maybeTimer <- liftIO $ readIORef refTemporizador
        case maybeTimer of
          Just t -> void $ t # UI.stop
          Nothing -> return ()
        -- Crear un nuevo temporizador con el nuevo intervalo
        void $ crearTemporizador ms
        -- Si no estaba pausado, iniciar el nuevo temporizador
        pausado <- liftIO $ readIORef refPausado
        unless pausado $ do
          newTimer <- liftIO $ readIORef refTemporizador
          case newTimer of
            Just t -> void $ t # UI.start
            Nothing -> return ()

  -- Función para reiniciar la simulación con el patrón seleccionado
  let reiniciarConPatron = do
        indiceSeleccionado <- get value selectorPatron
        let (_, patron) = patronesDisponibles !! read indiceSeleccionado
        
        let coords = S.toList patron
        if null coords
          then do
            liftIO $ do
              writeIORef refUniverso patron
              writeIORef refGeneraciones (generacionesToroidales anchoVista altoVista patron)
              writeIORef refPausado True
            renderizar patron
          else do
            let xs = map fst coords
                ys = map snd coords
                minPX = minimum xs
                minPY = minimum ys
                patW = maximum xs - minPX + 1
                patH = maximum ys - minPY + 1
                maxX = max 0 (anchoVista - patW)
                maxY = max 0 (altoVista - patH)
            rx <- liftIO $ randomRIO (0, maxX)
            ry <- liftIO $ randomRIO (0, maxY)
            let dx = rx - minPX + minXVista
                dy = ry - minPY + minYVista
                patronPos = trasladar (dx, dy) patron
            liftIO $ do
              writeIORef refUniverso patronPos
              writeIORef refGeneraciones (generacionesToroidales anchoVista altoVista patronPos)
              writeIORef refPausado True
            renderizar patronPos

  -- Eventos de los botones
  void $ btnIniciar # on UI.click $ \_ -> do
    maybeTimer <- liftIO $ readIORef refTemporizador
    case maybeTimer of
      Just t -> void $ t # UI.start
      Nothing -> return ()
    liftIO $ writeIORef refPausado False
    return ()

  void $ btnPausar # on UI.click $ \_ -> do
    maybeTimer <- liftIO $ readIORef refTemporizador
    case maybeTimer of
      Just t -> void $ t # UI.stop
      Nothing -> return ()
    liftIO $ writeIORef refPausado True
    return ()

  void $ btnPaso # on UI.click $ \_ -> do
    pausado <- liftIO $ readIORef refPausado
    when pausado $ do
      avanzarGeneracion

  void $ btnReiniciar # on UI.click $ \_ -> do
    reiniciarConPatron
    return ()

  void $ btnAleatorio # on UI.click $ \_ -> do
    u <- liftIO $ universoAleatorio anchoVista altoVista 0.2
    liftIO $ do
      writeIORef refUniverso u
      writeIORef refGeneraciones (generacionesToroidales anchoVista altoVista u)
      writeIORef refPausado True
    renderizar u

  -- Cambiar la velocidad con el slider
  void $ sliderVelocidad # on UI.valueChange $ \n -> do
    let ms = read n :: Int
    aplicarIntervalo ms

  -- Evento del slider en tiempo real (evento DOM 'input') para actualizar mientras se arrastra
  void $ sliderVelocidad # on (domEvent "input") $ \_ -> do
    valTexto <- get UI.value sliderVelocidad
    case reads valTexto :: [(Int, String)] of
      [(ms, "")] -> aplicarIntervalo ms
      _ -> return ()
  where
    -- Dimensiones de la ventana de visualización (en celdas)
    anchoVista, altoVista :: Int
    anchoVista = 50
    altoVista = 30
    -- Tamaño de cada celda y colores (mover aquí para evitar problemas de let en el do)
    tamCelda :: Int
    tamCelda = 14
    colorViva, colorMuerta :: String
    colorViva = "#28a745"
    colorMuerta = "#000000"

    -- Origen de la ventana de visualización (puede moverse en el futuro)
    minXVista, minYVista :: Int
    minXVista = 0
    minYVista = 0

    -- Universo inicial: dos planeadores desplazados para que se vean en pantalla
    universoInicial :: Universe
    universoInicial = trasladar (10, 5) glider <> trasladar (20, 10) glider

    -- Renderizado a texto: convierte el universo en líneas con caracteres de bloque
    -- width/height: tamaño del recorte a mostrar, minX/minY: esquina superior izquierda
    renderizarUniverso :: Int -> Int -> Int -> Int -> Universe -> String
    renderizarUniverso width height minX minY universo =
      unlines
        [ [ if celdaViva then '\9632' else '·' -- '■' como bloque, '·' como vacío
            | celdaViva <- fila
          ]
          | fila <- toMatrix (minX, minX + width - 1) (minY, minY + height - 1) universo
        ]

    -- Utilidades para trabajar con IORef
    modificarRef :: IORef a -> (a -> a) -> IO ()
    modificarRef referencia f = readIORef referencia >>= writeIORef referencia . f

    levantarUIRef :: IORef Universe -> (Universe -> UI b) -> UI b
    levantarUIRef referencia f = liftIO (readIORef referencia) >>= f

    -- Construye un universo aleatorio dentro de un área de ancho x alto.
    -- 'densidad' es la probabilidad de que una celda nazca viva (0.0 a 1.0).
    -- Implementación sencilla y clara: recorre todas las coordenadas y decide con random.
    universoAleatorio :: Int -> Int -> Double -> IO Universe
    universoAleatorio ancho alto densidad = do
      pares <-
        sequence
          [ do
              r <- randomRIO (0.0, 1.0)
              pure ((x, y), r)
            | x <- [0 .. ancho - 1],
              y <- [0 .. alto - 1]
          ]
      pure $ S.fromList [(x, y) | ((x, y), r) <- pares, r < densidad]
