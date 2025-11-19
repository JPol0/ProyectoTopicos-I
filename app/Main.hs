{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, void, when, zipWithM_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
  -- Referencia para el stream de generaciones (moved arriba para uso en handlers)
  refGeneraciones <- liftIO (newIORef (generacionesToroidales anchoVista altoVista universoInicial))
  refPausado <- liftIO (newIORef True) -- Comenzar pausado
  -- Contador de generaciones para mostrar en la interfaz
  refGeneracion <- liftIO (newIORef (0 :: Int))

  celdas <-
    mapM
      ( \i -> do
          cel <-
            UI.div
              # set
                style
                [ ("width", px tamCelda),
                  ("height", px tamCelda),
                  ("box-sizing", "border-box"),
                  ("border", "0.2px solid #f2f0e7"),
                  ("background-color", colorMuerta)
                ]
          -- Al hacer click en una celda alternamos su estado vivo/muerto
          void $ on UI.click cel $ \_ -> do
            let x = minXVista + (i `mod` anchoVista)
                y = minYVista + (i `div` anchoVista)
                coord = (x, y)
            u <- liftIO $ readIORef refUniverso
            let u' = if S.member coord u then S.delete coord u else S.insert coord u
            -- actualizar estado (IO) y marcar en pausa
            liftIO $ do
              writeIORef refUniverso u'
              writeIORef refGeneraciones (generacionesToroidales anchoVista altoVista u')
              writeIORef refPausado True
            -- actualizar solo la celda clickeada en el DOM para evitar dependencia de 'renderizar'
            void $
              element cel
                # set
                  style
                  [ ("background-color", if S.member coord u' then colorViva else colorMuerta),
                    ("width", px tamCelda),
                    ("height", px tamCelda),
                    ("box-sizing", "border-box"),
                    ("border", "0.2px solid #f2f0e7")
                  ]
          return cel
      )
      [0 .. totalCeldas - 1]
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
                      ("border", "1px solid #333"),
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

  -- Etiqueta de generación (se crea antes para poder actualizarla en avanzarGeneracion)
  lblGeneracionTitulo <- UI.span # set text "Generación: "
  lblGeneracionValor <- UI.span # set text "0"

  -- Temporizador para avanzar automáticamente cada cierto intervalo (ms)
  let intervaloInicial = 150 -- valor por defecto del intervalo en milisegundos

  -- Función para avanzar a la siguiente generación (actualiza contador y etiqueta)
  let avanzarGeneracion = do
        gs <- liftIO (readIORef refGeneraciones)
        case gs of
          (u : us) -> do
            liftIO (writeIORef refGeneraciones us)
            liftIO (writeIORef refUniverso u)
            liftIO $ modifyIORef' refGeneracion (+ 1)
            g <- liftIO (readIORef refGeneracion)
            void $ element lblGeneracionValor # set text (show g)
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
  -- Estilos base para botones "suaves"
  let estiloBotonBase =
        [ ("background", "linear-gradient(180deg,#3a3f44,#2a2d30"),
          ("color", "#f1f1f1"),
          ("padding", "8px 14px"),
          ("border", "1px solid #444"),
          ("border-radius", "6px"),
          ("font-size", "14px"),
          ("letter-spacing", "0.5px"),
          ("cursor", "pointer"),
          ("transition", "background 160ms, transform 120ms, box-shadow 160ms"),
          ("box-shadow", "0 2px 4px rgba(0,0,0,0.35)"),
          ("user-select", "none")
        ]
      hoverJS = "this.style.background='#50565c'"
      activeJS = "this.style.transform='scale(0.95)'"
      leaveJS = "this.style.background='linear-gradient(180deg,#3a3f44,#2a2d30)';this.style.transform='scale(1)'"
      mkBoton txt extraColor = do
        b <-
          UI.button
            #+ [string txt]
            # set style (estiloBotonBase ++ extraColor)
            # set (UI.attr "onmouseenter") hoverJS
            # set (UI.attr "onmousedown") activeJS
            # set (UI.attr "onmouseleave") leaveJS
            # set (UI.attr "onmouseup") "this.style.transform='scale(1)'"
        pure b
  btnIniciar <- mkBoton "Iniciar" [("background", "linear-gradient(180deg,#2e7d32,#1b5e20)")]
  btnPausar <- mkBoton "Pausar" [("background", "linear-gradient(180deg,#c62828,#8e0000)")]
  btnPaso <- mkBoton "Paso" [("background", "linear-gradient(180deg,#455a64,#263238)")]
  btnReiniciar <- mkBoton "Reiniciar" [("background", "linear-gradient(180deg,#1565c0,#0d47a1)")]
  btnAleatorio <- mkBoton "Aleatorio" [("background", "linear-gradient(180deg,#6a1b9a,#4a148c)")]
  btnLimpiar <- mkBoton "Limpiar" [("background", "linear-gradient(180deg,#37474f,#21292e)")]

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
      (zip [0 :: Int ..] patronesDisponibles)
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
          ("gap", "16px"),
          ("background", "#1e1e1e"),
          ("color", "#eee"),
          ("font-family", "Segoe UI, Arial, sans-serif")
        ]

  panel <-
    UI.div
      # set
        style
        [ ("display", "flex"),
          ("flex-direction", "column"),
          ("gap", "12px"),
          ("background", "#242424"),
          ("padding", "18px 22px"),
          ("border-radius", "10px"),
          ("box-shadow", "0 4px 18px rgba(0,0,0,0.35)"),
          ("border", "1px solid #333"),
          ("min-width", "720px")
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
           element btnAleatorio,
           element btnLimpiar
         ]
  filaVelocidad <-
    UI.div
      # set style [("display", "flex"), ("align-items", "center"), ("gap", "10px")]
      #+ [ element etiquetaVelocidad,
           element sliderVelocidad
         ]
  void $
    element contenedor
      #+ [ element panel
             #+ [ element titulo,
                  UI.div
                    # set style [("display", "flex"), ("gap", "6px")]
                    #+ [element lblGeneracionTitulo, element lblGeneracionValor],
                  element filaPatron,
                  element filaBotones,
                  element filaVelocidad,
                  UI.div
                    # set style [("font-size", "12px"), ("opacity", "0.85")]
                    #+ [string "Clic en una celda para alternar. Limpiar vacía el universo. Reiniciar coloca el patrón elegido aleatoriamente."],
                  element rejilla
                ]
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

  let actualizarGeneracionLabel = do
        g <- liftIO $ readIORef refGeneracion
        void $ element lblGeneracionValor # set text (show g)

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
              writeIORef refGeneracion 0
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
              writeIORef refGeneracion 0
            renderizar patronPos
        actualizarGeneracionLabel

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
      writeIORef refGeneracion 0
    renderizar u
    actualizarGeneracionLabel

  void $ btnLimpiar # on UI.click $ \_ -> do
    let vacio = S.empty
    liftIO $ do
      writeIORef refUniverso vacio
      writeIORef refGeneraciones (generacionesToroidales anchoVista altoVista vacio)
      writeIORef refPausado True
      writeIORef refGeneracion 0
    renderizar vacio
    actualizarGeneracionLabel

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
    colorViva = "#ffd600"
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
