{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void, zipWithM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as S
import Life (Universe, glider, stepUniverseToroidal, toMatrix)
import System.Random (randomRIO)

-- | Punto de entrada de la aplicación.
-- Levanta un pequeño servidor web local para mostrar la interfaz.
main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023 } setup

-- | Configura la ventana y todos los elementos de la interfaz.
-- Aquí creamos el estado del "Juego de la Vida", los controles y el bucle de actualización.
setup :: Window -> UI ()
setup ventana = do
  -- Título de la pestaña/navegador
  void $ return ventana # set UI.title "Conway - Threepenny"

  -- Estado de la simulación: guardamos el universo actual en un IORef
  refUniverso <- liftIO (newIORef universoInicial)

  -- Rejilla de visualización como una grilla de bloques coloreados

  rejilla <- UI.div # set style [
              ("display","grid"),
              ("grid-template-columns", "repeat(" ++ show anchoVista ++ ", " ++ show tamCelda ++ "px)"),
              ("grid-auto-rows", show tamCelda ++ "px"),
              ("gap","0px"),
              ("background-color", "#000"),
              ("padding","0px"),
              ("box-shadow","0 0 4px rgba(0,0,0,0.2)")
            ]

  -- Pre-creamos todas las celdas una sola vez y luego sólo actualizamos el color
  let
    totalCeldas = anchoVista * altoVista
    px n = show n ++ "px"
  celdas <- mapM
    (\_ -> UI.div # set style [
             ("width",  px tamCelda),
             ("height", px tamCelda),
             ("box-sizing","border-box"),
             ("border","0.2px solid #f2f0e7"),
             ("background-color", colorMuerta)
           ])
    [1..totalCeldas]
  void $ element rejilla # set UI.children celdas

  -- Renderiza el universo coloreando las celdas ya existentes
  let renderizar u = do
        let matriz = toMatrix (minXVista, minXVista + anchoVista - 1)
                               (minYVista, minYVista + altoVista - 1)
                               u
            vivos = concat matriz
        zipWithM_ (\cel viva ->
              void $ element cel # set style [
                ("width",  px tamCelda),
                ("height", px tamCelda),
                ("box-sizing","border-box"),
                ("border","0.2px solid #f2f0e7"),
                ("background-color", if viva then colorViva else colorMuerta)
              ]
            ) celdas vivos

  -- Controles: botones de iniciar, pausar, avanzar un paso, reiniciar y aleatorio
  btnIniciar <- UI.button #+ [string "Iniciar"]
  btnPausar  <- UI.button #+ [string "Pausar"]
  btnPaso    <- UI.button #+ [string "Paso"]
  btnReiniciar <- UI.button #+ [string "Reiniciar"]
  btnAleatorio <- UI.button #+ [string "Aleatorio"]

  -- Temporizador para avanzar automáticamente cada cierto intervalo (ms)
  let intervaloInicial = 150  -- valor por defecto del intervalo en milisegundos
  temporizador <- UI.timer # set UI.interval intervaloInicial

  -- Slider para controlar la velocidad (intervalo). Mientras más pequeño el valor, más rápido.
  etiquetaVelocidad <- UI.span # set text ("Intervalo: " ++ show intervaloInicial ++ " ms")
  sliderVelocidad <- UI.input # set UI.type_ "range"
                              # set (UI.attr "min") "30"    -- mínimo 30ms
                              # set (UI.attr "max") "1000"  -- máximo 1000ms
                              # set (UI.attr "step") "10"
                              # set (UI.value) (show intervaloInicial)
                              # set style [("width","250px")]

  -- Contenedor centrado usando Flexbox de CSS para ubicar todo en el medio de la pantalla
  contenedor <- UI.div # set style [
                  ("display","flex"),
                  ("flex-direction","column"),
                  ("justify-content","center"),   -- centra verticalmente
                  ("align-items","center"),        -- centra horizontalmente
                  ("height","100vh"),
                  ("gap","12px")
                ]

  -- Construcción del layout: título, fila con botones y el área de visualización
  void $ element contenedor #+ [
          UI.h1 #+ [string "Juego de la Vida"],
    row [element btnIniciar, element btnPausar, element btnPaso, element btnReiniciar, element btnAleatorio],
    row [element etiquetaVelocidad, element sliderVelocidad],
          element rejilla
        ]

  -- Agregamos el contenedor centrado al body de la ventana
  void $ getBody ventana #+ [element contenedor]

  -- Render inicial (pinta el universo actual)
  void $ levantarUIRef refUniverso renderizar

  -- Eventos de los botones
  on UI.click btnIniciar $ \_ -> UI.start temporizador
  on UI.click btnPausar  $ \_ -> UI.stop  temporizador
  on UI.click btnPaso    $ \_ -> do
    -- Avanza una generación y vuelve a dibujar
    liftIO $ modificarRef refUniverso (stepUniverseToroidal anchoVista altoVista)
    levantarUIRef refUniverso renderizar

  on UI.click btnReiniciar $ \_ -> do
    -- Vuelve al patrón inicial definido más abajo y re-renderiza
    liftIO $ writeIORef refUniverso universoInicial
    levantarUIRef refUniverso renderizar

  on UI.click btnAleatorio $ \_ -> do
    -- Genera un universo aleatorio con una densidad base (25% vivas)
    uRand <- liftIO $ universoAleatorio anchoVista altoVista 0.25
    liftIO $ writeIORef refUniverso uRand
    levantarUIRef refUniverso renderizar

  -- Función auxiliar para aplicar un nuevo intervalo al temporizador y actualizar la etiqueta
  let aplicarIntervalo ms = do
        void $ element etiquetaVelocidad # set text ("Intervalo: " ++ show ms ++ " ms")
        void $ return temporizador # set UI.interval ms

  -- Evento del slider: al cambiar el valor (evento 'change')
  on UI.valueChange sliderVelocidad $ \valTexto -> do
    case reads valTexto :: [(Int, String)] of
      [(ms, "")] -> aplicarIntervalo ms
      _           -> return ()

  -- Evento del slider en tiempo real (evento DOM 'input') para actualizar mientras se arrastra
  on (domEvent "input") sliderVelocidad $ \_ -> do
    valTexto <- get UI.value sliderVelocidad
    case reads valTexto :: [(Int, String)] of
      [(ms, "")] -> aplicarIntervalo ms
      _           -> return ()

  -- Evento del temporizador: avanza automáticamente y re-renderiza
  on UI.tick temporizador $ \_ -> do
    liftIO $ modificarRef refUniverso (stepUniverseToroidal anchoVista altoVista)
    levantarUIRef refUniverso renderizar

  where
  -- Dimensiones de la ventana de visualización (en celdas)
  anchoVista, altoVista :: Int
  anchoVista  = 50
  altoVista   = 30
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
  universoInicial = trasladar (10, 5) glider <> trasladar (20,10) glider

  -- Renderizado a texto: convierte el universo en líneas con caracteres de bloque
  -- width/height: tamaño del recorte a mostrar, minX/minY: esquina superior izquierda
  renderizarUniverso :: Int -> Int -> Int -> Int -> Universe -> String
  renderizarUniverso width height minX minY universo =
    unlines [ [ if celdaViva then '\9632' else '·'  -- '■' como bloque, '·' como vacío
                | celdaViva <- fila ]
            | fila <- toMatrix (minX, minX + width - 1) (minY, minY + height - 1) universo]

  -- Utilidades para trabajar con IORef
  modificarRef :: IORef a -> (a -> a) -> IO ()
  modificarRef referencia f = readIORef referencia >>= writeIORef referencia . f

  levantarUIRef :: IORef Universe -> (Universe -> UI b) -> UI b
  levantarUIRef referencia f = liftIO (readIORef referencia) >>= f

  -- Trasladar (desplazar) un universo por un vector (dx,dy)
  trasladar :: (Int,Int) -> Universe -> Universe
  trasladar (dx,dy) = S.mapMonotonic (\(x,y) -> (x+dx, y+dy))

  -- Construye un universo aleatorio dentro de un área de ancho x alto.
  -- 'densidad' es la probabilidad de que una celda nazca viva (0.0 a 1.0).
  -- Implementación sencilla y clara: recorre todas las coordenadas y decide con random.
  universoAleatorio :: Int -> Int -> Double -> IO Universe
  universoAleatorio ancho alto densidad = do
    pares <- sequence
      [ do r <- randomRIO (0.0, 1.0)
           pure ((x,y), r)
      | x <- [0 .. ancho - 1]
      , y <- [0 .. alto  - 1]
      ]
    pure $ S.fromList [ (x,y) | ((x,y), r) <- pares, r < densidad ]
