{-# LANGUAGE OverloadedStrings #-}
module Main where

import Life (Universe, glider, generaciones, trasladar, toMatrix)
import Control.Monad (forM_)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Concurrent (threadDelay)

-- | Punto de entrada para la versión de consola del simulador.
-- Consume el stream perezoso de generaciones y las presenta en consola
-- con pausas entre generaciones, simulando el avance del tiempo.
main :: IO ()
main = do
  -- Desactivar el buffering para que la salida se muestre inmediatamente
  hSetBuffering stdout NoBuffering
  
  putStrLn "=========================================="
  putStrLn "  Simulador del Juego de la Vida"
  putStrLn "  Versión Consola"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn "Consumiendo el stream perezoso de generaciones..."
  putStrLn "Presiona Ctrl+C para detener la simulación"
  putStrLn ""
  
  -- Universo inicial: dos planeadores desplazados
  let universoInicial = trasladar (10, 5) glider <> trasladar (20, 10) glider
  
  -- Consumir el stream infinito de generaciones y presentarlas en consola
  simularEnConsola universoInicial

-- | Simula el Juego de la Vida consumiendo el stream perezoso de generaciones.
-- Presenta cada generación en la consola de manera legible con pausas entre ellas,
-- simulando el avance del tiempo.
simularEnConsola :: Universe -> IO ()
simularEnConsola universoInicial = do
  -- Obtener el stream perezoso (lista infinita) de generaciones
  let generacionesStream = generaciones universoInicial
      -- Dimensiones de la ventana de visualización
      ancho = 50
      alto = 30
      minX = 0
      minY = 0
      -- Intervalo entre generaciones en microsegundos (500ms = 500000 microsegundos)
      -- Esta pausa simula el avance del tiempo
      intervaloMicrosegundos = 500000
  
  -- Consumir el stream infinito de generaciones usando forM_
  -- forM_ es una función de orden superior que itera sobre la lista
  forM_ (zip [(0::Int)..] generacionesStream) $ \(numGen, universo) -> do
    -- Limpiar la pantalla para mostrar solo la generación actual
    limpiarPantalla
    
    -- Mostrar información de la generación
    putStrLn "=========================================="
    putStrLn "  Simulador del Juego de la Vida"
    putStrLn "=========================================="
    putStrLn ""
    putStrLn $ "Generación: " ++ show numGen
    putStrLn ""
    putStrLn "------------------------------------------"
    putStrLn ""
    
    -- Renderizar el universo a texto y presentarlo en consola
    let representacion = renderizarUniverso ancho alto minX minY universo
    putStrLn representacion
    putStrLn ""
    putStrLn "------------------------------------------"
    putStrLn "Presiona Ctrl+C para detener"
    putStrLn ""
    
    -- Pausa entre generaciones para simular el avance del tiempo
    -- Esta pausa permite visualizar cada generación antes de pasar a la siguiente
    threadDelay intervaloMicrosegundos

-- | Renderizado a texto: convierte el universo en líneas con caracteres.
-- Convierte el universo (Set de coordenadas) en una representación legible
-- usando caracteres ASCII para mejor compatibilidad con consolas.
-- width/height: tamaño del recorte a mostrar, minX/minY: esquina superior izquierda
renderizarUniverso :: Int -> Int -> Int -> Int -> Universe -> String
renderizarUniverso width height minX minY universo =
  unlines [ [ if celdaViva then '#' else '.'  -- '#' para celdas vivas, '.' para muertas
              | celdaViva <- fila ]
          | fila <- toMatrix (minX, minX + width - 1) (minY, minY + height - 1) universo]

-- | Limpia la pantalla usando códigos ANSI de escape
limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"
