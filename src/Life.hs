module Life (
  -- Exportamos los tipos y funciones principales para el Juego de la Vida
  Cell(..), Universe, Coord, fromList,
  stepUniverse,
  -- Versión sencilla con bordes toroidales (envuelve por los extremos)
  stepUniverseToroidal,
  glider, block, toMatrix
) where

import Data.Set (Set)
import qualified Data.Set as S

-- | Una celda puede estar viva. (No almacenamos celdas muertas, sólo las vivas para ahorrar memoria.)
data Cell = Alive deriving (Eq, Show)

-- | Coordenadas de una celda (x,y)
type Coord = (Int, Int)

-- | Universo representado como un conjunto de coordenadas de celdas vivas.
type Universe = Set Coord

-- | Construir un universo a partir de una lista de coordenadas.
fromList :: [Coord] -> Universe
fromList = S.fromList

-- | Offsets del vecindario de Moore (8 vecinos alrededor).
offsetsVecinos :: [Coord]
offsetsVecinos = [(-1,-1), (0,-1), (1,-1),
                  (-1, 0),          (1, 0),
                  (-1, 1), (0, 1), (1, 1)]

-- | Contar cuántos vecinos vivos tiene una celda.
contarVecinosVivos :: Universe -> Coord -> Int
contarVecinosVivos universo (x,y) = length $ filter (`S.member` universo) vecinos
  where vecinos = [(x+dx, y+dy) | (dx,dy) <- offsetsVecinos]

-- | Posiciones candidatas: todas las celdas vivas y sus vecinos.
frontera :: Universe -> Set Coord
frontera universo = S.union universo (S.fromList [ (x+dx, y+dy)
                                                | (x,y) <- S.toList universo
                                                , (dx,dy) <- offsetsVecinos])

-- | Calcula la siguiente generación según las reglas de Conway.
stepUniverse :: Universe -> Universe
stepUniverse universo = S.fromList $ filter viveSiguiente (S.toList (frontera universo))
  where
    viveSiguiente coord = case S.member coord universo of
        True  -> let n = contarVecinosVivos universo coord
                 in n == 2 || n == 3              -- Sobrevive si tiene 2 o 3 vecinos
        False -> contarVecinosVivos universo coord == 3 -- Nace si tiene exactamente 3 vecinos

-- | Calcula la siguiente generación en una cuadrícula finita (ancho x alto)
-- usando bordes toroidales: si una celda "se sale" por un borde, reaparece por el lado opuesto.
-- Esta versión es intencionalmente simple y fácil de leer (no la más optimizada).
stepUniverseToroidal :: Int -> Int -> Universe -> Universe
stepUniverseToroidal ancho alto universo =
  fromList
    [ (x,y)
    | x <- [0 .. ancho - 1]
    , y <- [0 .. alto  - 1]
    , let vivaActual = S.member (x,y) universo
          vecinos    = vecinosToroidales ancho alto (x,y)
          n          = length [ v | v <- vecinos, S.member v universo ]
    , if vivaActual
        then n == 2 || n == 3  -- Regla de supervivencia
        else n == 3            -- Regla de nacimiento
    ]

-- | Lista de vecinos con wrapping toroidal.
vecinosToroidales :: Int -> Int -> Coord -> [Coord]
vecinosToroidales ancho alto (x,y) =
  [ ((x+dx) `mod` ancho, (y+dy) `mod` alto) | (dx,dy) <- offsetsVecinos ]

-- | Bloque 2x2 estable en el origen.
block :: Universe
block = fromList [(0,0),(1,0),(0,1),(1,1)]

-- | Patrón clásico "planeador" (glider).
glider :: Universe
glider = fromList [(1,0),(2,1),(0,2),(1,2),(2,2)]

-- | Convierte un universo a una matriz (lista de filas) en los límites dados.
-- Los límites son inclusivos: (minX,maxX), (minY,maxY)
toMatrix :: (Int,Int) -> (Int,Int) -> Universe -> [[Bool]]
toMatrix (minX,maxX) (minY,maxY) universo =
    [ [ S.member (x,y) universo | x <- [minX..maxX] ] | y <- [minY..maxY] ]
