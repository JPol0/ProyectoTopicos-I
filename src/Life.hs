module Life
  ( -- Exportamos los tipos y funciones principales para el Juego de la Vida
    Cell (..),
    Universe,
    Coord,
    fromList,
    stepUniverse,
    -- Versión sencilla con bordes toroidales (envuelve por los extremos)
    stepUniverseToroidal,
    -- Stream perezoso de generaciones
    generaciones,
    generacionesToroidales,
    -- Patrones predefinidos
    glider,
    block,
    blinker,
    toad,
    beacon,
    lwss,
    gliderGun,
    -- Patrones de crecimiento ilimitado (Puffers / Rakes)
    fumador,
    -- Funciones de utilidad
    toMatrix,
    trasladar,
  )
where

import Data.Set (Set)
import Data.Set qualified as S

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
offsetsVecinos =
  [ (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1)
  ]

-- | Contar cuántos vecinos vivos tiene una celda.
contarVecinosVivos :: Universe -> Coord -> Int
contarVecinosVivos universo (x, y) = length $ filter (`S.member` universo) vecinos
  where
    vecinos = [(x + dx, y + dy) | (dx, dy) <- offsetsVecinos]

-- | Posiciones candidatas: todas las celdas vivas y sus vecinos.
frontera :: Universe -> Set Coord
frontera universo =
  S.union
    universo
    ( S.fromList
        [ (x + dx, y + dy)
          | (x, y) <- S.toList universo,
            (dx, dy) <- offsetsVecinos
        ]
    )

-- | Calcula la siguiente generación según las reglas de Conway.
stepUniverse :: Universe -> Universe
stepUniverse universo = S.fromList $ filter viveSiguiente (S.toList (frontera universo))
  where
    viveSiguiente coord = case S.member coord universo of
      True ->
        let n = contarVecinosVivos universo coord
         in n == 2 || n == 3 -- Sobrevive si tiene 2 o 3 vecinos
      False -> contarVecinosVivos universo coord == 3 -- Nace si tiene exactamente 3 vecinos

-- | Calcula la siguiente generación en una cuadrícula finita (ancho x alto)
-- usando bordes toroidales: si una celda "se sale" por un borde, reaparece por el lado opuesto.
-- Esta versión es intencionalmente simple y fácil de leer (no la más optimizada).
stepUniverseToroidal :: Int -> Int -> Universe -> Universe
stepUniverseToroidal ancho alto universo =
  fromList
    [ (x, y)
      | x <- [0 .. ancho - 1],
        y <- [0 .. alto - 1],
        let vivaActual = S.member (x, y) universo
            vecinos = vecinosToroidales ancho alto (x, y)
            n = length [v | v <- vecinos, S.member v universo],
        if vivaActual
          then n == 2 || n == 3 -- Regla de supervivencia
          else n == 3 -- Regla de nacimiento
    ]

-- | Lista de vecinos con wrapping toroidal.
vecinosToroidales :: Int -> Int -> Coord -> [Coord]
vecinosToroidales ancho alto (x, y) =
  [((x + dx) `mod` ancho, (y + dy) `mod` alto) | (dx, dy) <- offsetsVecinos]

-- | Bloque 2x2 estable en el origen.
block :: Universe
block = fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

-- | Patrón clásico "planeador" (glider).
glider :: Universe
glider = fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- | Convierte un universo a una matriz (lista de filas) en los límites dados.
-- Los límites son inclusivos: (minX,maxX), (minY,maxY)
toMatrix :: (Int, Int) -> (Int, Int) -> Universe -> [[Bool]]
toMatrix (minX, maxX) (minY, maxY) universo =
  [[S.member (x, y) universo | x <- [minX .. maxX]] | y <- [minY .. maxY]]

-- | Genera un stream perezoso (lista infinita) de generaciones a partir de un universo inicial.
generaciones :: Universe -> [Universe]
generaciones u = u : generaciones (stepUniverse u)

-- | Genera un stream perezoso de generaciones usando bordes toroidales
-- limitados a un ancho x alto. Cada paso aplica 'stepUniverseToroidal'.
generacionesToroidales :: Int -> Int -> Universe -> [Universe]
generacionesToroidales ancho alto u =
  u : generacionesToroidales ancho alto (stepUniverseToroidal ancho alto u)

-- | Desplaza un patrón por un vector (dx,dy)
trasladar :: (Int, Int) -> Universe -> Universe
trasladar (dx, dy) = S.mapMonotonic (\(x, y) -> (x + dx, y + dy))

-- ============================================
-- Patrones predefinidos
-- ============================================

-- | Parpadeador (Blinker) - Oscilador de período 2
blinker :: Universe
blinker = fromList [(1, 0), (1, 1), (1, 2)]

-- | Sapo (Toad) - Oscilador de período 2
toad :: Universe
toad = fromList [(1, 1), (2, 1), (3, 1), (0, 2), (1, 2), (2, 2)]

-- | Faro (Beacon) - Oscilador de período 2
beacon :: Universe
beacon = fromList [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

-- | Nave espacial ligera (Lightweight Spaceship) - Período 4, se mueve en diagonal
lwss :: Universe
lwss = fromList [(1, 0), (2, 0), (0, 1), (0, 2), (0, 3), (1, 3), (2, 3), (3, 1), (3, 3)]

-- | Cañón de planeadores (Gosper Glider Gun) - Patrón que genera planeadores
gliderGun :: Universe
gliderGun =
  fromList $
    [ (1, 5),
      (2, 5),
      (1, 6),
      (2, 6), -- Bloque izquierdo
      (11, 5),
      (11, 6),
      (11, 7), -- Caja con muesca
      (12, 4),
      (12, 8),
      (13, 3),
      (14, 3),
      (13, 9),
      (14, 9),
      (15, 6),
      (16, 4),
      (16, 8),
      (17, 5),
      (17, 6),
      (17, 7),
      (18, 6),
      (21, 3),
      (21, 4),
      (21, 5), -- Bloque derecho
      (22, 3),
      (22, 4),
      (22, 5),
      (23, 2),
      (23, 6),
      (25, 1),
      (25, 2),
      (25, 6),
      (25, 7),
      (35, 3),
      (35, 4),
      (36, 3),
      (36, 4) -- Bloque final
    ]

-- | Fumador (Puffer) – también conocido como "Switch engine".
-- Es un pequeño patrón que se mueve en diagonal y deja un rastro caótico a su paso.
-- Nota: Esta semilla puede necesitar espacio libre alrededor para evolucionar correctamente.
-- Orientación: diseñado para moverse hacia abajo-derecha (diagonal SE).
fumador :: Universe
fumador =
  fromList
    [ (0, 1),
      (1, 2),
      (2, 0),
      (2, 1),
      (3, 3),
      (4, 0),
      (4, 1),
      (4, 2),
      (5, 1)
    ]
