module Tile where

import Classes (Cyclic (..), Pretty (..))
import qualified Data.Set as Set

data Direction = Up | Right | Down | Left deriving (Ord, Enum, Bounded, Eq, Show)

instance Cyclic Direction

type Shape = Set.Set Direction

pipe :: Shape
pipe = Set.fromList [Tile.Up, Tile.Down]

elbow :: Shape
elbow = Set.fromList [Tile.Up, Tile.Right]

tee :: Shape
tee = Set.fromList [Tile.Left, Tile.Up, Tile.Right]

baseShapes :: [Shape]
baseShapes =
  [ pipe,
    elbow,
    tee
  ]

type Orientation = Direction -> Direction

north :: Orientation
north = id

east :: Orientation
east = next

south :: Orientation
south = next . next

west :: Orientation
west = prev

orientations :: [Orientation]
orientations = [north, east, south, west]

data Tile = Tile
  { shape :: Shape,
    number :: Int,
    start :: Maybe Int
  }
  deriving (Eq, Show)

fromShape :: Shape -> Tile
fromShape s = Tile {shape = s, number = 0, start = Nothing}

allOrientations :: Tile -> [Tile]
allOrientations tile = map (\s -> tile {shape = s}) . Set.toList . Set.fromList $ map (\f -> Set.map f $ shape tile) orientations

-- TODO: Add colors based on goal number
-- TODO: Maybe make the representation 3x3 so we can display additional information?
instance Pretty Tile where
  pretty Tile {shape = s}
    -- Pipes
    | s == Set.map north pipe = "│"
    | s == Set.map east pipe = "―"
    -- Elbows
    | s == Set.map north elbow = "└"
    | s == Set.map east elbow = "┌"
    | s == Set.map south elbow = "┐"
    | s == Set.map west elbow = "┘"
    -- Tees
    | s == Set.map north tee = "┴"
    | s == Set.map east tee = "├"
    | s == Set.map south tee = "┬"
    | s == Set.map west tee = "┤"
    -- "N" -- N stands for Non-standard
    | otherwise = "N"

-- Shorthand shapes and orientation to allow easier input of board

(#) :: Ord b => (a -> b) -> Set.Set a -> Set.Set b
(#) = Set.map

n :: Orientation
n = north

e :: Orientation
e = east

w :: Orientation
w = west

s :: Orientation
s = south

t :: Shape
t = tee

p :: Shape
p = pipe

l :: Shape
l = elbow