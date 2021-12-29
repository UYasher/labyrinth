module Board where

import Classes (Cyclic (..), Pretty (..))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Tile

-- `Board !! i !! j` takes the i-th row and j-th column
-- I might want to make this a monad and traversable
newtype Board = Board [[Tile]] deriving (Show)

instance Pretty Board where
  pretty (Board b) = concatMap concatRow b
    where
      concatRow r = concatMap Classes.pretty r ++ "\n"

fromShapes :: [[Shape]] -> Board
fromShapes = Board . map (map fromShape)

sampleBoard :: Board
sampleBoard =
  fromShapes
    [ [Set.map east elbow, Set.map south elbow],
      [Set.map north elbow, Set.map west elbow]
    ]

type Location = (Int, Int)

-- Finds the location of the first tile in the board that matches a predicate
findLocation :: Board -> (Tile -> Bool) -> Maybe Location
findLocation (Board b) p = do
  x <- List.findIndex Maybe.isJust xs
  y <- xs !! x
  pure (x, y)
  where
    xs = map (List.findIndex p) b

initialPlayerLocations :: Board -> Int -> Maybe [Location]
initialPlayerLocations board numPlayers = sequence $ startLocation <$> players
  where
    players = [0 .. numPlayers - 1]
    startLocation player = findLocation board (\t -> start t == Just player)

getLocation :: Location -> Board -> Tile
getLocation (x, y) (Board b) = b !! x !! y

updateLocation :: Location -> Board -> Tile -> Board
updateLocation (x, y) (Board b) t = Board $ front ++ [newMiddle] ++ back
  where
    front = take x b
    back = drop (x + 1) b
    middle = b !! x
    newMiddle = take y middle ++ [t] ++ drop (y + 1) middle

height :: Board -> Int
height (Board b) = length b

width :: Board -> Int
width (Board b) = length $ head b

isEdge :: Board -> Location -> Bool
isEdge b (x, y) = isVerticalEdge || isHorizontalEdge
  where
    isVerticalEdge = x == 0 || x == height b
    isHorizontalEdge = y == 0 || y == width b

addDefaultStarts :: Board -> Board
addDefaultStarts b = foldr addStart b $ zip startLocations players
  where
    startLocations = [(0, 0), (0, width b - 1), (height b - 1, 0), (height b - 1, width b - 1)]
    players = [0 .. 3]
    addStart (loc, p) b' = updateLocation loc b' ((getLocation loc b') {start = Just p})

numberTiles :: Board -> Board
numberTiles board@(Board b) = Board $ chunksOf (width board) (numberList 0 $ concat b)
  where
    numberList n (x : xs) = x {number = n} : numberList (n + 1) xs
    numberList _ [] = []
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

acBoard :: Board
acBoard =
  addDefaultStarts . numberTiles . fromShapes $
    [ [Set.map east elbow, Set.map south tee, Set.map south tee, Set.map north elbow, Set.map south tee, Set.map east elbow, Set.map south elbow],
      [Set.map south elbow, Set.map east pipe, Set.map east pipe, Set.map east pipe, Set.map south elbow, Set.map west elbow, Set.map east elbow],
      [Set.map east tee, Set.map north elbow, Set.map east tee, Set.map north elbow, Set.map south tee, Set.map east elbow, Set.map west tee],
      [Set.map east pipe, Set.map north elbow, Set.map east tee, Set.map north pipe, Set.map south elbow, Set.map west elbow, Set.map north pipe],
      [Set.map east tee, Set.map north elbow, Set.map north tee, Set.map east pipe, Set.map west tee, Set.map south elbow, Set.map west tee],
      [Set.map east tee, Set.map south tee, Set.map north pipe, Set.map east pipe, Set.map east tee, Set.map north pipe, Set.map north elbow],
      [Set.map north elbow, Set.map south tee, Set.map north tee, Set.map west elbow, Set.map north tee, Set.map east pipe, Set.map west elbow]
    ]