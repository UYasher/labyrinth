module Board where

import Classes (Cyclic (..), Pretty (..))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Tile

-- `Board !! i !! j` takes the i-th row and j-th column
-- TODO: Re-implement the board using arrays rather than lists
-- TODO: Make the board traversable and a monad
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

initialPlayerTiles :: Board -> Int -> Maybe [Int]
initialPlayerTiles board numPlayers = do
  locs <- initialPlayerLocations board numPlayers
  pure $ map (number . flip getLocation board) locs

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

setUpBoard :: [[Shape]] -> Board
setUpBoard = addDefaultStarts . numberTiles . fromShapes

acBoard :: Board
acBoard =
  setUpBoard
    [ [Set.map east elbow, Set.map south tee, Set.map south tee, Set.map north elbow, Set.map south tee, Set.map east elbow, Set.map south elbow],
      [Set.map south elbow, Set.map east pipe, Set.map east pipe, Set.map east pipe, Set.map south elbow, Set.map west elbow, Set.map east elbow],
      [Set.map east tee, Set.map north elbow, Set.map east tee, Set.map north elbow, Set.map south tee, Set.map east elbow, Set.map west tee],
      [Set.map east pipe, Set.map north elbow, Set.map east tee, Set.map north pipe, Set.map south elbow, Set.map west elbow, Set.map north pipe],
      [Set.map east tee, Set.map north elbow, Set.map north tee, Set.map east pipe, Set.map west tee, Set.map south elbow, Set.map west tee],
      [Set.map east tee, Set.map south tee, Set.map north pipe, Set.map east pipe, Set.map east tee, Set.map north pipe, Set.map north elbow],
      [Set.map north elbow, Set.map south tee, Set.map north tee, Set.map west elbow, Set.map north tee, Set.map east pipe, Set.map west elbow]
    ]

-- TODO: make a shortcut to add the fixed tiles
acBoard2 :: Board
acBoard2 =
  setUpBoard
    [ [e # l, w # l, s # t, e # l, s # t, s # l, s # l],
      [n # p, e # p, e # t, s # l, e # l, n # p, n # p],
      [e # t, n # p, e # t, n # l, s # t, s # t, w # t],
      [n # l, w # t, s # l, s # l, n # l, n # p, s # l],
      [e # t, s # t, n # t, n # l, w # t, e # p, w # t],
      [w # l, s # l, e # l, e # p, e # p, n # p, w # t],
      [n # l, e # l, n # t, w # t, n # t, n # p, w # l]
    ]

acBoard3 :: Board
acBoard3 =
  setUpBoard
    [ [e # l, s # l, s # t, e # p, s # t, w # l, s # l],
      [n # t, e # l, n # p, e # l, n # t, e # l, n # p],
      [e # t, s # l, e # t, n # l, s # t, n # p, w # t],
      [e # p, e # l, s # t, e # l, n # p, s # l, e # p],
      [e # t, e # l, n # t, n # p, w # t, s # l, w # t],
      [n # p, e # l, w # t, n # p, e # l, e # t, n # p],
      [n # l, n # l, n # t, e # p, n # t, e # p, w # l]
    ]