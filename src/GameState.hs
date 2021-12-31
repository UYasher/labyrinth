{-# LANGUAGE TupleSections #-}

module GameState where

import Board
import Classes (Cyclic (next), Pretty (..))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Tile

data GameState = Game
  { board :: Board, -- The tiles currently in play
    numPlayers :: Int, -- The number of players, $n$
    playerTiles :: [Int], -- the $n-1$th element of the array has the tile the $n$th player is standing on
    extraTile :: Tile, -- The tile that will be inserted
    targets :: [[Int]], -- the $n-1$th element of the array has the
    insertRows :: Set.Set Int, -- the rows which we can insert the extra tile into
    insertCols :: Set.Set Int -- the columns which we can insert the extra tile into
  }
  deriving (Show)

-- Test this
-- The board + extraTile must have as many starts as there are players, otherwise this function returns Nothing
initializePlayerLocations :: GameState -> Maybe GameState
initializePlayerLocations g@Game {numPlayers = n} = updateGameState <$> initialPlayerTiles (board g) n
  where
    -- We use this helper function instead of an in-line call so that we can fmap
    updateGameState p = g {playerTiles = p}

defaultInsertRows :: Set.Set Int
defaultInsertRows = Set.fromList [1, 3, 5]

defaultInsertCols :: Set.Set Int
defaultInsertCols = defaultInsertRows

acGame :: Maybe GameState
acGame =
  initializePlayerLocations
    Game
      { board = acBoard,
        numPlayers = 4,
        playerTiles = [],
        extraTile = (fromShape $ Set.map north pipe) {number = 49},
        targets = [[0, 4 .. 23], [1, 5 .. 23], [2, 6 .. 23], [3, 7 .. 23]],
        insertRows = defaultInsertRows,
        insertCols = defaultInsertCols
      }

acGame2 :: Maybe GameState
acGame2 =
  initializePlayerLocations
    Game
      { board = acBoard2,
        numPlayers = 4,
        playerTiles = [],
        extraTile = (fromShape $ Set.map north pipe) {number = 49},
        targets = [[0, 4 .. 23], [1, 5 .. 23], [2, 6 .. 23], [3, 7 .. 23]],
        insertRows = defaultInsertRows,
        insertCols = defaultInsertCols
      }

{- Tile Insertion -}

-- Take in a board tile, row number, and boolean (representing front/back)
-- Returns the tile which was pushed off the board, as well as the board with the tile inserted into the front/back of the row
insertTileRow :: Board -> Tile -> Int -> Bool -> (Tile, Board)
insertTileRow board@(Board b) t r front = (removedTile, Board newBoard)
  where
    removedTile = getLocation (r, if front then width board - 1 else 0) board
    newBoard = take r b ++ [newRow] ++ drop (r + 1) b
    oldRow = b !! r
    newRow = if front then t : dropEnd 1 oldRow else drop 1 oldRow ++ [t]
    dropEnd n = reverse . drop n . reverse

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

transposeBoard :: Board -> Board
transposeBoard (Board b) = Board $ transpose b

-- Take in a board tile, col number, and boolean (representing front/back)
-- Return the board with the tile inserted into the front/back of the col, as well as the tile which was pushed off the board
insertTileCol :: Board -> Tile -> Int -> Bool -> (Tile, Board)
insertTileCol b t r front = transposeBoard <$> insertTileRow (transposeBoard b) t r front

insertTile :: Board -> Tile -> Bool -> Int -> Bool -> (Tile, Board)
insertTile b t isRow = (if isRow then insertTileRow else insertTileCol) b t

double :: [a] -> [a]
double [] = []
double (x : xs) = x : x : double xs

allInsertPositions :: Set.Set Int -> Set.Set Int -> [(Bool, Int, Bool)]
allInsertPositions rowSet colSet = toInsertPositions True rowSet ++ toInsertPositions False colSet
  where
    toInsertPositions :: Bool -> Set.Set Int -> [(Bool, Int, Bool)]
    toInsertPositions isRow rows = zip3 (repeat isRow) (double . Set.toList $ rows) (iterate not True)

allInsertMoves :: Tile -> Set.Set Int -> Set.Set Int -> [(Tile, Bool, Int, Bool)]
allInsertMoves t rowSet colSet = concatMap (\(a, b, c) -> map (,a,b,c) $ allOrientations t) (allInsertPositions rowSet colSet)

applyInsertMove :: Board -> (Tile, Bool, Int, Bool) -> (Tile, Board)
applyInsertMove b = uncurry4 $ insertTile b
  where
    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f (a, b, c, d) = f a b c d

-- TODO: Make it so that we can't insert a tile if it was just removed
applyInsertMoveGame :: GameState -> (Tile, Bool, Int, Bool) -> GameState
applyInsertMoveGame g@Game {board = b, extraTile = t} = updatePlayerTiles . toGame . applyInsertMove b
  where
    toGame = \(t', b') -> g {board = b', extraTile = t'}
    -- Check if any players were moved off the board and move those players to the newly inserted tile
    updatePlayerTiles g'@Game {extraTile = t'} = g' {playerTiles = (\p -> if p == number t' then number t else p) <$> playerTiles g'}

applyInsertMoveOrientation :: GameState -> (Orientation, Bool, Int, Bool) -> GameState
applyInsertMoveOrientation g@Game {extraTile = t} (o, a, b, c) = applyInsertMoveGame g (orient o t', a, b, c)
  where
    t' = Maybe.fromJust $ reorient t

allInsertions :: GameState -> [GameState]
allInsertions g@Game {extraTile = t} = map (applyInsertMoveGame g) $ allInsertMoves t (insertRows g) (insertCols g)

{- Finding Strongly Connected Components -}

inBounds :: Board -> Location -> Bool
inBounds b (x, y) = 0 <= x && x < height b && 0 <= y && y < width b

(+.) :: Location -> Location -> Location
(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- TODO: Make this function more elegant, it's currently a bit difficult to understand
adjacent :: Board -> Location -> [Location]
adjacent board loc = map snd $ filter (uncurry (connected board loc)) $ filter (inBounds board . snd) $ zip adjacentLocations $ map (+. loc) adjacentLocations
  where
    adjacentLocations = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    connected b l d l'
      | d == (0, 1) = Tile.Right `elem` shape (getLocation l b) && Tile.Left `elem` shape (getLocation l' b)
      | d == (0, -1) = Tile.Left `elem` shape (getLocation l b) && Tile.Right `elem` shape (getLocation l' b)
      | d == (1, 0) = Tile.Down `elem` shape (getLocation l b) && Tile.Up `elem` shape (getLocation l' b)
      | d == (-1, 0) = Tile.Up `elem` shape (getLocation l b) && Tile.Down `elem` shape (getLocation l' b)
      | otherwise = error $ "impossible direction: " ++ show d

--     Board -> Current  -> Visited          -> Reachable
dfs :: Board -> Location -> Set.Set Location -> Set.Set Location
dfs _ loc visited | loc `elem` visited = visited
dfs b loc visited = foldr (dfs b) (Set.insert loc visited) (adjacent b loc)

-- Give the state of the game, find all tiles the player can move to
findSCC :: Board -> Location -> Set.Set Int
findSCC b loc = Set.map toTileNumbers reachable
  where
    reachable = dfs b loc Set.empty
    toTileNumbers = number . flip getLocation b

-- Helper function to convert a board into a subset of the tiles
boardToSubset :: Board -> Set.Set Int -> Board
boardToSubset (Board b) s = Board $ map (map (\tile -> if number tile `elem` s then tile else fromShape Set.empty)) b

-- Given the game state, the player location, and the tile they want to reach, return all methods of reaching that tile
-- The methods are tuples of the form (isRow, row/col, isFront, moveLocation)
-- TODO: Test this method. It generally seems to work correctly, but I'm not sure it does
-- It also doesn't take into account movement of the player due to insertions.
-- TODO: Clean up this function
nextMoveCaptures :: GameState -> Location -> Int -> [(Tile, Bool, Int, Bool, Location)]
nextMoveCaptures g@Game {board = b, extraTile = t} loc n = map (\(g, (a, b, c, d)) -> (a, b, c, d, Maybe.fromJust $ findLocation (board g) (\t' -> number t' == n))) . filter (Set.member n . flip findSCC loc . board . fst) $ zip (allInsertions g) (allInsertMoves t (insertRows g) (insertCols g))

-- TODOs:
-- Add random number generation
-- Add readme
-- Extend to captures at larger depths
-- Extend to random generation of games
-- Review and make use of the state monad