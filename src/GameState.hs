module GameState where

import Board
import Classes (Pretty (..))
import qualified Data.Set as Set
import Tile

-- The board + extraTile must have as many starts as there are players
data GameState = Game
  { board :: Board, -- The tiles currently in play
    numPlayers :: Int, -- The number of players, $n$
    playerLocations :: [Location], -- the $n-1$th element of the array has the location of the $n$th player
    extraTile :: Tile, -- The tile that will be inserted
    targets :: [[Int]], -- the $n-1$th element of the array has the
    insertRows :: Set.Set Int, -- the rows which we can insert the extra tile into
    insertCols :: Set.Set Int -- the columns which we can insert the extra tile into
  }
  deriving (Show)

-- Test this
initializePlayerLocations :: GameState -> Maybe GameState
initializePlayerLocations g@Game {numPlayers = n} = updateGameState <$> initialPlayerLocations (board g) n
  where
    -- We use this helper function instead of an in-line call so that we can fmap
    updateGameState p = g {playerLocations = p}

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
        playerLocations = [], -- TODO: instead of storing locations, players should be associated with tile numbers
        extraTile = (fromShape $ Set.map north pipe) {number = 49},
        targets = [[0, 4 .. 23], [1, 5 .. 23], [2, 6 .. 23], [3, 7 .. 23]],
        insertRows = defaultInsertRows,
        insertCols = defaultInsertCols
      }

-- Take in a board tile, row number, and boolean (representing front/back)
-- Returns the tile which was pushed off the board, as well as the board with the tile inserted into the front/back of the row
insertTileRow :: Board -> Tile -> Int -> Bool -> (Tile, Board)
insertTileRow board@(Board b) t r front = (removedTile, Board newBoard)
  where
    removedTile = getLocation (if front then width board - 1 else 0, r) board
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

allInsertions :: GameState -> [GameState]
allInsertions g = undefined

-- Given the game state, the player, and the tile they want to reach, return all methods of reaching that tile
-- The methods are tuples of the form (isRow, row/col, isFront, moveLocation)
nextMoveCaptures :: GameState -> Int -> [(Bool, Bool, Int, Location)]
nextMoveCaptures = undefined

-- TODOs:
-- Write an easy input format which lets me create boards as I encounter them in real-life
--  This probably looks like (Bool, Bool, Location)
-- Add random number generation
-- Add readme
-- Extend to captures at larger depths
-- Extend to random generation of games
-- Review and make use of the state monad