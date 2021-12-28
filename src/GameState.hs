module GameState where

import Board
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

acGame :: Maybe GameState
acGame =
  initializePlayerLocations
    Game
      { board = acBoard,
        numPlayers = 4,
        playerLocations = [],
        extraTile = fromShape $ Set.map north pipe,
        targets = [[0, 4 .. 23], [1, 5 .. 23], [2, 6 .. 23], [3, 7 .. 23]],
        insertRows = defaultInsertRows,
        insertCols = defaultInsertCols
      }

-- TODOs:
-- Remove target and add number
-- Write dfs to find SCCs (and therefore if a game state allows for a one-turn capture)
-- Write a mechanism which gives the results of all the potential insertions
-- Write a method which uses insertion dfs pairs to find optimal one-turn captures
-- Write an easy input format which lets me create boards as I encounter them in real-life
-- Add random number generation
-- Add readme
-- Extend to captures at larger depths
-- Extend to random generation of games
-- Review the state monad, seems useful