module Lib
  ( program,
    game,
  )
where

import Board (getLocation)
import Classes (pretty)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GameState
import Tile (Orientation, east, north, number, shape, south, west)

-- TODO: Allow the user to undo steps of the simulation
simulate :: GameState -> IO GameState
simulate g = applyInsertMoveOrientation g <$> getInstruction g

-- TODO: Prompt user again if they give invalid input
getInstruction :: GameState -> IO (Orientation, Bool, Int, Bool)
getInstruction g = do
  putStrLn $ "How do you want to insert " ++ (pretty . extraTile $ g) ++ "?: "
  o <- getOrientation g
  putStrLn "And where do you want to insert it?: "
  (b1, i, b2) <- read <$> getLine
  pure (o, b1, i, b2)

orientations :: Map.Map String Orientation
orientations = Map.fromList [("n", north), ("e", east), ("s", south), ("w", west)]

getOrientation :: GameState -> IO Orientation
getOrientation g = do
  o <- getLine
  if o == "h"
    then do
      help g
      getOrientation g
    else do
      if Map.member o orientations
        then pure $ orientations Map.! o
        else getOrientation g

-- Stuffing help in getOrientation requires us to pass the GameState through many functions
-- TODO: change the invocation of help to prevent passing GameState
-- TODO: change the name of this function to something more informative
help :: GameState -> IO ()
help g = do
  print "Okay, let me help you..."
  print "What current coordinates on the board are you at?:"
  loc <- read <$> getLine
  print "What are the current coordinates on the board that you want to move to?:"
  n <- number . flip getLocation (board g) . read <$> getLine
  print "Here are all the ways of moving between those positions:"
  print $ nextMoveCaptures g loc n -- TODO: Print out the moves on individual lines and in the form (Orientation, Bool, Int, Bool)
  pure ()

game :: GameState
game = Maybe.fromJust acGame2

program :: GameState -> IO ()
program g = do
  putStrLn . pretty $ board g
  g' <- simulate g
  program g'
  pure ()