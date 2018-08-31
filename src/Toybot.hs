{-# LANGUAGE OverloadedStrings #-}

module Toybot ( Table(..)
              , run
              , parseCommand
              , defaultBot
              ) where

import Protolude 
import Data.List ((!!))
import qualified Data.Text.ICU as R
import Data.Text (breakOn, lines)
import qualified Data.Text.Read as T (decimal)

-- The x,y position of the bot
type Position = (Integer, Integer)

-- The direction the bot is facing
data Direction
  = North
  | South
  | East
  | West
  deriving Show

-- Table on which the bot is placed
data Table = Table
  { width :: Integer
  , breadth :: Integer
  }

-- Bot commands
data Command
  = Place Position Direction
  | MoveForward
  | TurnLeft
  | TurnRight
  | Report
  deriving Show

-- The bot states
data Bot
  = NotPlaced
  | Bot Position Direction
  deriving Show

defaultBot :: Bot
defaultBot = NotPlaced

turnLeft :: Direction -> Direction
turnLeft dir = case dir of
  North -> West
  South -> East
  East  -> North
  West  -> South

turnRight :: Direction -> Direction
turnRight dir = case dir of
  North -> East
  South -> West
  East  -> South
  West  -> North

toDirection :: Text -> Maybe Direction
toDirection s =
  case s of
    "NORTH" -> Just North
    "SOUTH" -> Just South
    "EAST"  -> Just East
    "WEST"  -> Just West
    otherwise -> Nothing

parseCommand :: Text -> Maybe Command
parseCommand s =
  let (cmd, rest) = breakOn " " s
  in case cmd of
    "PLACE" -> parsePlace rest
    "MOVE" -> Just MoveForward
    "LEFT" -> Just TurnLeft
    "RIGHT" -> Just TurnRight
    "REPORT" -> Just Report
    otherwise -> Nothing
  where
    parsePlace :: Text -> Maybe Command
    parsePlace s = do
      match <- R.find (R.regex [] "^\\s(\\d),(\\d),(NORTH|SOUTH|EAST|WEST)$") s
      x <- toDecimal =<< R.group 1 match
      y <- toDecimal =<< R.group 2 match
      dir <- toDirection =<< R.group 3 match
      pure $ Place (x, y) dir

    toDecimal :: Text -> Maybe Integer
    toDecimal = either (const Nothing) (Just . fst) . T.decimal
  

exec :: Table -> Bot -> Command -> IO Bot
exec table _ (Place pos dir) = pure $ Bot pos dir
exec table bot@NotPlaced command = pure NotPlaced -- if not yet placed, then ignore all commands
exec table bot@(Bot position dir) command =
  case command of
    MoveForward ->
      if nextPosition `onTable` table
      then pure $ Bot nextPosition dir
      else pure bot
    TurnLeft -> pure $ Bot position (turnLeft dir)
    TurnRight -> pure $ Bot position (turnRight dir)
    Report -> (putLText $ "Output: " <> show bot) *> pure bot
  where
    nextPosition :: Position
    nextPosition = case dir of
      North -> (x, (y + 1))
      South -> (x, (y - 1))
      East  -> ((x + 1), y)
      West  -> ((x - 1), y)
    (x, y) = position

run :: Bot -> Table -> [Command] -> IO Bot
run bot table = foldlM (exec table) bot

onTable :: Position -> Table -> Bool
onTable (x, y) (Table w h) =
  x >= 0 && x < w && y >= 0 && y < h
