-- Common types & definitions for the Nine Man Morris game.  
-- Supplied for Haskell programming project
-- Students should not change this module
-- CISC 260, Winter 2011
module MorrisDefinitions where
import MorrisModel
import Data.List

newtype Player = Player Color
  deriving (Eq, Show)

getPlayerMark :: Player -> Char
getPlayerMark (Player Black) = 'X'
getPlayerMark (Player White) = 'O'

opponent :: Player -> Player
opponent (Player Black) = (Player White)
opponent (Player White) = (Player Black)

invert :: Color -> Color
invert Black = White
invert White = Black

-- The state of a game at any time is described by a tuple of four items:
-- 1. person whose turn it is (humanChar or computerChar)
-- 2. the number of pieces the human player has not yet placed on the board 
-- 3. the number of pieces the computer has not yet placed on the board
-- 4. the board
type GameState = (Player, Int, Int, Board)

type Move = (Pos,Pos)

-- extract parts of a state
  
getPlayer :: GameState -> Player
getPlayer (p,_,_,_) = p

getPlayerColor :: GameState -> Color
getPlayerColor (Player c,_,_,_) = c

getBoard :: GameState -> Board
getBoard (_,_,_,b) = b

getHumanCount :: GameState -> Int
getHumanCount (_,hc,_,_) = hc

getCompCount :: GameState -> Int
getCompCount (_,_,cc,_) = cc

getPositions :: GameState -> Maybe Color -> [Pos]
getPositions (_,_,_,b) c = getPositionsWithState b c

switchPlayer :: GameState -> GameState
switchPlayer (p, hc, cc, b) = (opponent p, hc, cc, b)

