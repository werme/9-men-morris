-- Common types & definitions for the Nine Man Morris game.  
-- Supplied for Haskell programming project
-- Students should not change this module
-- CISC 260, Winter 2011
module MorrisDefinitions where
import MorrisModel
import Data.List

-- A game is played against a human player (who goes first) and the computer.
-- I'm using the initials 'H' (for the human) and 'C' (for the computer).
-- You may change them to two other characters, except don't use 'X' or 'D' or digits, as 
-- these are used for other purposes in the code and the output.

newtype Player = Player Color
  deriving (Eq, Show)

-- A board is described by a tuple of two lists: the human player's squares and the 
-- computer's squares.  Both lists should contain integers in [1..24] with no 
-- duplicates and no integers in both lists.  (Functions don't need to
-- check this assumption.)

--type Board = ([Int],[Int])

-- The state of a game at any time is described by a tuple of four items:
-- 1. person whose turn it is (humanChar or computerChar)
-- 2. the number of pieces the human player has not yet placed on the board 
-- 3. the number of pieces the computer has not yet placed on the board
-- 4. the board
type GameState = (Player, Int, Int, Board)

data Status = BlackWon | WhiteWon | Ongoing
  deriving (Eq)

getPlayerMark :: Player -> Char
getPlayerMark (Player Black) = 'X'
getPlayerMark (Player White) = 'O'

opponent :: Player -> Player
opponent (Player Black) = (Player White)
opponent (Player White) = (Player Black)

invert :: Color -> Color
invert Black = White
invert White = Black

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

getPossibleMovePositions :: GameState -> Pos -> [Pos]
getPossibleMovePositions state p = pps
  where
    eps = getPositions state Nothing
    pps = [ pp | pp <- eps, isAdjacent p pp ]

playerMills :: GameState -> [[Pos]]
playerMills (Player c,_,_,b) = getMills b c

canMove :: GameState -> Bool
canMove s = any (not . null . getPossibleMovePositions s) pps
  where pps = getPositionsWithState (getBoard s) (Just (getPlayerColor s))

isPlacingPhase :: GameState -> Bool
isPlacingPhase (_,hc,cc,_) = hc > 0 || cc > 0

isMovePhase :: GameState -> Bool
isMovePhase s = not $ isPlacingPhase s 

-- a move in phase1 is a single integer: the position on which to put a piece
-- a move in phase2 is a pair of integers: (position to move from, position to move to)
type Move = (Pos,Pos)

-- Given a game state, returns an equivalent game state where it's the other
-- player's turn
switchPlayer :: GameState -> GameState
switchPlayer (p, hc, cc, b) = (opponent p, hc, cc, b)


