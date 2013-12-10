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

data Player = Human | Computer
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

data Status = HumanWon | ComputerWon | Ongoing
  deriving (Eq)

getPlayerMark :: Player -> Char
getPlayerMark Human    = 'X'
getPlayerMark Computer = 'O'

opponent :: Player -> Player
opponent Human    = Computer
opponent Computer = Human

-- extract parts of a state
  
getPlayer :: GameState -> Player
getPlayer (p,_,_,_) = p

getBoard :: GameState -> Board
getBoard (_,_,_,b) = b

getHumanCount :: GameState -> Int
getHumanCount (_,hc,_,_) = hc

getCompCount :: GameState -> Int
getCompCount (_,_,cc,_) = cc

getHumanPositions :: GameState -> [Pos]
getHumanPositions (_,_,_,b) = getPositionsWithState b Black
-- getHumanPositions (_,_,_,(hps,_)) = sort hps

getCompPositions :: GameState -> [Pos]
getCompPositions (_,_,_,b) = getPositionsWithState b White
-- getCompPositions (_,_,_,(_,cps)) = cps

getPlayerPositions :: Player -> Board -> [Pos]
getPlayerPositions Human b = getPositionsWithState b Black 
getPlayerPositions Computer b = getPositionsWithState b White 

getEmptyPositions :: GameState -> [Pos]
getEmptyPositions (_,_,_,b) =
    filter (\p -> Nothing == getPositionState b p) positions

getPossibleMovePositions :: GameState -> Pos -> [Pos]
getPossibleMovePositions state p = pps
  where
    eps = getEmptyPositions state
    pps = [ pp | pp <- eps, isAdjacent p pp ]

playerMills :: GameState -> [[Pos]]
playerMills (p,_,_,b) = pms
  where 
    ps      = getPlayerPositions p b
    pms     = foldr (\m ms -> if hasMill m then m:ms else ms) [] mills
    hasMill = all (`elem` ps)

canMove :: GameState -> Bool
canMove s = any (not . null . getPossibleMovePositions s) pps
  where p   = getPlayer s
        pps = getPlayerPositions p $ getBoard s

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
switchPlayer (Human, hc, cc, b)    = (Computer, hc, cc, b)
switchPlayer (Computer, hc, cc, b) = (Human, hc, cc, b)

-- The set of all pairs of adjacent squares (meaning a piece can move 
-- directly from one to the other). Each adjacent pair is given as a 
-- 2-element list. Each pair is given in one order only. Observation: 
-- two adjacent squares must belong to a mill.
adjacentSpaces :: [[Pos]]
adjacentSpaces = [[x,y]|[x,y,_] <- mills] ++ [[x,y]|[_,x,y] <- mills]

-- adjacent x y is true if x and y are adjacent
isAdjacent :: Pos -> Pos -> Bool
isAdjacent x y = elem [x,y] adjacentSpaces || elem [y,x] adjacentSpaces

