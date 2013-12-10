-- Common types & definitions for the Nine Man Morris game.  
-- Supplied for Haskell programming project
-- Students should not change this module
-- CISC 260, Winter 2011
module MorrisDefinitions where
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
type Board = ([Int],[Int])

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

getHumanPositions :: GameState -> [Int]
getHumanPositions (_,_,_,(hps,_)) = sort hps

getCompPositions :: GameState -> [Int]
getCompPositions (_,_,_,(_,cps)) = cps

getPlayerPositions :: Player -> Board -> [Int]
getPlayerPositions Human (hps,_) = hps
getPlayerPositions Computer (_,cps) = cps

getEmptyPositions :: GameState -> [Int]
getEmptyPositions (_,_,_,(hps,cps)) =
    [ p | p <- [1..24], not $ elem p hps, not $ elem p cps ]

getPossibleMovePositions :: GameState -> Int -> [Int]
getPossibleMovePositions state p = pps
  where
    eps = getEmptyPositions state
    pps = [ pp | pp <- eps, isAdjacent p pp ]

playerMills :: GameState -> [[Int]]
playerMills (p,_,_,b) = pms
  where 
    ps        = getPlayerPositions p b
    pms       = foldr (\m ms -> if hasMill m then m:ms else ms) [] mills
    hasMill m = all (\mp -> mp `elem` ps) m

canMove :: GameState -> Bool
canMove s = any (\p' -> not $ null $ getPossibleMovePositions s p') pps
  where p   = getPlayer s
        pps = getPlayerPositions p $ getBoard s

isPlacingPhase :: GameState -> Bool
isPlacingPhase (_,hc,cc,_) = hc > 0 || cc > 0

isMovePhase :: GameState -> Bool
isMovePhase s = not $ isPlacingPhase s 

-- a move in phase1 is a single integer: the position on which to put a piece
type Place = Int
-- a move in phase2 is a pair of integers: (position to move from, position to move to)
type Move = (Int,Int)

-- Given a game state, returns an equivalent game state where it's the other
-- player's turn
switchPlayer :: GameState -> GameState
switchPlayer (Human, hc, cc, b)    = (Computer, hc, cc, b)
switchPlayer (Computer, hc, cc, b) = (Human, hc, cc, b)

-- The set of all the possible mills, each given as a 3-element list of 
-- positions.  Each mill is given in one order only.
mills :: [[Int]]
mills = [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18],
         [19,20,21],[22,23,24],[1,10,22],[4,11,19],[7,12,16],[2,5,8],
         [17,20,23],[9,13,18],[6,14,21],[3,15,24]]
         
-- The set of all pairs of adjacent squares (meaning a piece can move 
-- directly from one to the other). Each adjacent pair is given as a 
-- 2-element list. Each pair is given in one order only. Observation: 
-- two adjacent squares must belong to a mill.
adjacentSpaces :: [[Int]]
adjacentSpaces = [[x,y]|[x,y,_] <- mills] ++ [[x,y]|[_,x,y] <- mills]

-- adjacent x y is true if x and y are adjacent
isAdjacent :: Int -> Int -> Bool
isAdjacent x y = elem [x,y] adjacentSpaces || elem [y,x] adjacentSpaces

