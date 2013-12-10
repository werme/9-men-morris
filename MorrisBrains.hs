-- The "brains" of the Nine Man Morris game.
-- This skeleton version contains "dummy" versions of
-- all of the functions that the main module expects
-- to use.  Students must fill in bodies for all the
-- functions required for the play level they're implementing.
-- You will need plenty of helper functions as well.
-- CISC 260, winter 2011
-- M. Lamb
module MorrisBrains where
import MorrisDefinitions
import Data.List

-- this constant defines the level at which we are playing 
-- (must be 1, 2 or 3)
--playLevel :: Int
--playLevel = 3 -- implement level 1 first, then progress to 2 & 3

---------------------------------------------------------------------
-- FUNCTIONS NEEDED FOR ALL THREE LEVELS
---------------------------------------------------------------------

-- Parameters: a board and a player character.  Assume the player is 
-- either humanChar or computerChar.
-- Return value: the number of mills the player has
-- (This function is technically not necessary until level 2, but
-- it's a very useful helper for level 1)
millCount :: Player -> Board -> Int
millCount p b = pms
  where 
    ps      = getPlayerPositions p b
    pms     = foldr (\m mc -> if hasMill m then mc + 1 else mc) 0 mills
    hasMill = all (`elem` ps)

twoOutOfThreeCount :: Player -> Board -> Int
twoOutOfThreeCount p b = pms
  where 
    ps       = getPlayerPositions p b
    ops      = getPlayerPositions (opponent p) b
    pms      = foldr (\m mc -> if hasTwo m then mc + 1 else mc) 0 mills
    mps      = foldr (\p' ps' -> if p' `elem` ps then ps' + 1 else ps') 0
    hasTwo m = (mps m == 2) && not (any (`elem` ops) m)

playerScore :: GameState -> Int
playerScore (p,hc,cc,b) = mss + omss + tots + otots
  where 
    mss   = 10 * millCount p b
    omss  = (-9) * millCount (opponent p) b
    tots  = 4 * twoOutOfThreeCount p b
    otots = (-5) * twoOutOfThreeCount (opponent p) b

movableList :: GameState -> [Place]
movableList (p,hc,cc,b) = movable $ getPlayerPositions p b
  where
    pmps    = getPossibleMovePositions (p,hc,cc,b)
    movable = foldr (\p' mps -> pmps p' ++ mps) []

possibleMovesList :: GameState -> [Move]
possibleMovesList s = undefined

-- Tests if the game is over.  Returns one of four characters:
--   humanChar: game is over and human player has won
--   computerChar: game is over and computer has won
--   'D': game is over and game is a draw (only possible in levels 1&2)
--   'X': game is not over
-- At levels 1 and 2, the game is over when neither player has a piece
-- left to play.  The winner is the player with the most mills.  It's a
-- draw if the two players have the same number of mills.
-- At level 3, the game is over when one player has less than 3 pieces
-- on the board or can't move any of their pieces.  That player loses.
-- There will be no draws at level 3.
status :: GameState -> Status
status (p,hc,cc,(hps,cps)) | hasLost Human hps    = ComputerWon 
                           | hasLost Computer cps = HumanWon
                           | otherwise            = Ongoing 
  where 
    s                   = (p,hc,cc,(hps,cps))
    looseCondition p ps = length ps < 3 || not (canMove s)
    hasLost p ps        = isMovePhase s && looseCondition p ps
    
-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 1 move to make (adding a piece to the board).
-- Return value: the position where the new piece should go.
-- Assumes the game is not over, so there will be a legal move.
bestMove1 :: GameState -> Place
bestMove1 (p,hc,cc,(hps,cps)) = best $ getEmptyPositions (p,hc,cc,(hps,cps))
  where
    better np op = playerScore (p,hc,cc,(hps,np:cps)) > playerScore (p,hc,cc,(hps,op:cps)) 
    best         = foldr1 (\p' bp -> if better p' bp then p' else bp)

-- A new game state produced by placing a piece on the board
-- Parameters: initial state and position where piece will go.  The piece 
-- will be  taken from the player whose turn it is.  Assumes the player 
-- has at least one piece remaining and the position is free.
-- Returns: new game state.  The player does not change.
addPiece :: GameState -> Place -> GameState
addPiece (Human, hc, cc, (hss,css)) ns    = (Human, hc-1, cc, (ns:hss,css))
addPiece (Computer, hc, cc, (hss,css)) ns = (Computer, hc, cc-1, (hss,ns:css))
      
---------------------------------------------------------------------
-- FUNCTIONS NEEDED FOR LEVELS 2&3 ONLY
-- (Level 1 will not use these functions, so the dummy
--  values won't affect the game)
---------------------------------------------------------------------

-- a new game state produced by removing a piece from the board
-- Parameters: initial state, and position from which to remove the piece
-- Assumes the position is currently occupied.  The player does not 
-- change.  This is not used by the main module until level 3, but
-- it's a good helper function for level 2 when capturing pieces.
-- Returns: new game state
removePiece :: GameState -> Place -> GameState
removePiece (p,hc,cc,(hss,css)) s = (p,hc,cc,(nhss,ncss))
  where nhss = delete s hss
        ncss = delete s css

-- Given a game state after a player has made a mill, returns a list of
-- the opponent pieces it would be legal to capture.  These are all the
-- pieces which are not part of a mill.  Exception: if there are no 
-- pieces outside a mill, then any piece may be captured.  
captureList :: GameState -> [Place]
captureList (p,_,_,b) | not $ null cps = sort cps
                      | otherwise      = sort pps
  where
    pps       = getPlayerPositions (opponent p) b
    cps       = foldr (\m ps -> if hasMill m then deleteAll ps m else ps) pps mills
    deleteAll = foldr delete
    hasMill   = all (`elem` pps)
    
-- Picks the best capture for the computer to make after a mill 
-- Parameters: starting state and list of possible captures (assume 
-- non-empty)
bestCapture :: GameState -> [Place] -> Place
bestCapture (p,_,_,b) ps = head ps -- TODO

---------------------------------------------------------------------
-- FUNCTION NEEDED FOR LEVEL 3 ONLY
-- (Levels 1&2 will not use this function, so the dummy
--  value won't affect the game)
---------------------------------------------------------------------

-- This function is like bestMove1, but for phase 2 of the game
-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 2 move to make (moving a piece to an adjacent position).
-- Return value: the best move
-- Assumes the game is not over, so there will be a legal move.
-- Strategy:
--    A. If there's a move that gets you a mill (even if you have to 
--       break up a mill to do it), that's the best move
--    B. Move a piece away from a mill, hoping to move it back on your 
--       next move
--    C. Pick the move that gives you the state with the best score, as 
--       in phase 1.
bestMove2 :: GameState -> Move
bestMove2 (p,hc,cc,b) = (from,too) -- dummy
  where
    pmps = getPossibleMovePositions
    from = head [ p'' | p'' <- getPlayerPositions p b, not $ null $ pmps (p,hc,cc,b) p'' ]
    too  = head $ pmps (p,hc,cc,b) from
   
