-- The "brains" of the Nine Man Morris game.
-- This skeleton version contains "dummy" versions of
-- all of the functions that the main module expects
-- to use.  Students must fill in bodies for all the
-- functions required for the play level they're implementing.
-- You will need plenty of helper functions as well.
-- CISC 260, winter 2011
-- M. Lamb
module MorrisBrains where
import MorrisModel
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
millCount (Player c) b = length $ getMills b c
  -- where 
  --   pms     = foldr (\m mc -> if hasMill m then mc + 1 else mc) 0 mills
  --   hasMill = all (`elem` (getPositionsWithState b (Just c)))

twoOutOfThreeCount :: Player -> Board -> Int
twoOutOfThreeCount (Player c) b = pms
  where 
    pps      = getPositionsWithState b (Just c)
    ops      = getPositionsWithState b $ (Just $ invert c)
    pms      = foldr (\m mc -> if hasTwo m then mc + 1 else mc) 0 mills
    mps      = foldr (\p' ps' -> if p' `elem` pps then ps' + 1 else ps') 0
    hasTwo m = (mps m == 2) && not (any (`elem` ops) m)

playerScore :: GameState -> Int
playerScore (p,hc,cc,b) = mss + omss + tots + otots
  where 
    mss   = 10 * millCount p b
    omss  = (-9) * millCount (opponent p) b
    tots  = 4 * twoOutOfThreeCount p b
    otots = (-5) * twoOutOfThreeCount (opponent p) b

movableList :: GameState -> [Pos]
movableList (Player c,hc,cc,b) = movable $ getPositionsWithState b (Just c)
  where
    pmps    = getPossibleMovePositions (Player c,hc,cc,b)
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
status (p,hc,cc,b) | hasLost Black hps = WhiteWon 
                   | hasLost White cps = BlackWon
                   | otherwise         = Ongoing 
  where 
    hps                 = getPositionsWithState b (Just Black)
    cps                 = getPositionsWithState b (Just White)
    looseCondition p ps = length ps < 3 || not (canMove (p,hc,cc,b))
    hasLost c ps        = isMovePhase (p,hc,cc,b) && looseCondition p ps

-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 1 move to make (adding a piece to the board).
-- Return value: the position where the new piece should go.
-- Assumes the game is not over, so there will be a legal move.
bestPlacement :: GameState -> Pos
bestPlacement (p,hc,cc,b) = best $ getPositionsWithState b Nothing
  where
    best         = foldr1 (\p' bp -> if better p' bp then p' else bp)
    better np op = playerScore (p,hc,cc,nb np) > playerScore (p,hc,cc,nb op)
    nb p'        = updateBoard b (Just White ) p'

-- A new game state produced by placing a piece on the board
-- Parameters: initial state and position where piece will go.  The piece 
-- will be  taken from the player whose turn it is.  Assumes the player 
-- has at least one piece remaining and the position is free.
-- Returns: new game state.  The player does not change.
addPiece :: GameState -> Pos -> GameState
addPiece (Player pc,bpl,wpl,b) p = ns pc
  where ns c | c == Black = (Player c,bpl-1,wpl,updateBoard b (Just c) p)
             | c == White = (Player c,bpl,wpl-1,updateBoard b (Just c) p)
      
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
removePiece :: GameState -> Pos -> GameState
removePiece (p,hc,cc,b) s = (p,hc,cc,nb)
  where nb = updateBoard b Nothing s

-- Given a game state after a player has made a mill, returns a list of
-- the opponent pieces it would be legal to capture.  These are all the
-- pieces which are not part of a mill.  Exception: if there are no 
-- pieces outside a mill, then any piece may be captured.  
captureList :: GameState -> [Pos]
captureList (Player c,hc,cc,b) | not $ null cps = sort cps
                               | otherwise      = sort pps
  where
    pps       = getPositionsWithState b $ (Just $ invert c)
    cps       = foldr (\m ps -> if hasMill m then deleteAll ps m else ps) pps mills
    deleteAll = foldr delete
    hasMill   = all (`elem` pps)
    
-- Picks the best capture for the computer to make after a mill 
-- Parameters: starting state and list of possible captures (assume 
-- non-empty)
bestCapture :: GameState -> [Pos] -> Pos
bestCapture (p,_,_,b) ps = head ps -- TODO
 
---------------------------------------------------------------------
-- FUNCTION NEEDED FOR LEVEL 3 ONLY
-- (Levels 1&2 will not use this function, so the dummy
--  value won't affect the game)
---------------------------------------------------------------------

-- This function is like bestPlacement, but for phase 2 of the game
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
bestMove :: GameState -> Move
bestMove (Player c,hc,cc,b) = (from,too) -- dummy
  where
    pmps p = getPossibleMovePositions (Player c,hc,cc,b) p
    from   = head [ p' | p' <- getPositionsWithState b (Just c), not $ null $ pmps p' ]
    too    = head $ pmps from
   
