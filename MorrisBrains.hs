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

-- Returns the number of mills on the board for a given player
millCount :: Board -> Player -> Int
millCount b (Player c) = length $ getMills b c

-- Returns the number of twoOutOfThree:s on the board for the given player 
twoOutOfThreeCount :: Board -> Player -> Int
twoOutOfThreeCount b (Player c) = pms
  where 
    pps      = getPositionsWithState b (Just c)
    ops      = getPositionsWithState b (Just $ invert c)
    pms      = foldr (\m mc -> if hasTwo m then mc + 1 else mc) 0 mills
    mps      = foldr (\p' ps' -> if p' `elem` pps then ps' + 1 else ps') 0
    hasTwo m = (mps m == 2) && not (any (`elem` ops) m)

-- Returns the total board score of the current player. See code for details
playerScore :: GameState -> Int
playerScore (p,hc,cc,b) = mss + omss + tots + otots
  where 
    mss   = 10 * millCount b p
    omss  = (-9) * (millCount b $ opponent p)
    tots  = 4 * twoOutOfThreeCount b p
    otots = (-5) * (twoOutOfThreeCount b $ opponent p)

-- Returns true if the given move creates a mill for the current player
addsMill :: GameState -> Move -> Bool
addsMill (p,bpl,wpl,b) m = millCount b p > millCount nb p 
  where nb = getBoard $ move (p,bpl,wpl,b) m

-- Makes the given move for the current player
move :: GameState -> Move -> GameState
move s (f,t) = addPiece (removePiece s f) t

-- Returns the current state of the game
status :: GameState -> Status
status (p,hc,cc,b) | hasLost Black hps = WhiteWon 
                   | hasLost White cps = BlackWon
                   | otherwise         = Ongoing 
  where 
    hps                 = getPositionsWithState b (Just Black)
    cps                 = getPositionsWithState b (Just White)
    looseCondition p ps = length ps < 3 || not (canMove (p,hc,cc,b))
    hasLost c ps        = isMovePhase (p,hc,cc,b) && looseCondition p ps

-- Adds a piece at the given position for the current player
addPiece :: GameState -> Pos -> GameState
addPiece (Player pc,bpl,wpl,b) p = ns pc
  where ns c | c == Black = (Player c,bpl-1,wpl,updateBoard b (Just c) p)
             | c == White = (Player c,bpl,wpl-1,updateBoard b (Just c) p)

-- Removes a piece from the board at the given position
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
    pps       = getPositionsWithState b (Just $ invert c)
    cps       = foldr (\m ps -> if hasMill m then deleteAll ps m else ps) pps mills
    deleteAll = foldr delete
    hasMill   = all (`elem` pps)

-- Returns a list of all positions with movable pieces for the current player
movableList :: GameState -> [Pos]
movableList (Player c,hc,cc,b) = movable $ getPositionsWithState b (Just c)
  where
    pmps    = getPossibleMovePositions (Player c,hc,cc,b)
    movable = foldr (\p' mps -> pmps p' ++ mps) []

-- Returns a list of all possible moves for the current player
possibleMovesList :: GameState -> [Move]
possibleMovesList s = undefined

-- Returns the best (hehe) position for a placement move for the current player.
-- Assumes the game is not over, so there will be a legal move.
bestPlacement :: GameState -> Pos
bestPlacement (Player c,bpl,wpl,b) = best $ getPositionsWithState b Nothing
  where
    best         = foldr1 (\p' bp -> if better p' bp then p' else bp)
    better np op = playerScore (Player c,bpl,wpl,newBoard np) > 
                   playerScore (Player c,bpl,wpl,newBoard op)
    newBoard     = updateBoard b (Just c)

-- Picks the best capture for the computer to make after a mill 
-- Parameters: starting state and list of possible captures (assume 
-- non-empty)
bestCapture :: GameState -> [Pos] -> Pos
bestCapture (p,_,_,b) ps = head ps -- TODO
 
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
    pmps   = getPossibleMovePositions (Player c,hc,cc,b)
    from   = head [ p | p <- getPositionsWithState b (Just c), not $ null $ pmps p ]
    too    = head $ pmps from
   
