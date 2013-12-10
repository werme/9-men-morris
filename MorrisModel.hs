module MorrisModel where
import Data.Bits
import Data.Word
import Data.Maybe

newtype Pos = Pos Int deriving (Eq, Ord, Show)
newtype Board = Board Word64 deriving (Eq, Ord, Show)
data Color = White | Black deriving (Eq, Show)

-- Updates the board with the correct value (white, black or empty) for pos
updateBoard :: Board -> Maybe Color -> Pos -> Board
updateBoard (Board bitBoard) c (Pos n) =
    Board $ newBits .|. (bitMask .&. bitBoard)
    where   index   = n * 2
            newBits = shiftL (translate c) index
            bitMask = complement $ shiftL 3 index
            translate (Just White)  = 2
            translate (Just Black)  = 1
            translate Nothing       = 0

getPositionState :: Board -> Pos -> Maybe Color
getPositionState (Board bitBoard) (Pos n) =
    translate value
    where   index   = n * 2
            value   = 3 .&. (shiftR bitBoard index)
            translate 2 = (Just White)
            translate 1 = (Just Black)
            translate 0 = Nothing