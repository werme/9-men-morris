module MorrisTest where
import MorrisModel
import MorrisDefinitions
import Test.QuickCheck

instance Arbitrary Pos where
  arbitrary = oneof $ map return positions

instance Arbitrary Player where
  arbitrary = oneof [return $ Player White, return $ Player Black]
  
instance Arbitrary Color where
  arbitrary = oneof [return White, return Black]

-- As the last bits of the Board are not currently in use, this will do:
instance Arbitrary Board where
   arbitrary = 
      do board <- arbitrary
         return (Board board)