{-#LANGUAGE FlexibleInstances#-}
module Examples.RepMin.RepMinGeneratorMemo where

import Examples.RepMin.RepMinMemo
import Test.QuickCheck



genRoot n = Root_m <$> genTree n <*> pure emptyMemo
genTree n = frequency [
        (n, Fork_m <$> genTree (div n 2) 
                 <*> genTree (div n 2)
                 <*> pure emptyMemo),
        (1, Leaf_m <$> arbitrary <*> pure emptyMemo)
    ]

instance Arbitrary (Tree_m MemoTable) where 
    arbitrary = sized genRoot