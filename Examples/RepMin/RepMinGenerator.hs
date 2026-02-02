{-#LANGUAGE StandaloneDeriving#-}
module Examples.RepMin.RepMinGenerator where

import Examples.RepMin.Shared
import Test.QuickCheck

-- --------
-- --
-- - Generator for RepMin trees
-- --
-- --------

-- instance Arbitrary Tree where 
--     arbitrary = generateTree

{-
generateTree :: Gen Tree
generateTree = fmap Root $ sized generateTree'  
 where generateTree' n = frequency 
                         [(n, Fork <$> generateTree' n <*> generateTree' n),
                          (10, Leaf <$> arbitrary)]
-}


deriving instance Eq Tree

genRootNaive = Root <$> genTreeNaive
genTreeNaive = oneof [
        Fork <$> genTreeNaive <*> genTreeNaive,
        Leaf <$> arbitrary 
    ]

genRoot n = Root <$> genTree n
genTree n = frequency [
        (n, Fork <$> genTree (div n 2) 
                 <*> genTree (div n 2)),
        (1, Leaf <$> arbitrary)
    ]

instance Arbitrary Tree where 
    arbitrary = sized genRoot

{-
generateTree :: Gen Tree
generateTree = fmap Root (generateTree' 80 20) 
 where generateTree' f l = frequency 
                         [(f, Fork 
                                <$> generateTree' (div f 2) (l*2) 
                                <*> generateTree' (div f 2) (l*2)),
                          (l, Leaf <$> arbitrary)]

generateTreeA :: Gen Tree 
generateTreeA = fmap Root (genTreeA' maxBound 80 20)
 where genTreeA' :: Int -> Int -> Int -> Gen Tree 
       genTreeA' mb f l = do  
                            left <- genTreeA' mb (div f 2) (l*2)
                            let minLeft = globmin $ toZipper $ Root left 
                            right <- genTreeA' minLeft (div f 2) (l*2)
                            frequency [(f, return $ Fork left right),
                                       (l, Leaf <$> arbitrary `suchThat` (<mb))] 
-}