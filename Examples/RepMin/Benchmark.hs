module Examples.RepMin.Benchmark (
    repminNone,
    repminTUTP, 
    repminSTUTP, 
    repminAG,
    repminMemo,
    repminZtrategic,
    repminZtrategicS,
    testTree,
    sumTree, 
    sumTree_m
    )  where 

import qualified Examples.RepMin.RepMinTUTP as RepMinTUTP
import qualified Examples.RepMin.RepMinTUTP_Strafunski as RepMinTUTP_Strafunski

import qualified Examples.RepMin.RepMinAG as RepMinAG
import qualified Examples.RepMin.RepMinMemo as RepMinMemo
import qualified Examples.RepMin.RepMinZtrategic as RepMinZtrategic
import qualified Examples.RepMin.RepMinZtrategicS as RepMinZtrategicS


import Examples.RepMin.Shared
import Examples.RepMin.RepMinMemo (Tree_m(..), MemoTable(..))

-- tree generation and sum of one for each leaf - no repmin
repminNone        size = print $ sumTreeOnes                              $ testTree size

-- expected fastest solutions - guaranteed two simple traversals
repminTUTP        size = print $ sumTree   $ RepMinTUTP.repmin            $ testTree size
repminSTUTP       size = print $ sumTree   $ RepMinTUTP_Strafunski.repmin $ testTree size

-- versions that compute repmin using the classical, inefficient approach 
repminAG          size = print $ sumTree   $ RepMinAG.repmin              $ testTree size
repminMemo        size = print $ sumTree   $ RepMinMemo.repmin            $ testTree size
repminZtrategic   size = print $ sumTree_m $ RepMinZtrategic.repmin       $ testTree size
repminZtrategicS  size = print $ sumTree_m $ RepMinZtrategicS.repmin      $ testTree size



-- --------
-- --
-- - Generation of Balanced Trees  
-- --
-- --------

testTree :: Int -> Tree
testTree n = Root $ buildTree [1..n]
   where buildTree :: [Int] -> Tree
         buildTree [a,b]   = Fork (Leaf a) (Leaf b)
         buildTree [a,b,c] = Fork (Fork (Leaf a) (Leaf b)) (Leaf c)
         buildTree list    = Fork (buildTree $ take half list) (buildTree $ drop half list)
                           where half =  div (length list) 2

-- --------
-- --
-- - Reduction of a Tree  
-- --
-- --------

sumTreeOnes :: Tree -> Int
sumTreeOnes (Leaf l  ) = seq l 1
sumTreeOnes (Fork l r) = sumTreeOnes l + sumTreeOnes r
sumTreeOnes (Root r)   = sumTreeOnes r


sumTree :: Tree -> Int
sumTree (Leaf l  ) = l
sumTree (Fork l r) = sumTree l + sumTree r
sumTree (Root r) = sumTree r


sumTree_m :: Tree_m MemoTable -> Int
sumTree_m (Root_m r m  ) = sumTree_m r
sumTree_m (Leaf_m l m  ) = l
sumTree_m (Fork_m l r m) = sumTree_m l + sumTree_m r


