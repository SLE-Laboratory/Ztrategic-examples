{-#LANGUAGE TemplateHaskell#-}
module Examples.RepMin.RepMinProperty where

import Examples.RepMin.Shared
import Examples.RepMin.RepMinAG
import Examples.RepMin.RepMinGenerator

import Test.QuickCheck
import Data.Generics.Zipper

import Language.Ztrategic
import Data.Data (Typeable, Data)
import Language.StrategicData (StrategicData)


-- --------
-- --
-- - Properties over trees and their attributes
-- --
-- --------

forallNodes :: (Typeable a, Data b, StrategicData b) => (a -> Zipper b -> [Property]) -> b -> Property
forallNodes p ast = let
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     props = applyTU (full_tdTU step) astZipper
    in conjoin props


existsNode :: (Typeable a, Data b, StrategicData b) => (a -> Zipper b -> [Property]) -> b -> Property
existsNode p ast = let
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     props = applyTU (full_tdTU step) astZipper
    in disjoin props


prop_count_2 :: Tree -> Bool
prop_count_2 ast = count (toZipper t)
    == count (toZipper (replace (toZipper t)))

prop_count :: Tree -> Property
prop_count ast = property (count (toZipper t)
    == count (toZipper (replace (toZipper t))))

-- for any node, locmin >= globmin
prop_locMin :: Tree -> Property
prop_locMin ast = forallNodes validateLocMin ast

validateLocMin :: Tree -> Zipper Tree -> [Property]
validateLocMin _ z = [property (locmin z >= globmin z)]


-- for any int, globmin is smaller than it
prop_globminIsSmaller :: Tree -> Property
prop_globminIsSmaller ast = forallNodes globminIsSmaller ast

globminIsSmaller :: Tree -> Zipper Tree -> [Property]
globminIsSmaller (Leaf n)   z = [property (globmin z <= n)]
globminIsSmaller _          _ = []

-- all values of resulting tree are contained in the original tree
-- forall integer i in t.replace, exists i in t
prop_resultingInOriginal :: Tree -> Property
prop_resultingInOriginal ast = forallNodes intInOriginal (replace $ toZipper ast)
 where intInOriginal :: Int -> Zipper Tree -> [Property]
       intInOriginal i _ = return $ existsNode equal ast
        where equal j _ = return $ property $ i == j



-- same number of leaves 
prop_numberLeaves :: Tree -> Property
prop_numberLeaves t = property $ countLeaves t == countLeaves (replace $ toZipper t)

countLeaves :: Tree -> Int
countLeaves (Root t) = countLeaves t
countLeaves (Fork l r) = countLeaves l + countLeaves r
countLeaves (Leaf _) = 1

-- globmin of resulting tree is the same as globmin of original tree 
prop_globminPreserved :: Tree -> Property
prop_globminPreserved t = let t' = toZipper t
                          in property $ globmin t' == globmin (toZipper $ replace t')


-- globmin is in the tree
prop_globminInTree :: Tree -> Property
prop_globminInTree ast = existsNode isGlobmin ast

isGlobmin :: Tree -> Zipper Tree -> [Property]
isGlobmin (Leaf n) z = [property (n == globmin z)]
isGlobmin _ _ = []


-- trees are isomorphic before and after replace
isomorphic :: Tree -> Tree -> Bool
isomorphic (Leaf _) (Leaf _) = True
isomorphic (Root x) (Root y) = isomorphic x y
isomorphic (Fork l1 r1) (Fork l2 r2) =
        isomorphic l1 l2 && isomorphic r1 r2
isomorphic _ _ = False

prop_isomorphic :: Tree -> Property
prop_isomorphic t = property (isomorphic t (replace (toZipper t)))


-- applying replace once and twice is the same
prop_idempotent :: Tree -> Property
prop_idempotent t =
    property (replace (toZipper t) ==
    replace (toZipper (replace (toZipper t))))


-- all values on tree produced by replace are the same
prop_allEqual :: Tree -> Property
prop_allEqual ast = let x = replace (toZipper ast) in
    forallNodes (iter1 x) x

iter1 x (Leaf n1) _ = [forallNodes (iter2 n1) x]
iter1 _ _ _ = []

iter2 n1 (Leaf n2) _ = [property (n1 == n2)]
iter2 n1 _ _ = []


-- to be able to run all tests at once. Note that they must start with "prop_".
return []
runTests = $quickCheckAll


{-
---
---
---

-- non-memo and memo behave equally
prop_repminMemo :: Tree -> Property
prop_repminMemo t = property $ replace (toZipper t) == Root (Memo.repmin t)

-- all nodes have globmin which we artificially insert 
prop_memoHasGlobmin ::  Int -> Tree -> Property
prop_memoHasGlobmin n t = 
        let
            injectedMemo = (Just n, Nothing, Nothing)
            z :: Zipper (Memo.Tree_m Memo.MemoTable)
            z = toZipper $ Memo.buildMemoTree injectedMemo t
            (newTree, _) = Memo.replace z
        in conjoin $ applyTU (full_tdTU step) $ toZipper newTree 
            
 where step = failTU `adhocTUZ` globminInNode
       globminInNode :: Tree -> Zipper Tree -> [Property]
       globminInNode (Leaf l) z = return $ property $ l == n
       globminInNode _        _ = []
-}