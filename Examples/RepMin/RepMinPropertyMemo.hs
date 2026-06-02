{-#LANGUAGE TemplateHaskell#-}
module Examples.RepMin.RepMinPropertyMemo where

import Examples.RepMin.RepMinMemo
import Language.Memo.AGMemo
import Examples.RepMin.RepMinGeneratorMemo

import Test.QuickCheck
import Data.Generics.Zipper

import Language.Memo.Ztrategic
import Data.Data (Typeable, Data)
import Language.StrategicData (StrategicData)


-- --------
-- --
-- - Properties over trees and their attributes
-- --
-- --------

forallNodes :: (Typeable a, Data (d mm), Memoizable d mm, StrategicData (d mm)) => (a -> Zipper (d mm) -> ([Property], Zipper (d mm))) -> d mm -> Property
forallNodes p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     (props, _) = applyTU (full_tdTU step) astZipper
    in conjoin props


existsNode :: (Typeable a, Data (d mm), Memoizable d mm, StrategicData (d mm)) => (a -> Zipper (d mm) -> ([Property], Zipper (d mm))) -> d mm -> Property
existsNode p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     (props, _) = applyTU (full_tdTU step) astZipper
    in disjoin props




-- for any node, locmin >= globmin
prop_locMin :: Tree_m MemoTable -> Property
prop_locMin ast = forallNodes validateLocMin ast

validateLocMin :: Tree_m MemoTable -> Zipper (Tree_m MemoTable) -> ([Property], Zipper (Tree_m MemoTable))
validateLocMin _ z = let (computedLocmin, z') = locmin z
                         (computedGlobmin, z'') = globmin z'
                             in  ([property (computedLocmin >= computedGlobmin)], z'')


-- for any int, globmin is smaller than it
prop_globminIsSmaller :: Tree_m MemoTable -> Property
prop_globminIsSmaller ast = forallNodes globminIsSmaller ast

globminIsSmaller :: Tree_m MemoTable -> Zipper (Tree_m MemoTable) -> ([Property], Zipper (Tree_m MemoTable))
globminIsSmaller (Leaf_m n _)   z = let (computedGlobmin, z') = globmin z
                                in ([property (computedGlobmin <= n)], z')
globminIsSmaller _ z = ([], z)


-- to be able to run all tests at once. Note that they must start with "prop_".
return []
runTests = $quickCheckAll