module Examples.RepMin.RepMinZtrategic where 

import Examples.RepMin.RepMinMemo
import Examples.RepMin.Shared
import Data.Generics.Zipper
import Data.Generics.Aliases

import Data.Maybe

import Language.Memo.Ztrategic


repmin :: Tree -> Tree_m MemoTable
repmin t = fromZipper t'
  where z :: Zipper (Tree_m MemoTable)
        z = toZipper (buildMemoTree emptyMemo t)
        Just t' = applyTP (full_tdTP step) z
        step = failTP `adhocTPZ` aux

aux :: Tree_m MemoTable -> Zipper (Tree_m MemoTable) -> Maybe (Zipper (Tree_m MemoTable))
aux (Leaf_m _ _) z = Just $ trans (mkT updVal) z' 
    where (r, z') = globmin z 
          updVal :: Tree_m MemoTable -> Tree_m MemoTable
          updVal (Leaf_m v m) = Leaf_m r m
aux _ z = Nothing