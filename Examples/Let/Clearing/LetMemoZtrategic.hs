module Examples.Let.Clearing.LetMemoZtrategic where 

import Data.Generics.Zipper
import Data.Generics.Aliases
import Examples.Let.Shared (Root, Errors)

import Examples.Let.LetMemo
import Examples.Let.Let_Zippers (expand)

import Language.Ztrategic

----------
----
--- Definition of Memoized optimization strategy with no MemoTable propagation
----
----------

exprM :: Let MemoTable -> Maybe (Let MemoTable)
exprM (Add e (Const 0 _) m)           = Just $ e
exprM (Add (Const 0 _) t _)           = Just $ t   
exprM (Add (Const a _) (Const b _) m) = Just $ Const (a+b) m
exprM (Sub a b m)          = Just $ Add a (Neg b emptyMemo) emptyMemo
exprM (Neg (Neg f _) _)               = Just $ f
exprM (Neg (Const n m) _)             = Just $ Const (-n) m
exprM _                               = Nothing

exprX :: Let MemoTable -> Zipper (Let MemoTable) -> Maybe (Let MemoTable)
exprX (Var i _) z = let (e, _) = env z
                        (l, _) = lev z
                    in fmap (buildMemoTreeExp emptyMemo) $ expand (i, l) e
exprX _   z       = Nothing

opt :: Root -> Root
opt t = letToRoot (fromZipper t') 
 where z :: Zipper (Let MemoTable)
       z = toZipper (buildMemoTree emptyMemo t)
       Just t' = applyTP (innermost step) z
       step = failTP `adhocTPZ` exprX `adhocTP` exprM