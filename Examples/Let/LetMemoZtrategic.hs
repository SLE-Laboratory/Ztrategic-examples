module Examples.Let.LetMemoZtrategic where 

import Data.Generics.Zipper
import Data.Generics.Aliases
import Examples.Let.Shared (Root, Errors)

import Examples.Let.LetMemo
import Examples.Let.Let_Zippers (expand)

import Language.Memo.Ztrategic

----------
----
--- Definition of Memoized optimization strategy
----
----------

exprZ :: Let MemoTable -> Zipper (Let MemoTable) -> Maybe (Zipper (Let MemoTable))
exprZ (Add e (Const 0 _) m) z         = Just $ setHole e z 
exprZ (Add (Const 0 _) t _) z         = Just $ setHole t z   
exprZ (Add (Const a _) (Const b _) m) z  = Just $ setHole (Const (a+b) m) z
exprZ (Sub a b m)       z   = Just $ setHole (Add a (Neg b emptyMemo) emptyMemo) z  
exprZ (Neg (Neg f _) _) z   = Just $ setHole f z
exprZ (Neg (Const n m) _) z   = Just $ setHole (Const (-n) m) z
exprZ (Var i _) z                    = let (e, z') = env z
                                           (l, z'') = lev z'
                                           expr :: Maybe (Let MemoTable)
                                           expr = fmap (buildMemoTreeExp emptyMemo) $ expand (i, l) e
                                       in fmap (\k -> setHole k z'') expr      
exprZ _   z                        = Nothing


opt :: Root -> Root
opt t = letToRoot (fromZipper t') 
 where z :: Zipper (Let MemoTable)
       z = toZipper (buildMemoTree emptyMemo t)
       Just t' = applyTP (innermost step) z
       step = failTP `adhocTPZ` exprZ



exprM :: Let MemoTable -> Maybe (Let MemoTable)
exprM (Add e (Const 0 _) m)           = Just $ e
exprM (Add (Const 0 _) t _)           = Just $ t   
exprM (Add (Const a _) (Const b _) m) = Just $ Const (a+b) m
exprM (Sub a b m)          = Just $ Add a (Neg b emptyMemo) emptyMemo
exprM (Neg (Neg f _) _)               = Just $ f
exprM (Neg (Const n m) _)             = Just $ Const (-n) m
exprM _                               = Nothing

exprZM :: Let MemoTable -> Zipper (Let MemoTable) -> Maybe (Zipper (Let MemoTable))
exprZM (Var i _) z = let (e, z') = env z
                         (l, z'') = lev z'
                         expr :: Maybe (Let MemoTable)
                         expr = fmap (buildMemoTreeExp emptyMemo) $ expand (i, l) e
                     in fmap (\k -> setHole k z'') expr      
exprZM _   z       = Nothing

opt2 :: Root -> Root
opt2 t = letToRoot (fromZipper t') 
 where z :: Zipper (Let MemoTable)
       z = toZipper (buildMemoTree emptyMemo t)
       Just t' = applyTP (innermost step) z
       step = failTP `adhocTPZ` exprZM `adhocTP` exprM