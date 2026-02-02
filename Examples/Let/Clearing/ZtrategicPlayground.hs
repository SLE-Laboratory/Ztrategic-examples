module Examples.Let.Clearing.ZtrategicPlayground where 

import Data.Generics.Zipper
import Data.Generics.Aliases
import Examples.Let.Shared (Root, Errors)

import Examples.Let.LetMemo
import Examples.Let.Let_Zippers (expand)

import Language.Memo.Ztrategic
import Examples.Let.Benchmark (generator)

import qualified Language.Memo.Safe.AGMemo as AGMD
import qualified Language.Memo.Safe.Ztrategic as ZD
import qualified Testing.LetMemo as TLM
import Data.List (union)

----------
----
--- Count the "Add" nodes, optimize, count the "Add" nodes again
----
----------

l = toZipper $ buildMemoTree emptyMemo $ generator 10 
l' = toZipper $ TLM.buildMemoTree TLM.emptyMemo $ generator 10 


baseValue = fst $ adds l 
optValue = fst $ adds $ opt_unclean l 
optValue_unclean  = fst $ adds $ opt_unclean $ snd $ adds l
optValue_clean    = fst $ adds $ opt_clean   $ snd $ adds l
optValue_dep      = (\(a,b,c) -> a) $ TLM.adds $ opt_dep' $ (\(a,b,c) -> b) $ TLM.adds l'
optValue_dep_noWork=(\(a,b,c) -> a) $ TLM.adds $ id $ (\(a,b,c) -> b) $ TLM.adds l'


main = print $ optValue_dep_noWork




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
opt t = letToRoot $ fromZipper $ opt_unclean $ toZipper $ buildMemoTree emptyMemo t 

opt_unclean :: Zipper (Let MemoTable) -> Zipper (Let MemoTable)
opt_unclean z = t'
 where Just t' = applyTP (innermost step) z
       step = failTP `adhocTPZ` exprZ

opt_clean :: Zipper (Let MemoTable) -> Zipper (Let MemoTable)
opt_clean z = t'
 where Just t' = applyTP_clean (innermost step) z
       step = failTP `adhocTPZ` exprZ







exprZDep :: TLM.Let TLM.Table -> Zipper (TLM.Let TLM.Table) -> Maybe (Zipper (TLM.Let TLM.Table))
exprZDep (TLM.Add e (TLM.Const 0 _) m) z         = Just $ setHole e z 
exprZDep (TLM.Add (TLM.Const 0 _) t _) z         = Just $ setHole t z   
exprZDep (TLM.Add (TLM.Const a _) (TLM.Const b _) m) z  = Just $ setHole (TLM.Const (a+b) m) z
exprZDep (TLM.Sub a b m)       z   = Just $ setHole (TLM.Add a (TLM.Neg b TLM.emptyMemo) TLM.emptyMemo) z  
exprZDep (TLM.Neg (TLM.Neg f _) _) z   = Just $ setHole f z
exprZDep (TLM.Neg (TLM.Const n m) _) z   = Just $ setHole (TLM.Const (-n) m) z
exprZDep (TLM.Var i _) z                    = let (e, z', d) = TLM.env z
                                                  (l, z'', d') = TLM.lev z'
                                                  expr :: Maybe (TLM.Let TLM.Table)
                                                  expr = fmap (TLM.buildMemoTreeExp TLM.emptyMemo) $ expand (i, l) e
                                       in fmap (\k -> AGMD.upd' (AGMD.addDependencies (d `union` d')) $ setHole k z'') expr
exprZDep _   z                        = Nothing



opt_dep l = opt_dep' $ toZipper $ TLM.buildMemoTree TLM.emptyMemo l 

opt_dep' :: Zipper (TLM.Let TLM.Table) -> Zipper (TLM.Let TLM.Table)
opt_dep' z = t'
 where Just t' = ZD.applyTP (ZD.full_tdTP step) z
       step = ZD.idTP_invalidateDep `ZD.adhocTPZ` exprZDep