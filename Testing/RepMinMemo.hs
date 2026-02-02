{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances, 
             DeriveDataTypeable
#-}

module Testing.RepMinMemo where

import Examples.RepMin.Shared hiding (t)
import Examples.RepMin.SharedAG
import Data.Generics.Zipper hiding (up, down', left, right)
import Data.Generics.Aliases
import Data.Data
import Language.ZipperAG
import Data.Maybe (fromJust)
import Data.List (union)

import Language.Memo.Safe.AGMemo
import Language.Memo.Safe.Ztrategic
import Language.StrategicData

-- --------
-- --
-- - Memoized data type and relevant functions 
-- --
-- --------

data Tree_m m 
  =  Root_m (Tree_m m) m 
  |  Fork_m (Tree_m m) (Tree_m m) m
  |  Leaf_m Int m 
 deriving (Show, Data, Typeable)

buildMemoTree :: m -> Tree -> Tree_m m
buildMemoTree m (Root t  )
  = Root_m (buildMemoTree m t) m
buildMemoTree m (Fork l r)
  = Fork_m (buildMemoTree m l) (buildMemoTree m r) m
buildMemoTree m (Leaf i  )
  = Leaf_m i m              

updMemoTable' :: (m -> m) -> Tree_m m -> Tree_m m
updMemoTable' f (Root_m t m)   = Root_m t (f m) 
updMemoTable' f (Leaf_m i m)   = Leaf_m i (f m) 
updMemoTable' f (Fork_m l r m) = Fork_m l r (f m) 

getMemoTable' :: Tree_m m -> m
getMemoTable' (Root_m _ m)   = m
getMemoTable' (Leaf_m _ m)   = m
getMemoTable' (Fork_m _ _ m) = m


-- --------
-- --
-- - AG Boilerplate 
-- --
-- --------

data Constructor_m = CRoot_m 
                   | CFork_m 
                   | CLeaf_m
                  deriving Show

constructor_m :: (Typeable a) => Zipper a -> Constructor_m
constructor_m a = case ( getHole a :: Maybe (Tree_m Table)) of
                 Just (Root_m _ _)   -> CRoot_m
                 Just (Fork_m _ _ _) -> CFork_m
                 Just (Leaf_m _ _)   -> CLeaf_m
                 _               -> error "Naha, that production does not exist!"

lexeme_m :: Typeable a => Zipper a -> Int 
lexeme_m a = case (getHole a :: Maybe (Tree_m Table)) of
                 Just (Leaf_m v _) -> v
                 _                 -> error "use of Leaf_m lexeme"


-- --------
-- --
-- - Definition of attributes, Memo Table and relevant instances 
-- --
-- --------

data Globmin = Globmin
data Locmin  = Locmin
data Replace = Replace

type MemoTableValues = ( Maybe Int     -- Globmin
                 , Maybe Int     -- Locmin
                 , Maybe Tree    -- Replace
                 )

type Table = (MemoTableValues, [Dependency], Bool)

emptyMemoV = (Nothing,Nothing,Nothing)
emptyMemo = (emptyMemoV, [], False)

instance MemoTable Table where 
  isValidMemoTable    (_, _  , isV)        = isV
  invalidateMemoTable (v, dep, isV)        = (v,dep,False)
  validateMemoTable   (v, dep, isV)        = (v,dep,True)
  getDependencies     (_, dep, _)          = dep
  addDependency       newDep (v, dep, isV) = if newDep `elem` dep then (v, dep, isV) else (v, newDep:dep, isV)
           

instance Memo Globmin Table Int where
  mlookup _   ((g,_,_), dep, isV) = g
  massign _ v ((g,l,r), dep, isV) = ((Just v,l,r), dep, isV)

instance Memo Locmin Table Int where
  mlookup _   ((_,l,_), dep, isV) = l
  massign _ v ((g,l,r), dep, isV) = ((g,Just v,r), dep, isV)

instance Memo Replace Table Tree where
  mlookup _   ((_,_,r), dep, isV) = r
  massign _ v ((g,l,r), dep, isV) = ((g,l,Just v), dep, isV)

instance Memoizable Tree_m Table where 
  updMemoTable = updMemoTable'
  getMemoTable = getMemoTable'

instance StrategicData (Tree_m Table) where 
  isTerminal z = isJust (getHole z :: Maybe Table)
              || isJust (getHole z :: Maybe Int      )


-- --------
-- --
-- - Definition of Memoized Attributes
-- --
-- --------

-- Inherited
globmin :: (Memo Globmin Table Int, Memo Locmin Table Int) => AGTree_m Tree_m Table Int
globmin = memo Globmin $ \z -> case constructor_m z of
                                 CRoot_m -> locmin z
                                 CLeaf_m -> globmin `atParent` z 
                                 CFork_m -> globmin `atParent` z

-- Synthesized
locmin :: (Memo Locmin Table Int) => AGTree_m Tree_m Table Int
locmin  = memo Locmin $ \z -> case constructor_m z of 
                                CRoot_m -> atChild locmin z 1
                                CLeaf_m -> (lexeme_m z,z, [])
                                CFork_m -> let (left ,z' , d ) = atChild locmin z  1
                                               (right,z'', d') = atChild locmin z' 2
                                           in  (min left right, z'', d `union` d')

replace :: (Memo Replace Table Tree, Memo Globmin Table Int, Memo Locmin Table Int) =>
           AGTree_m Tree_m Table Tree
replace = memo Replace $ \z -> case constructor_m z of     
                                 CRoot_m -> atChild replace z 1
                                 CLeaf_m -> let (mini, z', d) = globmin z 
                                            in  (Leaf mini, z', d)
                                 CFork_m -> let (l,z' , d ) = atChild replace z  1 
                                                (r,z'', d') = atChild replace z' 2 
                                            in  (Fork l r, z'', d `union` d')

-- --------
-- --
-- - Definition of RepMin 
-- --
-- --------

repmin :: Tree -> Tree
repmin t = t'
  where (t',_, _) = replace z
        z :: Zipper (Tree_m Table)
        z = mkAG (buildMemoTree emptyMemo t)


t = Root $ Fork  (Fork (Leaf 1) (Leaf 2))
                 (Fork (Leaf 3) (Leaf 4))


-- --------
-- --
-- - Definition of RepMin as a Strategy
-- --
-- --------

repminDepStrategy :: Tree -> Tree_m Table
repminDepStrategy t = fromZipper t'
  where z :: Zipper (Tree_m Table)
        z = toZipper (buildMemoTree emptyMemo t)
        Just t' = applyTP (full_tdTP step) z
        step = idTP_invalidateDep `adhocTPZ` aux
-- idTP_invalidateDep


aux :: Tree_m Table -> Zipper (Tree_m Table) -> Maybe (Zipper (Tree_m Table))
aux (Leaf_m _ _) z = Just $ trans (mkT updVal) z' 
    where (r, z', d) = globmin z 
          updVal :: Tree_m Table -> Tree_m Table
          updVal (Leaf_m v m) = Leaf_m r m
aux _ z = Nothing

-- --------
-- --
-- - For Benchmarking and testing
-- --
-- --------

sumTree_m :: Tree_m Table -> Int
sumTree_m (Root_m r m  ) = sumTree_m r
sumTree_m (Leaf_m l m  ) = l
sumTree_m (Fork_m l r m) = sumTree_m l + sumTree_m r