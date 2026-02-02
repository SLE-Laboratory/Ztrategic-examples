{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances, 
             DeriveDataTypeable
#-}

module Examples.RepMin.RepMinMemo where

import Examples.RepMin.Shared
import Examples.RepMin.SharedAG
import Data.Generics.Zipper hiding (up, down', left, right)
import Data.Generics.Aliases
import Data.Data
import Language.ZipperAG
import Data.Maybe (fromJust)

import Language.Memo.AGMemo
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
constructor_m a = case ( getHole a :: Maybe (Tree_m MemoTable)) of
                 Just (Root_m _ _)   -> CRoot_m
                 Just (Fork_m _ _ _) -> CFork_m
                 Just (Leaf_m _ _)   -> CLeaf_m
                 _               -> error "Naha, that production does not exist!"

lexeme_m :: Typeable a => Zipper a -> Int 
lexeme_m a = case (getHole a :: Maybe (Tree_m MemoTable)) of
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

type MemoTable = ( Maybe Int     -- Globmin
                 , Maybe Int     -- Locmin
                 , Maybe Tree    -- Replace
                 )
                
emptyMemo = (Nothing,Nothing,Nothing)

instance Memo Globmin MemoTable Int where
  mlookup _   (g,_,_) = g
  massign _ v (g,l,r) = (Just v,l,r)

instance Memo Locmin MemoTable Int where
  mlookup _   (_,l,_) = l
  massign _ v (g,l,r) = (g,Just v,r)

instance Memo Replace MemoTable Tree where
  mlookup _   (_,_,r) = r
  massign _ v (g,l,r) = (g,l,Just v)

instance Memoizable Tree_m MemoTable where 
  updMemoTable = updMemoTable'
  getMemoTable = getMemoTable'

instance StrategicData (Tree_m MemoTable) where 
  isTerminal z = isJust (getHole z :: Maybe MemoTable)
              || isJust (getHole z :: Maybe Int      )


-- --------
-- --
-- - Definition of Memoized Attributes
-- --
-- --------

-- Inherited
globmin :: (Memoizable Tree_m m, Memo Globmin m Int, Memo Locmin m Int) => AGTree_m Tree_m m Int
globmin = memo Globmin $ \z -> case constructor_m z of
                                 CRoot_m -> locmin z
                                 CLeaf_m -> globmin `atParent` z 
                                 CFork_m -> globmin `atParent` z

-- Synthesized
locmin :: (Memoizable Tree_m m, Memo Locmin m Int) => AGTree_m Tree_m m Int
locmin  = memo Locmin $ \z -> case constructor_m z of 
                                CRoot_m -> locmin .@. (z.$1)
                                CLeaf_m -> (lexeme_m z,z)
                                CFork_m -> let (left ,z' ) = locmin .@. (z.$1)
                                               (right,z'') = locmin .@. (z'.$2)
                                           in  (min left right, z'')

replace :: (Memoizable Tree_m m, Memo Replace m Tree, Memo Globmin m Int, Memo Locmin m Int) =>
           AGTree_m Tree_m m Tree
replace = memo Replace $ \z -> case constructor_m z of     
                                 CRoot_m -> replace .@. (z.$1)
                                 CLeaf_m -> let (mini, z') = globmin z 
                                            in  (Leaf mini, z')
                                 CFork_m -> let (l,z')  = replace .@. (z.$1) 
                                                (r,z'') = replace .@. (z'.$2) 
                                            in  (Fork l r, z'')

-- --------
-- --
-- - Definition of RepMin 
-- --
-- --------

repmin :: Tree -> Tree
repmin t = t'
  where (t',_) = replace z
        z :: Zipper (Tree_m MemoTable)
        z = mkAG (buildMemoTree emptyMemo t)
