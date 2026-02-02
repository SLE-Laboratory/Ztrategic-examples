{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module Examples.RepMin.RepMinTUTP where 

import Data.Data
import Data.Generics.Zipper hiding (left, right, up, down')
import Data.Maybe (fromJust)

import Language.Ztrategic
import Language.StrategicData

import Examples.RepMin.Shared

import Control.Monad.State.Lazy
import Debug.Trace


-- --------
-- --
-- - Zipper Examples
-- --
-- --------

t_1 = toZipper t

leafTwo :: Maybe Tree
leafTwo = (getHole  .  fromJust . right    
                    .  fromJust . down' .  fromJust . down') t_1


-- --------
-- --
-- - RepMin defined as two strategies
-- --
-- --------

nodeVal :: Tree -> [Int]
nodeVal (Leaf x)  = [x]
nodeVal _         = []

minimumValue :: Tree -> Int
minimumValue t =  foldr1TU step (toZipper t) min
       where   step = failTU `adhocTU` nodeVal




replace :: Int -> Tree -> Maybe Tree
replace i (Leaf r)  = Just (Leaf i)
replace i x        = Just x 

replaceTree :: Int -> Tree -> Tree
replaceTree v t =  fromZipper $ fromJust $ 
                   applyTP (full_tdTP step) (toZipper t)
  where step = idTP `adhocTP` (replace v)


repmin t =  let  v = minimumValue t 
          in   replaceTree v t 


-- --------
-- --
-- - Testing new combinators
-- --
-- --------

t'_ = let subtree a b = (Fork (Leaf a) (Leaf b))
     in Fork (Fork (subtree 1 2) (subtree 3 4)) (Fork (subtree 5 6) (subtree 7 8))

t' = toZipper t'_

t'' = fromJust $ down' $ fromJust $ down' $ fromJust $ down' $ t'

monadic :: Tree -> StateT [Int] Maybe Tree
monadic (Leaf n) = modify (++ [n]) >> (return $ Leaf n)
monadic v = return v

strategy_inPlace :: Zipper Tree -> StateT [Int] Maybe (Zipper Tree)
strategy_inPlace r = applyTP (full_tdTP step) r
 where step = idTP `adhocTP` monadic

run_example_inPlace :: Zipper Tree -> [Int]
run_example_inPlace v = fromJust $ execStateT (strategy_inPlace $ v) []

strategy_atRoot :: Zipper Tree -> StateT [Int] Maybe (Zipper Tree)
strategy_atRoot r = applyTP (atRoot (full_tdTP step)) r
 where step = idTP `adhocTP` monadic

run_example_atRoot :: Zipper Tree -> [Int]
run_example_atRoot v = fromJust $ execStateT (strategy_atRoot $ v) []

{-
------ for t'', which is t focused on subtree with values 1 and 2: 
> run_example_inPlace t''
[1,2]
> run_example_atRoot  t''
[1,2,3,4,5,6,7,8]
-}

sumTree :: Zipper Tree -> Int
sumTree t =  foldr1TU step t (+)
       where   step = failTU `adhocTU` nodeVal

monadicSums :: Tree -> Zipper Tree -> StateT [Int] Maybe Tree
monadicSums v z = modify (++ [sumTree z]) >> return v

strategy_towardsRoot :: Zipper Tree -> StateT [Int] Maybe (Zipper Tree)
strategy_towardsRoot r = applyTP (full_upbuTP step) r
 where step = idTP `adhocTPZ` monadicSums

run_example_towardsRoot :: Zipper Tree -> [Int]
run_example_towardsRoot v = fromJust $ execStateT (strategy_towardsRoot $ v) []

{-
------ for t'', which is t focused on subtree with values 1 and 2: 
> run_example_towardsRoot t''
[3, 10, 36, 36]
------ We do get 36 twice, one computed at the topmost Fork and another computed at the root Root node
-}


{-
-- https://github.com/zyla/haskell-nullpointer
full_tdTPUp :: Zipper Tree -> StateT [Int] Maybe (Zipper Tree)
full_tdTPUp z =
     let (Just v) = (getHole z) :: Maybe Tree
         z' = trans forbid z
         step = idTP `adhocTP` monadic
         traversed = applyTP (atRoot (full_tdTP step)) z' 
     in fmap (setHole v) traversed

full_tdTPUp :: forall a. StrategicData a => Zipper a -> StateT [Int] Maybe (Zipper a)
full_tdTPUp z =
     let (Just v) = (getHole @(Maybe a) z)
         z' = trans forbid z
         step = idTP `adhocTP` monadic
         traversed = applyTP (atRoot (full_tdTP step)) z' 
     in fmap (setHole v) traversed
-}


{-
proxyMaybe :: Proxy b -> Maybe b -> Maybe b
proxyMaybe _ x = x

full_tdTPUp :: (Typeable b, StrategicData a) => Proxy b -> Zipper a -> StateT [Int] Maybe (Zipper a)
full_tdTPUp p z =
     let (Just v) = proxyMaybe p (getHole z)  
         z' = setNull p z
         step = idTP `adhocTP` monadic
         traversed = applyTP (atRoot (full_tdTP step)) z' 
     in fmap (setHole v) traversed
-}

-- run_example_Up :: Zipper Tree -> [Int]
-- run_example_Up v = fromJust $ execStateT (applyTP (full_tdTPupwards (Proxy :: Proxy Tree)step) v) []
--  where step = idTP `adhocTP` monadic

full_tdTPDown :: Zipper Tree -> StateT [Int] Maybe (Zipper Tree)
full_tdTPDown z = applyTP (full_tdTP step) z 
 where step = idTP `adhocTP` monadic

run_example_Down :: Zipper Tree -> [Int]
run_example_Down v = fromJust $ execStateT (full_tdTPDown $ v) []


t''' = fromJust $ up $ t''

---------
-- Breadth-first traversal
---------

w = Fork (Fork (Fork (Leaf 0) (Leaf 0)) (Leaf 0)) (Leaf 0)

breadthFirstNumbering z = fromZipper $ evalState (applyTP (breadthFirst_tdTP step) $ toZipper z) 0
 where step = idTP `adhocTP` number 

number :: MonadState Int m => Tree -> m Tree
number (Leaf _) = modify (+1) >> get >>= return . Leaf
number v = return v

{- 
         ^ 
       /   \
      ^     0
     / \
    ^   0
   / \
  0   0 

becomes 

         ^ 
       /   \
      ^     1
     / \
    ^   2
   / \
  3   4 
-}