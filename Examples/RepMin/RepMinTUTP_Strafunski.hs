module Examples.RepMin.RepMinTUTP_Strafunski where 

import Data.Maybe (fromJust)

import qualified Data.Generics.Strafunski.StrategyLib.StrategyLib as S

import Examples.RepMin.Shared

-- --------
-- --
-- - RepMin defined as two strategies, using Strafunski 
-- --
-- --------

minimumValue :: Tree -> Int
minimumValue t = minimum $ head $ S.applyTU (S.full_tdTU step) t
       where step = S.constTU [] `S.adhocTU` nodeVal

nodeVal :: Tree -> [[Int]]
nodeVal (Leaf x) = return [x]
nodeVal _        = return []


replace :: Int -> Tree -> Maybe Tree
replace i (Leaf r)  = Just (Leaf i)
replace i x         = Just x 

replaceTree :: Int -> Tree -> Tree
replaceTree v t =  fromJust $ S.applyTP (S.full_tdTP step) t
  where step = S.idTP `S.adhocTP` (replace v) 


repmin t =  let  v = minimumValue t 
          in   replaceTree v t 
