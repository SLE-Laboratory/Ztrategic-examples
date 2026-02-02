{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Examples.Let.LetMisc where 

import Data.Generics.Zipper
import Data.Maybe (fromJust)

import Language.Ztrategic
import Language.StrategicData

import Examples.Let.Shared 
import Examples.Let.Let_Zippers

instance StrategicData Let

countAssigns :: List -> Maybe [String]
countAssigns (Assign s _ _)    = Just [s]
countAssigns (NestedLet s _ _) = Just [s]
countAssigns _                 = Nothing 

declarations :: Zipper Let -> Int 
declarations t = let result = applyTU (full_tdTU step) t 
                     step   = failTU `adhocTU` countAssigns
                 in case result of 
                      Nothing -> 0
                      Just l  -> length l


-- declarations $ toZipper ex3


instance StrategicData String
occurrences :: Int
occurrences = foldr1TU step (toZipper "Test") (+)
 where step = constTU 1 :: TU [] Int