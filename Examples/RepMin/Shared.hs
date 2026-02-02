{-# LANGUAGE DeriveDataTypeable #-}
module Examples.RepMin.Shared where 

import Data.Data

import Language.StrategicData

-- --------
-- --
-- - Tree Data Type and Example Tree 
-- --
-- --------

data Tree  =  Root Tree 
           |  Fork Tree Tree 
           |  Leaf Int
            deriving (Show, Data, Typeable)

instance StrategicData Tree

t = Fork  (Fork (Leaf 1) (Leaf 2))
          (Fork (Leaf 3) (Leaf 4))