module Examples.RepMin.RepMinAG where

import Examples.RepMin.Shared
import Examples.RepMin.SharedAG

import Data.Generics.Zipper
import Language.ZipperAG


type AGTree a = Zipper Tree -> a

-- Repmin

globmin :: AGTree Int
globmin t = case constructor t of
            CRoot   -> locmin (t.$1)
            CLeaf   -> globmin (parent t)
            CFork   -> globmin (parent t)

locmin :: AGTree Int
locmin t = case constructor t of
            CRoot   -> locmin (t.$1) 
            CLeaf   -> lexeme t 
            CFork   -> min (locmin (t.$1))
                           (locmin (t.$2))


replace :: AGTree Tree
replace t = case constructor t of
            CRoot   -> Root (replace (t.$1))
            CLeaf   -> Leaf (globmin t)
            CFork   -> Fork (replace (t.$1))
                            (replace (t.$2))

count :: AGTree Int 
count t = case constructor t of 
            CRoot -> count (t.$1)
            CFork -> count (t.$1) + count (t.$2)
            CLeaf -> 1 


repmin tree = replace (mkAG tree)
