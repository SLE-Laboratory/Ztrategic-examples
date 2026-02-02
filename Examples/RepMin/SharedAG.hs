module Examples.RepMin.SharedAG where 

import Data.Generics.Zipper
import Data.Data

import Examples.RepMin.Shared


data Constructor = CRoot 
                 | CFork 
                 | CLeaf
                deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case ( getHole a :: Maybe Tree ) of
                 Just (Root _)   -> CRoot
                 Just (Fork _ _) -> CFork
                 Just (Leaf _)   -> CLeaf
                 _               -> error "Naha, that production does not exist!"


lexeme :: Typeable a => Zipper a -> Int 
lexeme a = case ( getHole a :: Maybe Tree ) of
                 Just (Leaf v) -> v
                 _             -> error "use of Leaf lexeme"
