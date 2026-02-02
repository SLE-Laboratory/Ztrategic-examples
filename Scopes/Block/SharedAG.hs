-----------------------------------------------------------------------
--
--  The Fun of Programming With Attribute Grammars
--
--  code shared by AG solutions
--
----------------------------------------------------------------------

module Scopes.Block.SharedAG ( module Scopes.Block.Shared, Constructor(..), constructor, lexeme
                , AGTree) where

import Language.ZipperAG hiding ((.$>),(.$<))
import Data.Generics.Zipper
import Data.Data
import Scopes.Block.Shared

data Constructor = CConsIts
                 | CNilIts
                 | CDecl
                 | CUse
                 | CBlock
                 | CRoot
                deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case ( getHole a :: Maybe Its ) of
                 Just (ConsIts _ _) -> CConsIts
                 Just (NilIts) -> CNilIts
                 otherwise -> case ( getHole a :: Maybe It ) of
                                Just (Decl _ _) -> CDecl
                                Just (Use _ _) -> CUse
                                Just (Block _) -> CBlock
                                otherwise -> case ( getHole a :: Maybe P) of 
                                                Just (Root _) -> CRoot
                                                otherwise -> error "Naha, that production does not exist!"

lexeme z = case (getHole z :: Maybe It) of
            Just (Use x _) -> x
            Just (Decl x _) -> x
            Just (Block a) -> (show a) ++ "!!!! SHOWING BLOCK (TODO REMOVE THIS)"
            _              -> error "unexpected use of Block lexeme!"

type AGTree a  = Zipper P -> a
