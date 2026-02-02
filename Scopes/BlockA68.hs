-- module Scopes.BlockA68 (block_a68, dead) where
module Scopes.BlockA68 where
import Data.Data ( Data, Typeable )
import Data.Generics.Zipper
import Language.Ztrategic
import Data.Generics.Aliases
import Language.ZipperAG
import Data.Maybe
import Debug.Trace
import  Language.StrategicData
import  Scopes.Block.Shared 
-- import  Scopes.Block.Block_Zippers 
import  Scopes.Block.SharedAG 
----------------------------------------------------------------------------------------------------------------------------------------------
-- ALGOL 68 block processor
----------------------------------------------------------------------------------------------------------------------------------------------
dclo :: Env -> Zipper P -> Env
dclo a t =  case constructor t of
                    CNilIts   -> dcli a t
                    CConsIts  -> dclo a (t.$2)
                    CDecl     -> (lexeme t,lev t, fromJust $ getHole t) : dcli a t
                    CUse      -> dcli a t
                    CBlock    -> dcli a t
                    CRoot     -> dclo a (t.$1)

errors :: Env -> Zipper P -> Errors
errors a t =  case constructor t of
                       CRoot     -> errors a (t.$1)
                       CNilIts   -> []
                       CConsIts  -> errors a (t.$1) ++ errors a (t.$2)
                       CBlock    -> errors a (t.$1)
                       CUse      -> mustBeIn (lexeme t) (fromJust $ getHole t) (env a t) a
                       CDecl     -> mustNotBeIn (lev t) (lexeme t) (fromJust $ getHole t) (dcli a t) a

dcli :: Env -> Zipper P -> Env
dcli a t =  case constructor t of
                    CRoot     -> a
                    CNilIts   -> case  constructor $ parent t of
                                             CConsIts  -> dclo a (t.$<1)
                                             CBlock    -> env a (parent t)
                                             CRoot     -> []
                    CConsIts  -> case  constructor $ parent t of
                                             CConsIts  -> dclo a (t.$<1)
                                             CBlock    -> env a (parent t)
                                             CRoot     -> []
                    CBlock    -> dcli a (parent t)
                    CUse      -> dcli a (parent t)
                    CDecl     -> dcli a (parent t)

lev :: AGTree Int
lev t =  case constructor t of
                     CRoot     ->  0
                     CBlock    ->  lev (parent t)
                     CUse      ->  lev (parent t)
                     CDecl     ->  lev (parent t)
                     _         ->  case  constructor $ parent t of
                                        CBlock    -> lev (parent t) + 1
                                        CConsIts  -> lev (parent t)
                                        CRoot     -> 0

env :: Env -> Zipper P -> Env
env a t =  case constructor t of
                    CRoot      ->  dclo a t
                    CBlock     ->  env a (parent t)
                    CUse       ->  env a (parent t)
                    CDecl      ->  env a (parent t)
                    _          ->  case constructor $ parent t of
                                                CBlock    -> dclo a t
                                                CConsIts  -> env a (parent t)
                                                CRoot     -> dclo a t


mustBeIn :: Name -> It -> Env -> Env -> Errors
mustBeIn n i e a = [(n, i, " <= [Undeclared use!]") | not (any ((== n) . fst3) (e++a))]

mustNotBeInE :: Int -> Name ->  It -> Env -> Errors
mustNotBeInE i name item e =
    let names = map (\(name', lev, _) -> (name',lev)) e
        errorMsg = " <= [Duplicated declaration!]"
    in  [(name, item, errorMsg) | (name,i) `elem` names]

mustNotBeInA :: Name -> It -> Env -> Errors
mustNotBeInA name item a =
    let items = filter (\(name', _, item') -> name' == name && item /= item') a
        errorMsg = " <= [Different declaration!]"
    in  [(name, item, errorMsg) | not (null items)]

mustNotBeIn :: Int -> Name -> It -> Env -> Env -> Errors
mustNotBeIn i name item e a = mustNotBeInE i name item e ++
                              mustNotBeInA name item a


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

block_a68 :: Env -> P -> Errors
block_a68 a p = errors a (mkAG p)



-- --------
-- --
-- - Same but only for current block! 
-- --
-- --------

dcloBlock :: Env -> Zipper P -> Env
dcloBlock a t =  case constructor t of
                    CNilIts   -> dcliBlock a t
                    CConsIts  -> dcloBlock a (t.$2)
                    CDecl     -> (lexeme t,lev t, fromJust $ getHole t) : dcliBlock a t
                    CUse      -> dcliBlock a t
                    CBlock    -> dcliBlock a t
                    CRoot     -> dcloBlock a (t.$1)


dcliBlock :: Env -> Zipper P -> Env
dcliBlock a t =  case constructor t of
                    CRoot     -> [] -- modified
                    CNilIts   -> case  constructor $ parent t of
                                             CConsIts  -> dcloBlock a (t.$<1)
                                             CBlock    -> [] -- modified
                                             CRoot     -> []
                    CConsIts  -> case  constructor $ parent t of
                                             CConsIts  -> dcloBlock a (t.$<1)
                                             CBlock    -> [] -- modified
                                             CRoot     -> []
                    CBlock    -> dcliBlock a (parent t)
                    CUse      -> dcliBlock a (parent t)
                    CDecl     -> dcliBlock a (parent t)

envBlock :: Env -> Zipper P -> Env
envBlock a t =  case constructor t of
                    CRoot      ->  dcloBlock a t
                    CBlock     ->  envBlock a (parent t)
                    CUse       ->  envBlock a (parent t)
                    CDecl      ->  envBlock a (parent t)
                    _          ->  case constructor $ parent t of
                                                CBlock    -> dcloBlock a t
                                                CConsIts  -> envBlock a (parent t)
                                                CRoot     -> dcloBlock a t


dead :: Zipper P -> Bool 
dead t = case constructor t of 
          CDecl -> deadsFromBlock (lexeme t) t 
          _        -> error "checking dead declarations in non-declaration node"

deadsFromBlock :: String -> Zipper P -> Bool
deadsFromBlock s t = case constructor t of
                      CBlock -> deads s (t.$1) 
                      CRoot  -> deads s (t.$1) 
                      _         -> deadsFromBlock s (parent t)

deads :: String -> Zipper P -> Bool 
deads s t = case constructor t of 
                    CNilIts   -> True 
                    CConsIts  -> deads s (t.$1) && deads s (t.$2)
                    CDecl     -> True 
                    CUse      -> s /= lexeme t 
                    CBlock    -> blockContain || deadsFromBlock s t 
                    CRoot     -> blockContain || deadsFromBlock s t 
 where blockContain = not $ null [ name | (name, _, _) <- envBlock [] (t.$1), name==s]



-- --------
-- --
-- - Testing
-- --
-- --------


t1 = mkAG t1_ 
t1_ = Root (ConsIts (Decl "x" [])
           (ConsIts (Use "y" [])
           (ConsIts (Block (ConsIts (Use "y" []) 
                            NilIts))
          NilIts)))