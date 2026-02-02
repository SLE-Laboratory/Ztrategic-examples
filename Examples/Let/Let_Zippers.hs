module Examples.Let.Let_Zippers where 
import Language.ZipperAG
import Data.Generics.Zipper
import Data.Data
import Data.List (sortBy)

import Examples.Let.Shared


-- --------
-- --
-- - Synthesized Attributes
-- --
-- --------

dclo :: Zipper a ->  Env
dclo ag = case (constructor ag) of
           CRoot       -> dclo (ag.$1)
           CLet        -> dclo (ag.$1)
           CNestedLet  -> dclo (ag.$3)
           CAssign     -> dclo (ag.$3)
           CEmptyList  -> dcli ag

errs :: Zipper a -> Errors
errs ag = case (constructor ag) of
           CRoot      -> errs (ag.$1)
           CLet       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CAdd       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CSub       -> (errs (ag.$1)) ++ (errs (ag.$2))
           CNeg       -> errs (ag.$1)
           CEmptyList -> []
           CConst     -> []
           CVar       -> mBIn (lexeme_Var ag) (env ag)
           CAssign    -> mNBIn (lexeme_Assign ag,lev ag) (dcli ag) 
                            ++ (errs (ag.$2)) ++ (errs (ag.$3))
           CNestedLet -> mNBIn (lexeme_NestedLet ag,lev ag) (dcli ag) 
                            ++ (errs (ag.$2)) ++ (errs (ag.$3))

-- --------
-- --
-- - Inheritted Attributes
-- --
-- --------

dcli :: Zipper a -> Env
dcli ag = case (constructor ag) of
           CLet  -> case (constructor (parent ag)) of
                         CRoot      -> []
                         CNestedLet -> env  (parent ag)
           _     -> case (constructor (parent ag)) of
                         CLet       -> dcli (parent ag)
                         CAssign    -> (lexeme_Assign (parent ag), lev (parent ag), Just $ lexeme_Assign_2 $ parent ag) : (dcli (parent ag))
                         CNestedLet -> (lexeme_NestedLet (parent ag), lev (parent ag), Nothing) : (dcli (parent ag))
                         _          -> dcli (parent ag)



env :: Zipper a -> Env
env ag = case (constructor ag) of
          CRoot -> dclo ag
          CLet  -> dclo ag
          _     -> env (parent ag)

env' :: Zipper a -> Env
env' ag = inherit isNest dclo ag 
 where isNest (Let _ _) = True

lev :: Zipper a -> Int
lev ag = case (constructor ag) of
           CLet  -> case (constructor (parent ag)) of
                       CNestedLet -> (lev (parent ag)) + 1
                       CRoot      -> 0
           _     -> lev (parent ag)


-- --------
-- --
-- - Environment lookup functions
-- --
-- --------

mBIn :: Name -> Env -> Errors
mBIn name [] = [name]
mBIn name ((n,i,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Int) -> Env -> Errors
mNBIn t []                   = [] 
mNBIn (n1,l1) ((n2,l2,_):es) = if (n1==n2) && (l1 == l2)
                               then [n1] else mNBIn (n1,l1) es

{-
expand :: (Name,Int) -> Env -> Maybe Exp
expand (n, -1) _ = Nothing 
expand (n, l) e = case expand' (n,l) e of 
      Nothing -> expand (n, l-1) e
      r -> r 
 where expand' (n, l) [] = Nothing 
       expand' (n, l) ((n2,l2,e2):es) = if n==n2 && l==l2 then e2 else expand' (n,l) es
-}

expand :: (Name,Int) -> Env -> Maybe Exp
expand (i, l) e = case level of 
      ((nE, lE, Just (Const c)):_) -> Just (Const c) 
      _ -> Nothing 
 where vars = filter (\(nE, lE, _) -> nE == i && lE <= l) e
       level = sortBy (\(nE1, lE1, _) (nE2, lE2, _) -> compare lE2 lE1) vars
                    

semantics :: Root -> [String]
semantics p = errs (mkAG p)




-- --------
-- --
-- - Same but only for current block! 
-- --
-- --------

dcloBlock :: Zipper a ->  EnvBlock
dcloBlock ag = case (constructor ag) of
           CRoot       -> dcloBlock (ag.$1)
           CLet        -> dcloBlock (ag.$1)
           CNestedLet  -> dcloBlock (ag.$3)
           CAssign     -> dcloBlock (ag.$3)
           CEmptyList  -> dcliBlock ag

dcliBlock :: Zipper a -> EnvBlock
dcliBlock ag = case (constructor ag) of
           CLet  -> case (constructor (parent ag)) of
                         CRoot      -> []
                         -- we change this - no inherited from parent blocks!
                         CNestedLet -> [] -- env  (parent ag)
           _     -> case (constructor (parent ag)) of
                         CLet       -> dcliBlock (parent ag)
                         CAssign    -> (lexeme_Assign (parent ag), Just $ lexeme_Assign_2 $ parent ag) : (dcliBlock (parent ag))
                         CNestedLet -> (lexeme_NestedLet (parent ag), Nothing) : (dcliBlock (parent ag))

envBlock :: Zipper a -> EnvBlock
envBlock ag = case (constructor ag) of
          CRoot -> dcloBlock ag
          CLet  -> dcloBlock ag
          _     -> envBlock (parent ag)

-- --------
-- --
-- - Same but only for current block, simpler, assumes we're at start of let
-- --
-- --------

dclBlock :: Zipper a ->  EnvBlock
dclBlock ag = case (constructor ag) of
           CLet        -> dclBlock (ag.$1)
           CNestedLet  -> (lexeme_NestedLet ag, Nothing)       : dclBlock (ag.$3)
           CAssign     -> (lexeme_Assign ag, Just $ lexeme_Assign_2 ag) : dclBlock (ag.$3)
           CEmptyList  -> []


-- --------
-- --
-- - Calculate the value of a Let. Code taken partially from Pedro Martins' work.
-- --
-- --------

calculate :: Zipper a -> Int
calculate ag = case (constructor ag) of
                 CRoot       -> calculate $ ag.$1
                 CLet        -> calculate $ ag.$2
                 CAdd        -> (calculate $ ag.$1) + (calculate $ ag.$2)
                 CSub        -> (calculate $ ag.$1) - (calculate $ ag.$2)
                 CNeg        -> negate $ calculate $ ag.$1 
                 CVar        -> getVarValue (lexeme_Var ag) ag
                 CConst      -> lexeme_Const ag

-- ----- AUX's -------
getVarValue :: String -> Zipper a -> Int
getVarValue name ag = case (constructor ag) of
                       CRoot      -> auxGetVarValue name ag
                       CLet -> auxGetVarValue name (ag.$1)
                       _          -> getVarValue name (parent ag)

auxGetVarValue :: String -> Zipper a -> Int
auxGetVarValue name ag = case (constructor ag) of
                          CRoot       -> auxGetVarValue name (ag.$1)
                          CLet       -> auxGetVarValue name (ag.$1)
                          CAssign -> if (lexeme_Assign ag == name) then calculate (ag.$2)
                                           else (auxGetVarValue name (ag.$3))
                          CNestedLet    -> if (lexeme_NestedLet ag == name) then calculate (ag.$2)
                                           else (auxGetVarValue name (ag.$3))
                          CEmptyList  -> oneUpGetVarValue name ag

oneUpGetVarValue :: String -> Zipper a -> Int
oneUpGetVarValue name ag = case (constructor ag) of
                       CLet -> getVarValue name (parent ag)
                       _          -> oneUpGetVarValue name (parent ag)


-- --------
-- --
-- - Compute only errors due to uses of undeclared variables
-- --
-- --------

errs_uses :: Zipper a -> Errors
errs_uses ag = case constructor ag of
           CRoot      -> errs_uses (ag.$1)
           CLet       -> errs_uses (ag.$1) ++ errs_uses (ag.$2)
           CAdd       -> errs_uses (ag.$1) ++ errs_uses (ag.$2)
           CSub       -> errs_uses (ag.$1) ++ errs_uses (ag.$2)
           CNeg       -> errs_uses (ag.$1)
           CEmptyList -> []
           CConst     -> []
           CVar       -> mBIn (lexeme_Var ag) (env ag)
           CAssign    -> errs_uses (ag.$2) ++ errs_uses (ag.$3)
           CNestedLet -> errs_uses (ag.$2) ++ errs_uses (ag.$3)