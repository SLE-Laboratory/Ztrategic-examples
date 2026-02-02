{-# LANGUAGE GADTs,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances, 
             DeriveDataTypeable
#-}

module Examples.Let.LetMemo where

import Data.Generics.Zipper
import Data.Generics.Aliases
import Data.Data
import Language.ZipperAG
import Data.Maybe (fromJust)

import qualified Examples.Let.Shared as Shared
import Examples.Let.Shared (Name, Env, EnvBlock, Errors)

import Language.Memo.AGMemo
import Language.StrategicData
import Language.Ztrategic

----------
----
--- Memoized data type and relevant functions
----
----------

data Let m = Root  (Let m)         m 
           | Let   (Let m) (Let m) m
           | NestedLet  Name (Let m) (Let m) m
           | Assign     Name (Let m) (Let m) m
           | EmptyList             m
           | Add   (Let m) (Let m) m
           | Sub   (Let m) (Let m) m
           | Neg   (Let m)         m
           | Var   Name            m
           | Const Int             m
       deriving (Data, Typeable)

letToRoot :: (Let m) -> Shared.Root
letToRoot (Root l _ ) = Shared.Root (letToLet l)

letToLet :: (Let m) -> Shared.Let 
letToLet (Let l1 l2 _) = Shared.Let (letToList l1) (letToExp l2)

letToList :: (Let m) -> Shared.List 
letToList (NestedLet  n l1 l2 _) = Shared.NestedLet n (letToLet l1) (letToList l2)
letToList (Assign     n l1 l2 _) = Shared.Assign    n (letToExp l1) (letToList l2)
letToList (EmptyList m) = Shared.EmptyList


letToExp :: (Let m) -> Shared.Exp
letToExp (Add e1 e2 _) = Shared.Add (letToExp e1) (letToExp e2)
letToExp (Sub e1 e2 _) = Shared.Sub (letToExp e1) (letToExp e2)
letToExp (Neg e1    _) = Shared.Neg (letToExp e1)
letToExp (Var v     _) = Shared.Var v
letToExp (Const i   _) = Shared.Const i

buildMemoTree :: m -> Shared.Root -> Let m
buildMemoTree m (Shared.Root r) = Root (buildMemoTreeLet m r) m

buildMemoTreeLet :: m -> Shared.Let -> Let m
buildMemoTreeLet m (Shared.Let l e) = Let (buildMemoTreeList m l) (buildMemoTreeExp m e) m 

buildMemoTreeList :: m -> Shared.List -> Let m
buildMemoTreeList m (Shared.NestedLet n l li) = NestedLet n (buildMemoTreeLet m l) (buildMemoTreeList m li) m
buildMemoTreeList m (Shared.Assign    n e li) = Assign    n (buildMemoTreeExp m e) (buildMemoTreeList m li) m
buildMemoTreeList m (Shared.EmptyList) = EmptyList m

buildMemoTreeExp :: m -> Shared.Exp -> Let m
buildMemoTreeExp m (Shared.Add e1 e2) = Add (buildMemoTreeExp m e1) (buildMemoTreeExp m e2) m
buildMemoTreeExp m (Shared.Sub e1 e2) = Sub (buildMemoTreeExp m e1) (buildMemoTreeExp m e2) m
buildMemoTreeExp m (Shared.Neg e1   ) = Neg (buildMemoTreeExp m e1) m
buildMemoTreeExp m (Shared.Var v    ) = Var v m
buildMemoTreeExp m (Shared.Const i  ) = Const i m


updMemoTable' :: (m -> m) -> Let m -> Let m
updMemoTable' f (Root  l     m) = Root  l     (f m)
updMemoTable' f (Let   l1 l2 m) = Let   l1 l2 (f m)
updMemoTable' f (NestedLet n l1 l2 m) = NestedLet n l1 l2 (f m)
updMemoTable' f (Assign    n l1 l2 m) = Assign    n l1 l2 (f m)
updMemoTable' f (EmptyList   m) = EmptyList   (f m)
updMemoTable' f (Add   l1 l2 m) = Add   l1 l2 (f m)
updMemoTable' f (Sub   l1 l2 m) = Sub   l1 l2 (f m)
updMemoTable' f (Neg   l     m) = Neg   l     (f m)
updMemoTable' f (Var   n     m) = Var   n     (f m)
updMemoTable' f (Const i     m) = Const i     (f m)

getMemoTable' :: Let m -> m
getMemoTable' (Root  _     m) = m
getMemoTable' (Let   _ _   m) = m
getMemoTable' (NestedLet _ _ _ m) = m
getMemoTable' (Assign    _ _ _ m) = m
getMemoTable' (EmptyList   m) = m
getMemoTable' (Add   _ _   m) = m
getMemoTable' (Sub   _ _   m) = m
getMemoTable' (Neg   _     m) = m
getMemoTable' (Var   _     m) = m
getMemoTable' (Const _     m) = m

cleanMemoTable' :: Zipper (Let MemoTable) -> Zipper (Let MemoTable)
cleanMemoTable' z = fromJust $ applyTP (full_tdTP step) z
 where step = idTP `adhocTP` aux 
       aux :: Let MemoTable -> Maybe (Let MemoTable)
       aux l = Just $ updMemoTable' (const emptyMemo) l
----------
----
--- AG Boilerplate 
----
----------

data Constructor = CRoot 
                 | CLet 
                 | CNestedLet
                 | CAssign
                 | CEmptyList            
                 | CAdd 
                 | CSub
                 | CNeg
                 | CVar  
                 | CConst
                  deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case (getHole a :: Maybe (Let MemoTable)) of
                  Just (Root  _     m) -> CRoot
                  Just (Let   _ _   m) -> CLet
                  Just (NestedLet _ _ _ m) -> CNestedLet
                  Just (Assign    _ _ _ m) -> CAssign
                  Just (EmptyList   m) -> CEmptyList
                  Just (Add   _ _   m) -> CAdd
                  Just (Sub   _ _   m) -> CSub
                  Just (Neg   _     m) -> CNeg
                  Just (Var   _     m) -> CVar
                  Just (Const _     m) -> CConst
                  _                    -> error "Naha, that production does not exist!"

lexeme_Name :: Zipper a -> Name
lexeme_Name ag = case (getHole ag :: Maybe (Let MemoTable)) of
                  Just (Assign    v _ _ _) -> v
                  Just (NestedLet v _ _ _) -> v
                  Just (Var       v _    ) -> v
                  _ -> error "Error in lexeme_Name!"

lexeme_Exp :: Zipper a -> Maybe Shared.Exp
lexeme_Exp ag = case (getHole ag :: Maybe (Let MemoTable)) of
                  Just (Assign _ e _ _) -> Just $ letToExp e
                  -- _                     -> Nothing

mBIn :: Name -> Env -> Errors
mBIn name [] = [name]
mBIn name ((n,i,l):es) = if (n==name) then [] else mBIn name es

mNBIn :: (String, Int) -> Env -> Errors
mNBIn t []                   = [] 
mNBIn (n1,l1) ((n2,l2,_):es) = if (n1==n2) && (l1 == l2)
                               then [n1] else mNBIn (n1,l1) es


-- --------
-- --
-- - Definition of attributes, Memo Table and relevant instances 
-- --
-- --------

data Att_DCLI = Att_DCLI
data Att_DCLBLOCK = Att_DCLBLOCK
data Att_DCLO  = Att_DCLO
data Att_Lev = Att_Lev
data Att_Env = Att_Env
data Att_Errs = Att_Errs 
data Att_Adds = Att_Adds

type MemoTable = ( Maybe Env      -- dcli
                 , Maybe EnvBlock -- dclBlock
                 , Maybe Env      -- dclo
                 , Maybe Int      -- lev
                 , Maybe Env      -- Env
                 , Maybe Errors   -- Errs
                 , Maybe Int      -- Adds
                 )
                 
emptyMemo = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing)

instance Memo Att_DCLI MemoTable Env where
  mlookup _   (a,_,_,_,_,_,_) = a
  massign _ v (a,b,c,d,e,f,g) = (Just v,b,c,d,e,f,g)

instance Memo Att_DCLBLOCK MemoTable EnvBlock where
  mlookup _   (_,b,_,_,_,_,_) = b
  massign _ v (a,b,c,d,e,f,g) = (a,Just v,c,d,e,f,g)

instance Memo Att_DCLO MemoTable Env where
  mlookup _   (_,_,c,_,_,_,_) = c
  massign _ v (a,b,c,d,e,f,g) = (a,b,Just v,d,e,f,g)

instance Memo Att_Lev MemoTable Int where
  mlookup _   (_,_,_,d,_,_,_) = d
  massign _ v (a,b,c,d,e,f,g) = (a,b,c,Just v,e,f,g)

instance Memo Att_Env MemoTable Env where
  mlookup _   (_,_,_,_,e,_,_) = e
  massign _ v (a,b,c,d,e,f,g) = (a,b,c,d,Just v,f,g)

instance Memo Att_Errs MemoTable Errors where
  mlookup _   (_,_,_,_,_,f,_) = f
  massign _ v (a,b,c,d,e,f,g) = (a,b,c,d,e,Just v,g)

instance Memo Att_Adds MemoTable Int where
  mlookup _   (_,_,_,_,_,_,g) = g
  massign _ v (a,b,c,d,e,f,g) = (a,b,c,d,e,f,Just v)

instance Memoizable Let MemoTable where 
  updMemoTable = updMemoTable'
  getMemoTable = getMemoTable'
  cleanMemoTable = cleanMemoTable' 

instance StrategicData (Let MemoTable) where 
  isTerminal z = isJust (getHole z :: Maybe MemoTable)
              || isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )

----------
----
--- Definition of Memoized Attributes
----
----------

dclo :: (Memo Att_DCLO MemoTable Env) => AGTree_m Let MemoTable Env 
dclo = memo Att_DCLO $ \ag -> case (constructor ag) of
           CRoot       -> dclo .@. (ag.$1)
           CLet        -> dclo .@. (ag.$1)
           CNestedLet  -> dclo .@. (ag.$3)
           CAssign     -> dclo .@. (ag.$3)
           CEmptyList  -> dcli ag


dcli :: (Memo Att_DCLI MemoTable Env) => AGTree_m Let MemoTable Env 
dcli = memo Att_DCLI $ \ag -> case (constructor ag) of
           CLet  -> case (constructor (parent ag)) of
                         CRoot      -> ([], ag)
                         CNestedLet -> env  `atParent` ag
           _     -> case (constructor (parent ag)) of
                         CLet       -> dcli `atParent` ag
                         CAssign    -> let (levP, ag')   = lev `atParent` ag
                                           (dcliP, ag'') = dcli `atParent` ag'
                                           assignName    = lexeme_Name (parent ag'')
                                           assignExp     = lexeme_Exp  (parent ag'')
                                       in ((assignName, levP, assignExp) : dcliP, ag'')
                         CNestedLet -> let (levP, ag')   = lev `atParent` ag
                                           (dcliP, ag'') = dcli `atParent` ag'
                                           assignName    = lexeme_Name (parent ag'')
                                       in ((assignName, levP, Nothing) : dcliP, ag'')

dclBlock :: (Memo Att_DCLBLOCK MemoTable EnvBlock) => AGTree_m Let MemoTable EnvBlock
dclBlock = memo Att_DCLBLOCK $ \ag -> case (constructor ag) of
             CLet       -> dclBlock .@. (ag.$1)
             CNestedLet -> let (rest, ag') = dclBlock .@. (ag.$3)
                               assignName  = lexeme_Name ag'
                           in ((assignName, Nothing) : rest, ag')
             CAssign    -> let (rest, ag') = dclBlock .@. (ag.$3)
                               assignName  = lexeme_Name ag'
                               assignExp   = lexeme_Exp ag'
                           in ((assignName, assignExp) : rest, ag')
             CEmptyList -> ([], ag)
             v -> error $ show v 

lev :: (Memo Att_Lev MemoTable Int) => AGTree_m Let MemoTable Int 
lev = memo Att_Lev $ \ag -> case (constructor ag) of
           CLet  -> case (constructor $ parent ag) of
                       CNestedLet -> let (r, ag') = lev `atParent` ag
                                     in (r + 1, ag')
                       CRoot      -> (0, ag)
           _     -> lev `atParent` ag

env :: (Memo Att_Env MemoTable Env) => AGTree_m Let MemoTable Env 
env = memo Att_Env $ \ag -> case (constructor ag) of
          CRoot -> dclo ag
          CLet  -> dclo ag
          _     -> env `atParent` ag

errs :: (Memo Att_Errs MemoTable Errors) => AGTree_m Let MemoTable Errors
errs = memo Att_Errs $ \ag -> case (constructor ag) of
           CRoot      -> errs .@. (ag.$1)
           CLet       -> let (r1, ag') =   errs .@. (ag.$1)
                             (r2, ag'') =  errs .@. (ag'.$2)
                         in (r1 ++ r2, ag'')
           CAdd       -> let (r1, ag') =   errs .@. (ag.$1)
                             (r2, ag'') =  errs .@. (ag'.$2)
                         in (r1 ++ r2, ag'')
           CSub       -> let (r1, ag') =   errs .@. (ag.$1)
                             (r2, ag'') =  errs .@. (ag'.$2)
                         in (r1 ++ r2, ag'')
           CNeg       -> errs .@. (ag.$1)
           CEmptyList -> ([], ag)
           CConst     -> ([], ag)
           CVar       -> let (r1, ag') = env ag 
                         in (mBIn (lexeme_Name ag) r1, ag')
           CAssign    -> let (dcliHere, ag'  )  = dcli ag
                             (levHere, ag'')    = lev ag'
                             errHere            = mNBIn (lexeme_Name ag, levHere) dcliHere
                             (errDown1, ag''' ) = errs .@. (ag''.$2)
                             (errDown2, ag'''') = errs .@. (ag'''.$3)
                         in (errHere ++ errDown1 ++ errDown2, ag'''') 
           CNestedLet -> let (dcliHere, ag'   ) = dcli ag
                             (levHere, ag''   ) = lev ag'
                             errHere            = mNBIn (lexeme_Name ag, levHere) dcliHere
                             (errDown1, ag''' ) = errs .@. (ag''.$2)
                             (errDown2, ag'''') = errs .@. (ag'''.$3)
                         in (errHere ++ errDown1 ++ errDown2, ag'''') 

adds :: (Memo Att_Adds MemoTable Int) => AGTree_m Let MemoTable Int
adds = memo Att_Adds $ \ag -> case (constructor ag) of
           CRoot      -> adds .@. (ag.$1)
           CLet       -> let (r1, ag') =   adds .@. (ag.$1)
                             (r2, ag'') =  adds .@. (ag'.$2)
                         in (r1 + r2, ag'')
           CAdd       -> let (r1, ag') =   adds .@. (ag.$1)
                             (r2, ag'') =  adds .@. (ag'.$2)
                         in (1 + r1 + r2, ag'')
           CSub       -> let (r1, ag') =   adds .@. (ag.$1)
                             (r2, ag'') =  adds .@. (ag'.$2)
                         in (r1 + r2, ag'')
           CEmptyList -> (0, ag)
           CConst     -> (0, ag)
           CVar       -> (0, ag)
           CAssign    -> let (r1, ag' ) = adds .@. (ag.$2)
                             (r2, ag'') = adds .@. (ag'.$3)
                         in (r1 + r2, ag'') 
           CNestedLet -> let (r1, ag' ) = adds .@. (ag.$2)
                             (r2, ag'') = adds .@. (ag'.$3)
                         in (r1 + r2, ag'') 

-- --------
-- --
-- - Name analysis through attribute errs
-- --
-- --------

errors :: Shared.Root -> Errors
errors t = r
  where (r,_) = errs z
        z :: Zipper (Let MemoTable)
        z = mkAG (buildMemoTree emptyMemo t)

semantics = errors