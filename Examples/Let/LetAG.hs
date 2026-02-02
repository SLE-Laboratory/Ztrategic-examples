module Examples.Let.LetAG where 

import Language.ZipperAG
import Data.Generics.Zipper

import Examples.Let.Shared
-- import Examples.Let.SharedAG
import Examples.Let.Let_Zippers

import Data.Function (fix)

----------
----
--- Partial Optimization as an attribute
----
----------

circ :: Root -> Root
circ = fix (\f t -> if t == (optRoot $ mkAG t) then t else f (optRoot $ mkAG t))

optRoot :: Zipper Root -> Root
optRoot ag = case (constructor ag) of
           CRoot      -> Root $ optLet (ag.$1)

optLet :: Zipper Root -> Let
optLet ag = case (constructor ag) of
           CLet       -> Let (optList (ag.$1)) (optExp (ag.$2))

optList :: Zipper Root -> List
optList ag = case (constructor ag) of
           CEmptyList -> EmptyList
           CAssign    -> Assign    (lexeme_Assign    ag) (optExp (ag.$2)) (optList (ag.$3))
           CNestedLet -> NestedLet (lexeme_NestedLet ag) (optLet (ag.$2)) (optList (ag.$3))

optExp :: Zipper Root -> Exp
optExp ag = case (constructor ag) of
           -- rules 1, 2, 3
           CAdd       -> case (lexeme_Add ag, lexeme_Add_2 ag) of 
                           (e, Const 0)       -> e 
                           (Const 0, t)       -> t 
                           (Const a, Const b) -> Const (a+b) 
                           _                  -> Add (optExp (ag.$1)) (optExp (ag.$2))
           -- rule 4                
           CSub       -> Add (lexeme_Sub ag) (Neg (lexeme_Sub_2 ag)) 
           CConst     -> Const (lexeme_Const ag)
           -- rules 5, 6
           CNeg       -> case (lexeme_Neg ag) of 
                           (Neg f)   -> f
                           (Const n) -> Const (-n)
                           _         -> Neg (optExp (ag.$1))
           -- rule 7
           CVar       -> case expand (lexeme_Var ag, lev ag) (env ag) of 
                           Just e  -> e
                           Nothing -> Var (lexeme_Var ag)