module Examples.Let.LetZtrategic where 

import Data.Generics.Zipper
import Data.Maybe (fromJust)

import Language.Ztrategic
import Language.StrategicData

import Examples.Let.Shared 
-- import Examples.Let.SharedAG
import Examples.Let.Let_Zippers

import Control.Monad (MonadPlus, mzero)

import Debug.Trace (trace)

import Data.Generics.Aliases

----------
----
--- Example defined in paper
----
----------

{-
p = let a = b + 0
        c = 2
        b = let c = 3 in c + c
    in a + 7 - c
-}

p :: Let
p =  Let  (Assign     "a"  (Add (Var "b") (Const 0))
          (Assign     "c"  (Const 2)
          (NestedLet  "b"  (Let  (Assign "c" (Const 3)
                                 EmptyList)
                                 (Add (Var "c") (Var "c")))
          EmptyList)))
          (Sub (Add (Var "a") (Const 7)) (Var "c"))

----------
----
--- Optimization via rules 1 through 6
----
----------      

expr :: Exp -> Maybe Exp
expr (Add e (Const 0))          = Just e 
expr (Add (Const 0) t)          = Just t  
expr (Add (Const a) (Const b))  = Just (Const (a+b)) 
expr (Sub a b)                  = Just (Add a (Neg b))    
expr (Neg (Neg f))              = Just f           
expr (Neg (Const n))            = Just (Const (-n)) 
expr _                          = Nothing

opt'  :: Zipper Let -> Maybe (Zipper Let)
opt'  t =  applyTP (innermost step) t
      where step = failTP `adhocTP` expr


t_1 = toZipper p 
run_example_TP = fromZipper $ fromJust $ opt' t_1



-- --------
-- --
-- - Optimization via all 7 rules
-- --
-- --------

expC :: Exp -> Zipper Root -> Maybe Exp
expC (Add e (Const 0))        _ = Just e 
expC (Add (Const 0) t)        _ = Just t  
expC (Add (Const a) (Const b))_ = Just (Const (a+b)) 
expC (Sub a b)                _ = Just (Add a (Neg b))    
expC (Neg (Neg f))            _ = Just f           
expC (Neg (Const n))          _ = Just (Const (-n)) 
expC (Var i) z = expand (i, lev z) (env z)
expC _ z = Nothing 

opt'' :: Zipper Root -> Maybe (Zipper Root)
opt'' r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expC

run_example_opt_t_1 = fromZipper $ fromJust $ opt'' $ toZipper $ Root p 


-- --------
-- --
-- - Testing adhocTPSeq
-- --
-- --------

expC_1 :: Exp -> Zipper Root -> Maybe Exp
expC_1 (Add e (Const 0))        _ = Just e 
expC_1 (Add (Const 0) t)        _ = Just t  
expC_1 (Add (Const a) (Const b))_ = Just (Const (a+b)) 
expC_1 (Sub a b)                _ = Just (Add a (Neg b))    
expC_1 (Neg (Neg f))            _ = Just f           
expC_1 (Neg (Const n))          _ = Just (Const (-n)) 
expC_1 _ z = Nothing 

expC_2 :: Exp -> Zipper Root -> Maybe Exp
expC_2 (Var i) z = expand (i, lev z) (env' z)
expC_2 _ z = Nothing 

opt''_1 :: Zipper Root -> Maybe (Zipper Root)
opt''_1 r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expC_1 `adhocTPZ` expC_2

opt''_2 :: Zipper Root -> Maybe (Zipper Root)
opt''_2 r = applyTP (innermost step) r
 where step = failTP `adhocTPZSeq` expC_2 `adhocTPZSeq` expC_1 


expC_X :: Exp -> Zipper Root -> Maybe Exp
expC_X (Add e (Const 0))        _ = Just e 
expC_X _ z = Nothing 

expC_Y :: Exp -> Zipper Root -> Maybe Exp
--expC_Y (Add e (Const 0))        _ = Just e 
expC_Y (Const 2)_ = Just (Const 17) 
expC_Y _ z = Nothing 

opt''_3 :: Zipper Root -> Maybe (Zipper Root)
opt''_3 r = applyTP (innermost step) r
 where step = failTP `adhocTPZSeq` expC_X `adhocTPZSeq` expC_Y


run_example_seq_1 = fromZipper $ fromJust $ opt''_1 $ toZipper $ Root p 
run_example_seq_2 = fromZipper $ fromJust $ opt''_2 $ toZipper $ Root p 
run_example_seq_3 = fromZipper $ fromJust $ opt''_3 $ toZipper $ Root p 


-- --------
-- --
-- - Testing mutations
-- --
-- --------


exp_transformation :: (MonadPlus m) => Exp -> m Exp
exp_transformation (Const n) = return (Const 100000)
exp_transformation _ = mzero

mut = fmap fromZipper $ flip once_RandomTP exp_transformation $ toZipper $ Root p

-- --------
-- --
-- - Testing upwards traversals
-- --
-- --------
definesVarTrace :: String -> Let -> Zipper Root -> Maybe [Exp]
definesVarTrace i _ z = trace (show (envBlock z)) $ case (lookup i $ envBlock z) of 
                        Just (Just e) -> Just [e]
                        _ -> Nothing

expC_2Up :: Exp -> Zipper Root -> Maybe Exp
expC_2Up (Var i) z = case lookupVar i z of 
                      Just [e] -> Just e
                      _        -> Nothing
expC_2Up _ z = Nothing 

lookupVar :: String -> Zipper Root -> Maybe [Exp]
lookupVar i z = applyTU (once_upbuTU step) z
 where step = failTU `adhocTUZ` (definesVar i)  

definesVar :: String -> Let -> Zipper Root -> Maybe [Exp]
definesVar i _ z = case lookup i (dclBlock z) of 
                     Just (Just (Const c)) -> Just [Const c]
                     Just _                -> Just []
                     _                     -> Nothing
{-
definesVar' :: String -> Let -> Zipper Root -> Maybe [Exp]
definesVar' i _ z = case lookup i (envBlock z) of 
                     Just (Just (Const c)) -> Just [Const c]
                     Just _                -> Just []
                     _                     -> Nothing
-}

optBlock :: Zipper Root -> Maybe (Zipper Root)
optBlock r = applyTP (innermost step) r
 where step = failTP `adhocTPZSeq` expC_1 `adhocTPZSeq` expC_2Up

run_example_block_1 = fromZipper $ fromJust $ optBlock $ toZipper $ Root p 
run_example_block_2 = fromZipper $ fromJust $ optBlock $ toZipper $ Root pblock
run_example_block_3 = fromZipper $ fromJust $ optBlock $ toZipper $ Root pblock2

pblock :: Let
pblock =  Let  (Assign     "a"  (Const 1)
               (Assign     "b"  (Const 2)
               (NestedLet  "c"  (Let  (Assign "a" (Const 3)
                                      (Assign "b" (Var "d")
                                      EmptyList))
                                      (Var "b"))
               (Assign     "d"  (Const 4)
               EmptyList))))
               (Var "c")

pblock2 :: Let
pblock2 =  Let  (Assign     "a"  (Const 1)
                (Assign     "b"  (Const 2)
                (NestedLet  "c"  (Let  (Assign     "a" (Const 3)
                                       (Assign     "b" (Const 4)
                                       (NestedLet  "c"  (Let  (Assign "a" (Const 5)
                                                              (Assign "b" (Var "d")
                                                              EmptyList))
                                                        (Var "b"))
                                        EmptyList)))
                                       (Var "b"))
                (Assign     "d"  (Const 4)
                EmptyList))))
                (Var "c")



-- --------
-- --
-- - Testing that expand works fine
-- --
-- --------

run_example_expand = fromZipper $ fromJust $ opt''_2 $ toZipper $ Root pExpand

{-
let a = 1 
    b = 2 
    c = a + b
    d = let e = c
        in e 
    in d
-}

pExpand :: Let
pExpand = Let (Assign     "a"  (Const 1)
              (Assign     "b"  (Const 2)
              (Assign     "c"  (Add (Var "a") (Var "b"))
              (NestedLet  "d"  (Let  (Assign "e" (Var "c")
                                     (Assign "a"  (Const 4) 
                                      EmptyList))
                                (Var "e"))
              EmptyList))))
              (Var "d")


-- --------
-- --
-- - Testing upwards + downwards for no attributes
-- --
-- --------

optBlockS :: Zipper Root -> Maybe (Zipper Root)
optBlockS r = applyTP (innermost step) r
 where step = failTP `adhocTPZSeq` expC_1 `adhocTPZSeq` expC''

-- try to find definition of variable i
expC'' :: Exp -> Zipper Root -> Maybe Exp
expC'' (Var i) z = case goToLetRoot i z of 
                      Just [e] -> Just e
                      _        -> Nothing
expC'' _ z = Nothing 

-- traverse to let root
goToLetRoot :: String -> Zipper Root -> Maybe [Exp]
goToLetRoot i z = applyTU (once_upbuTU step) z
 where step = failTU `adhocTUZ` (fromLetRoot i)  

-- matches a Let node, so it's a let root. Now we search assigns in this block
-- (and unfortunately, on blocks below because we cant stop)
fromLetRoot :: String -> Let -> Zipper Root -> Maybe [Exp]
fromLetRoot i _ z = applyTU (once_tdTU step) z
 where step = failTU `adhocTU` (findVar i)  

-- checks one assign
findVar :: String -> List -> Maybe [Exp]
findVar s (Assign n e l) = case (s==n, e) of 
    (True, Const c) -> Just [Const c] 
    (True, _)       -> Just []
    _               -> Nothing
findVar s _ = Nothing

run_example_blockS_1 = fromZipper $ fromJust $ optBlockS $ toZipper $ Root p
run_example_blockS_2 = fromZipper $ fromJust $ optBlockS $ toZipper $ Root pblock
run_example_blockS_3 = fromZipper $ fromJust $ optBlockS $ toZipper $ Root pblock2
run_example_blockS_4 = fromZipper $ fromJust $ optBlockS $ toZipper $ Root pExpand
run_example_blockS_5 = fromZipper $ fromJust $ optBlockS $ toZipper $ Root pNest

pNest = Let (Assign     "a"  (Var "b")
            (NestedLet  "x"  (Let  (Assign "b" (Const 1)
                                    EmptyList)
                             (Var "b"))
            (Assign     "b"  (Const 0)
            EmptyList)))
            (Var "x") 



-- --------
-- --
-- - Testing "mutations" function
-- --
-- --------

-- it works with 1 function. 
-- extM doesnt work here. We will have to rethink "mutations"
muts = mutations pNest $ replaceNumber `extM` replaceString

replaceNumber :: Int -> Maybe Int 
replaceNumber n = Just (n+1)
replaceString :: String -> Maybe String
replaceString n = Just ("Undefined")

-- --------
-- --
-- - Definition of terminal symbols
-- --
-- --------

instance StrategicData Let
instance StrategicData Root

-- comment above and uncomment below to enable skipping terminal symbols 
{-
instance StrategicData Let where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )
              
instance StrategicData Root where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )
-}

-- for benchmarking

expCB :: Exp -> Zipper Root -> Maybe Exp
expCB (Add e (Const 0))        _ = Just e 
expCB (Add (Const 0) t)        _ = Just t  
expCB (Add (Const a) (Const b))_ = Just (Const (a+b)) 
expCB (Sub a b)                _ = Just (Add a (Neg b))    
expCB (Neg (Neg f))            _ = Just f           
expCB (Neg (Const n))          _ = Just (Const (-n)) 
expCB (Var i) z = case lookupVar i z of 
                      Just [e] -> Just e
                      _        -> Nothing
expCB _ z = Nothing 

opt''_local :: Zipper Root -> Maybe (Zipper Root)
opt''_local r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expCB

-- only one worker function
opt = fromZipper . fromJust . opt'' . toZipper
optLocal = fromZipper . fromJust . opt''_local . toZipper

-- two worker functions
optAdhoc      = fromZipper . fromJust . opt''_2 . toZipper
optAdhocLocal = fromZipper . fromJust . optBlock . toZipper

