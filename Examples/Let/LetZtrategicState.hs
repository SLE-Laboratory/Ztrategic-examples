module Examples.Let.LetZtrategicState where 

import Data.Generics.Zipper
import Data.Maybe (fromJust)

import Language.Ztrategic
import Language.StrategicData

import Examples.Let.Shared 
-- import Examples.Let.SharedAG
import Examples.Let.Let_Zippers

import Control.Monad.State.Lazy

----------
----
--- Example defined in paper
----
----------

{-
p = let a = b + 0
        c = 2
        b = let c = 3 in c + c
    in a + 7 − c
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

expr :: (MonadState s m, MonadFail m, Num s) => Exp -> m Exp
expr (Add e (Const 0))          = modify (+1) >> return  e 
expr (Add (Const 0) t)          = modify (+1) >> return t  
expr (Add (Const a) (Const b))  = modify (+1) >> return (Const (a+b)) 
expr (Sub a b)                  = modify (+1) >> return (Add a (Neg b))    
expr (Neg (Neg f))              = return f           
expr (Neg (Const n))            = return (Const (-n)) 
expr _                          = fail ""



opt'  ::( MonadPlus m, MonadState s m, MonadFail m, Num s)
      => Zipper Let -> m (Zipper Let)
opt'  t =  applyTP (innermost step) t
      where step = failTP `adhocTP` expr

type StateMaybe s = StateT s Maybe 

t_1 = toZipper p 
run_example_TP = (\(z,s) -> (fromZipper z,s)) $
         fromJust $ runStateT (opt' t_1) 0


-- --------
-- --
-- - Optimization via all 7 rules
-- --
-- --------

expC :: (MonadState s m, MonadFail m, Num s) => Exp -> Zipper Root -> m Exp
expC (Add e (Const 0))        _ = modify (+1) >> return e 
expC (Add (Const 0) t)        _ = modify (+1) >> return t  
expC (Add (Const a) (Const b))_ = modify (+1) >> return (Const (a+b)) 
expC (Sub a b)                _ = modify (+1) >> return (Add a (Neg b))    
expC (Neg (Neg f))            _ = modify (+1) >> return f           
expC (Neg (Const n))          _ = modify (+1) >> return (Const (-n)) 
expC (Var i) z = modify (+1) >> (maybe (fail "") return $ expand (i, lev z) (env z))
expC _ z = fail ""

opt'' :: (MonadPlus m, MonadState s m, MonadFail m, Num s) => Zipper Root -> m (Zipper Root)
opt'' r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expC

run_example_opt_t_1 = 
  (\(z,s) -> (fromZipper z,s)) $ 
  fromJust $ runStateT (opt'' $ toZipper $ Root p) 0 

-- --------
-- --
-- - Definition of terminal symbols
-- --
-- --------

instance StrategicData Let
instance StrategicData Root
{-
-- comment above and uncomment below to enable skipping terminal symbols 
{-
instance StrategicData Let where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )
              
instance StrategicData Root where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )
-}

opt = fromZipper . fromJust . opt'' . toZipper
-}
