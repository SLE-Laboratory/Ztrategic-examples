{-#LANGUAGE TemplateHaskell#-}
module Examples.Let.LetFailProperty where

import Language.Ztrategic
import Data.Data (Typeable, Data)
import Language.StrategicData (StrategicData)

import Test.QuickCheck
import Data.Generics.Zipper
import Data.List (intersect)
import Data.Maybe (fromJust)


import Examples.Let.Shared
import Examples.Let.Let_Zippers
import Examples.Let.LetZtrategic
import Examples.Let.LetAG (circ)
import Language.ZipperAG
import Examples.Let.LetFaultyGenerator

forallNodes :: (Typeable a, Data b, StrategicData b) => (a -> Zipper b -> [Property]) -> b -> Property
forallNodes p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     props = applyTU (full_tdTU step) astZipper
    in conjoin props


existsNode :: (Typeable a, Data b, StrategicData b) => (a -> Zipper b -> [Property]) -> b -> Property
existsNode p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     props = applyTU (full_tdTU step) astZipper
    in disjoin props

-- --------
-- --
-- - Properties over said Let expressions
-- --
-- --------

-- ensure that the generator is not producing invalid Let expressions
prop_errors :: Root -> Property
prop_errors t = counterexample ("Errs: " ++ show (errs (toZipper t))) $ 
                within 1000000 $ property (null (errs (toZipper t)))


-- all vars must be in environment
prop_nameInEnv :: Root -> Property
prop_nameInEnv t = forallNodes oneNameInEnv t 

oneNameInEnv :: Exp -> Zipper Root -> [Property]
oneNameInEnv (Var v) z = [property (v `varIn` env z)]
oneNameInEnv _       _ = []

varIn :: String -> Env -> Bool 
varIn v e = null (v `mBIn` e)

-- checks if the local environment of a block is contained in the global environment
prop_localInGlobal :: Root -> Property
prop_localInGlobal t = forallNodes localInGlobal t

localInGlobal :: Let -> Zipper Root -> [Property]
localInGlobal _ z = [property (dclBlock z `isIn` env z)]

isIn :: EnvBlock -> Env -> Bool
isIn db e = let eNoLevels = map (\(a,b,c) -> (a,c)) e
                   in (db `intersect` eNoLevels) == db


-- checks if the original Let as well as 3 different optimization functions applied to it all evaluate to the same value
prop_sameEval :: Root -> Property
prop_sameEval t = within 10000000 $ 
                  let eval = calculate . toZipper
                      r1 = eval t                                               -- original 
                      r2 = eval (circ t)                                        -- opt HOAG
                      r3 = eval (fromZipper $ fromJust $ opt''    $ toZipper t) -- opt Ztrategic
                      r4 = eval (fromZipper $ fromJust $ optBlock $ toZipper t) -- opt upwards + block-level decls

                   --   in verbose $ counterexample ("Let is: " ++ show t ++ "\r1 is " ++ (show r1) ++ "\nr2 is " ++ (show r2) ++ "\nr3 is "++ (show r3) ++ "\nr4 is " ++ (show r4))
                   in property
                   $ r1 == r2 && r1 == r3 && r1 == r4
                   -- $ all (==r1) [r2, r3, r4]
                   -- $ all (==r1) [r1]  )

-- will fail if any test case takes more than 3 seconds
timedprop_sameEval = within 3000000 prop_sameEval


-- --------
-- --
-- - Properties over said Let expressions, pt. 2
-- --
-- --------



{-
-- if our generator could generate errors, then it would make more sense to say
oneNameInEnv :: Exp -> Zipper Root -> [Property]
oneNameInEnv (Var v) z = null (errs z)
                            ==> 
                         let listErrors = v `mBIn` env z
                         in return $ property $ null listErrors
oneNameInEnv _       _ = []
-}

-- alternatively, if we find a var not in the environment, then the attribute "errs" must report it

prop_errFound :: Root -> Property 
prop_errFound t = forallNodes errFound t

errFound :: Exp -> Zipper Root -> [Property]
errFound (Var v) z = [not (v `varIn` env z)
                  ==> not (null (errs z))]
errFound _       _ = []

-- if dcli == [] and errs == [], then name is in dclBlock
prop_localNames :: Root -> Property
prop_localNames t = forallNodes localNames t

localNames :: Exp -> Zipper Root -> [Property]
localNames (Var v) z = let l = nest z in 
        [null (dcli l) && null (errs l)
        ==> v `varIn'` dclBlock l]
localNames _       _ = []


-- aux functions to declutter code of localNames
nest z = inherit isNest id z
 where isNest (Let _ _) = True
varIn' v l = v `elem` map fst l

-- to be able to run all tests at once. Note that they must start with "prop_".
return []
runTests = $quickCheckAll