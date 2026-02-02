{-#LANGUAGE TemplateHaskell, StandaloneDeriving, FlexibleInstances #-}
module Examples.Let.LetPropertyMemo where

import Language.Memo.Ztrategic
import Language.Memo.AGMemo
import Data.Data (Typeable, Data)
import Language.StrategicData (StrategicData)

import Test.QuickCheck
import Data.Generics.Zipper
import Data.List (intersect)
import Data.Maybe (fromJust)


import Examples.Let.Shared (Env, EnvBlock)
-- import Examples.Let.Let_Zippers
-- import Examples.Let.LetZtrategic
-- import Examples.Let.LetAG (circ)
import Language.ZipperAG
-- import Examples.Let.LetGenerator
import Examples.Let.LetMemo
import Examples.Let.LetGeneratorMemo

forallNodes :: (Typeable a, Data (d mm), Memoizable d mm, StrategicData (d mm)) => (a -> Zipper (d mm) -> ([Property], Zipper (d mm))) -> d mm -> Property
forallNodes p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     (props, _) = applyTU (full_tdTU step) astZipper
    in conjoin props


existsNode :: (Typeable a, Data (d mm), Memoizable d mm, StrategicData (d mm)) => (a -> Zipper (d mm) -> ([Property], Zipper (d mm))) -> d mm -> Property
existsNode p ast = let 
     astZipper = toZipper ast
     step = failTU `adhocTUZ` p
     (props, _) = applyTU (full_tdTU step) astZipper
    in disjoin props

-- --------
-- --
-- - Memoized Let data type is not usually intended to be shown so it does not implement Show
-- - Thus we derive it here
-- --
-- --------

deriving instance Show (Let MemoTable)

-- --------
-- --
-- - Properties over said Let expressions
-- --
-- --------

-- ensure that the generator is not producing invalid Let expressions
prop_errors :: Let MemoTable -> Property
prop_errors t = counterexample ("Errs: " ++ show (fst (errs (toZipper t)))) $ 
                within 1000000 $ property (null (fst (errs (toZipper t))))


-- all vars must be in environment
prop_nameInEnv :: Let MemoTable -> Property
prop_nameInEnv t = forallNodes oneNameInEnv t 

oneNameInEnv :: Let MemoTable -> Zipper (Let MemoTable) -> ([Property], Zipper (Let MemoTable))
oneNameInEnv (Var v _) z = let (computedEnv, z') = env z
                           in  ([property (v `varIn` computedEnv)], z')
oneNameInEnv _       z   = ([], z)

varIn :: String -> Env -> Bool 
varIn v e = null (v `mBIn` e)

-- checks if the local environment of a block is contained in the global environment
prop_localInGlobal :: Let MemoTable -> Property
prop_localInGlobal t = forallNodes localInGlobal t

-- only operate on Let nodes. Cannot type-match so we pattern-match instead
localInGlobal :: Let MemoTable -> Zipper (Let MemoTable) -> ([Property], Zipper (Let MemoTable))
localInGlobal (Let _ _ _) z = let (computedEnv, z') = env z 
                                  (computedDclBlock, z'') = dclBlock z' 
                              in ([property (computedDclBlock `isIn` computedEnv)], z'')
localInGlobal _ z = ([], z)

isIn :: EnvBlock -> Env -> Bool
isIn db e = let eNoLevels = map (\(a,b,c) -> (a,c)) e
                   in (db `intersect` eNoLevels) == db

{-
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
-}


-- --------
-- --
-- - Properties over said Let expressions, pt. 2
-- --
-- --------



{-
-- if our generator could generate errors, then it would make more sense to say
oneNameInEnv :: Let MemoTable -> Zipper (Let MemoTable) -> ([Property], Zipper (Let MemoTable))
oneNameInEnv (Var v _) z = let (computedErrs, z') = errs z 
                               (computedEnv, z'') = env z'
                               listErrors = v `mBIn` computedEnv
                           in 
                              ([null computedErrs ==> property $ null listErrors], z'')
oneNameInEnv _       z = ([], z)
-}

-- alternatively, if we find a var not in the environment, then the attribute "errs" must report it

prop_errFound :: Let MemoTable -> Property 
prop_errFound t = forallNodes errFound t

errFound :: Let MemoTable -> Zipper (Let MemoTable) -> ([Property], Zipper (Let MemoTable))
errFound (Var v _) z = let (computedEnv,  z')   = env z
                           (computedErrs, z'') = errs z' 
                       in ([not (v `varIn` computedEnv) ==> not (null computedErrs)], z'')
errFound _       z = ([], z)

-- if dcli == [] and errs == [], then name is in dclBlock
prop_localNames :: Let MemoTable -> Property
prop_localNames t = forallNodes localNames t

localNames :: Let MemoTable -> Zipper (Let MemoTable) -> ([Property], Zipper (Let MemoTable))
localNames (Var v _) z = let l = nest z 
                             (computedDcli    , l'  ) = dcli l 
                             (computedErrs    , l'' ) = errs l' 
                             (computedDclBlock, l''') = dclBlock l'' 
                         in ([null computedDcli && null computedErrs ==> v `varIn'` computedDclBlock], z)
localNames _       z = ([], z)



-- aux functions to declutter code of localNames
nest :: Zipper (Let MemoTable) -> Zipper (Let MemoTable)
nest z = inherit isNest id z
 where  isNest :: Let MemoTable -> Bool 
        isNest (Let _ _ _) = True
        isNest _           = False 
 
varIn' v l = v `elem` map fst l


-- to be able to run all tests at once. Note that they must start with "prop_".
return []
runTests = $quickCheckAll