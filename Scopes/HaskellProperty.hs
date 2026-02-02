module Scopes.HaskellProperty where


import Test.QuickCheck
import Language.Haskell.Syntax
import Language.Haskell.Parser

import Data.Generics.Zipper

import Examples.RepMin.RepMinProperty (forallNodes, existsNode)
import Scopes.Haskell_Interface
import Scopes.BlockA68
-- import Language.Grammars.ZipperAG (parent)
import Language.ZipperAG

import Language.Ztrategic
import Language.Haskell.Pretty

import Control.Monad.State
import Control.Monad.Trans.State.Plus
import Control.Monad.Plus
import Data.Functor.Identity 

prop_noDeadPatterns :: HsModule -> Property 
prop_noDeadPatterns h = forallNodes patternNotDead h 
 where patternNotDead :: HsPat -> Zipper HsModule -> [Property] 
       patternNotDead (HsPVar (HsIdent s)) z = return $ 
                        counterexample (s ++ " is dead; value of isTopLevel' is " ++ show (isTopLevel' z)) $ 
                        not (isTopLevel' z && dead (inBlock z))

prop_noDeadFunctions :: HsModule -> Property 
prop_noDeadFunctions h = forallNodes functionNotDead h 
 where functionNotDead :: HsDecl -> Zipper HsModule -> [Property] 
       functionNotDead (HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = return $ counterexample (s ++ " is dead; isTopLevel' is " ++ show (isTopLevel' z)) $ not (isTopLevel' z && dead (inBlock z))
-- prop_onlyMainDead s = deadCode' s == ["main"]





prop_noDeadCode :: HsModule -> Property 
prop_noDeadCode h = prop_noDeadPatterns h .&&. prop_noDeadFunctions h

isTopLevel :: Zipper HsModule -> Bool 
isTopLevel t = lev (inBlock t) == 0    
               
isTopLevel' :: Zipper HsModule -> Bool 
isTopLevel' t = case constructor t of 
                  CHsPVar -> case constructor (parent t) of 
                              CHsPatBind -> isTopLevelAux (parent (parent t))
                              _          -> False 
                  CHsFunBind -> isTopLevelAux (parent t)
                  _       -> False 
 where isTopLevelAux t = case constructor t of 
                           CPrimitiveList -> isTopLevelAux (parent t)
                           CHsModule      -> True 
                           _              -> False  

-- https://hackage.haskell.org/package/module-management-0.20.4
-- https://hackage.haskell.org/package/module-management-0.20.4/docs/Language-Haskell-Modules.html
-- github/hackage code scraping
-- investigate if GHC can merge all modules into a single file 
-- (find big projects that dont use extensions maybe)
-- remove dead functions and see if it compiles again :)
-- TP strategy that only points towards dead code
-- TP strategy that REMOVES (??) dead code (for functions -> empty list of hsmatch may work)
-- When we present property-based AGs, say that we have some properties (attributes?) that only look at the AST and some that look at attributes of the first-order AND higher-order AGs.  

-- next meeting feb 10, 9AM UY time / 12 PT time


-- strategies 
-- we can find dead code with deadCode (querying Block) and deadCode' (querying the Haskell AST itself)

-- finding dead code at the top level only
deadCodeTopLevel' :: String -> [String]
deadCodeTopLevel' s = let ast     = parse s 
              in applyTU (full_tdTU step) (toZipper ast) 
 where step = failTU `adhocTUZ` checkOneDeadTopLevel' `adhocTUZ` checkOneFunTopLevel

checkOneDeadTopLevel' :: HsPat -> Zipper HsModule -> [String]
checkOneDeadTopLevel' (HsPVar (HsIdent s)) z = if isTopLevel' z && dead (inBlock z) then [s] else []
checkOneDeadTopLevel' _ z          = []

checkOneFunTopLevel :: HsDecl -> Zipper HsModule -> [String]
checkOneFunTopLevel (HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = if isTopLevel' z && dead (inBlock z) then [s] else []
checkOneFunTopLevel _ z          = []

-- remove dead declarations...?
-- patterns are replaced by wildcard `_`
-- functions become empty (therefore removed)
-- this one incorrectly uses a full_ strategy 
removeDeadDeclTD :: String -> String 
removeDeadDeclTD s = let ast          = parse s
                         Just cleaned = applyTP (full_tdTP step) (toZipper ast) 
                         step         = idTP `adhocTPZ` cleanPatternsTD `adhocTPZ` cleanFunctionsTD
                     in prettyPrint $ fromZipper cleaned

cleanPatternsTD :: HsPat -> Zipper HsModule -> Maybe HsPat 
cleanPatternsTD x@(HsPVar (HsIdent s)) z = if {- isTopLevel' z && -} dead (inBlock z) then Just HsPWildCard else Just x
cleanPatternsTD x _ = Just x 

cleanFunctionsTD :: HsDecl -> Zipper HsModule -> Maybe HsDecl 
cleanFunctionsTD x@(HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = if {- isTopLevel' z && -} dead (inBlock z) then Just (HsFunBind []) else Just x 
cleanFunctionsTD x _ = Just x 


exRemove = "module Main where \n\
\\n\
\main = f1 1\n\
\f3 x = f2 x\n\
\f2 x = f1 x\n\
\f1 x = putStrLn \"test\""

exRemoveBU = "module Main where \n\
\\n\
\f1 x = putStrLn \"test\" \n\
\f2 x = f1 x\n\
\f3 x = f2 x\n\
\main = f3 1"


removeDeadDecl :: String -> String 
removeDeadDecl s = let ast          = parse s
                       Just cleaned = applyTP (innermost step) (toZipper ast) 
                       step         = failTP `adhocTPZ` cleanPatterns `adhocTPZ` cleanFunctions
                     in prettyPrint $ fromZipper cleaned

cleanPatterns :: HsPat -> Zipper HsModule -> Maybe HsPat 
cleanPatterns x@(HsPVar (HsIdent "main")) z = Nothing
cleanPatterns x@(HsPVar (HsIdent s)) z = if {- isTopLevel' z && -} dead (inBlock z) then Just HsPWildCard else Nothing
cleanPatterns x _ = Nothing 

cleanFunctions :: HsDecl -> Zipper HsModule -> Maybe HsDecl 
cleanFunctions x@(HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = if {- isTopLevel' z && -} dead (inBlock z) then Just (HsFunBind []) else Nothing
cleanFunctions x _ = Nothing



-- innermost strategy removing dead code, and storing whatever
-- is removed into the state monad. Problem: removing patterns
-- only removes the lefthand-side, not enabling to signal 
-- the RHS as dead code
-- computeDeadDecl :: String -> [String]
computeDeadDecl s = let ast          = parse s
                        Identity (Just cleaned, deads) = runStatePlusT (applyTP (innermost step) (toZipper ast)) [] 
                     --    step         = failTP `adhocTPZ` cleanPatternsM  `adhocTPZ` cleanFunctionsM
                        step         = failTP `adhocTPZ` cleanM
                     in (prettyPrint $ fromZipper cleaned, deads) 
                     -- in deads 

cleanPatternsM :: HsPat -> Zipper HsModule -> StatePlusT [String] Identity HsPat 
cleanPatternsM x@(HsPVar (HsIdent "main")) z = mzero
cleanPatternsM x@(HsPVar (HsIdent s)) z = if isTopLevel' z && dead (inBlock z) 
                                            then get >>= \st -> put (s:st) >> return HsPWildCard 
                                            else mzero
cleanPatternsM x _ = mzero 

cleanFunctionsM :: HsDecl -> Zipper HsModule -> StatePlusT [String] Identity HsDecl 
cleanFunctionsM x@(HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = 
                  if isTopLevel' z && dead (inBlock z) 
                        then get >>= \st -> put (s:st) >> return (HsFunBind []) 
                        else mzero 
cleanFunctionsM x _ = mzero

-- we attempt to remove RHSs 
cleanM :: HsDecl -> Zipper HsModule -> StatePlusT [String] Identity HsDecl
cleanM x@(HsPatBind _ (HsPVar (HsIdent "main")) _ _) z = mzero
cleanM x@(HsPatBind sr (HsPVar (HsIdent s)) _ _) z = 
      if isTopLevel' (z.$2) && dead (inBlock (z.$2)) 
            then get >>= \st -> put (s:st) >> return (HsPatBind sr HsPWildCard (HsUnGuardedRhs HsWildCard) []) 
            else mzero
cleanM x@(HsFunBind ((HsMatch _ (HsIdent s) _ _ _):_)) z = 
                  if isTopLevel' z && dead (inBlock z) 
                        then get >>= \st -> put (s:st) >> return (HsFunBind []) 
                        else mzero 
cleanM x _ = mzero

-- Weeder uses HIE files for analysis. 1 HIE file per module. 
-- Builds a graph crossing the info from these files. 

-- Maybe we can argue we can use a strategy to find toplevel decls from each imported module
-- and build the same graph, but that is an engineering exercise  





-- meet at 14h UY / 17h PT 

-- subsection on properties on HOAGs
-- ensure changes are coloured

halex_block  = do 
      r <- readFile "/home/zenunomacedo/repos/hs-all-in-one/Generated.hs"
      return $ blockExample r


halex_deadCode = do 
      r <- readFile "/home/zenunomacedo/repos/hs-all-in-one/Generated.hs"
      -- print $ deadCodeTopLevel' r
      print $ computeDeadDecl r

main = halex_deadCode