module Examples.Smells.Common where 

import Data.Maybe (fromJust)
import Language.Haskell.Syntax 
import Language.Haskell.Parser
import Language.Haskell.Pretty(prettyPrint)

----------
----
--- The 4 rules  
----
----------

force (ParseOk x) = x
parse = force . parseModule

transformWith :: (HsModule -> Maybe HsModule) -> String -> String
transformWith transform s = prettyPrint $ fromJust $ transform $ parse s 

----------
----
--- The 4 rules  
----
----------

--changes [a] ++ b to a:b
joinList :: HsExp -> Maybe HsExp
joinList (HsInfixApp (HsList [h]) (HsQVarOp (UnQual (HsSymbol "++"))) (HsList t)) = Just $ HsInfixApp h (HsQConOp (Special HsCons)) (HsList t)
joinList _ = Nothing

--transforms "length x == 0", and "x == []", into "null x"
nullList :: HsExp -> Maybe HsExp
nullList (HsInfixApp (HsApp (HsVar (UnQual (HsIdent "length"))) a) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 0))) = Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
nullList (HsInfixApp (HsLit (HsInt 0)) (HsQVarOp (UnQual (HsSymbol "=="))) (HsApp (HsVar (UnQual (HsIdent "length"))) a)) = Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
nullList  (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsList [])) = Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
nullList  (HsInfixApp (HsList []) (HsQVarOp (UnQual (HsSymbol "=="))) a) = Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
nullList _ = Nothing

--transforms "x==True" and "True==x" into "x", and "x==False" and "False==x" into "not x"
redundantBoolean :: HsExp -> Maybe HsExp
redundantBoolean (HsInfixApp (HsCon (UnQual (HsIdent "True"))) (HsQVarOp (UnQual (HsSymbol "=="))) a) = Just a
redundantBoolean (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsCon (UnQual (HsIdent "True")))) = Just a
redundantBoolean (HsInfixApp (HsCon (UnQual (HsIdent "False"))) (HsQVarOp (UnQual (HsSymbol "=="))) a) = Just $ 
        (HsApp (HsVar (UnQual (HsIdent "not"))) a)
redundantBoolean (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsCon (UnQual (HsIdent "False")))) = Just $ 
        (HsApp (HsVar (UnQual (HsIdent "not"))) a)
redundantBoolean _ = Nothing

--transforms "if x then True else False" into x, and "if x then False else True" into not x
reduntantIf :: HsExp -> Maybe HsExp
reduntantIf (HsIf a (HsCon (UnQual (HsIdent "True"))) (HsCon (UnQual (HsIdent "False")))) = Just a
reduntantIf (HsIf a (HsCon (UnQual (HsIdent "False"))) (HsCon (UnQual (HsIdent "True")))) = Just $ HsApp (HsVar (UnQual (HsIdent "not"))) a
reduntantIf _ = Nothing

----------
----
--- Do-Notation Elimination  
----
----------

doElim    :: HsExp -> Maybe HsExp
doElim (HsDo [HsQualifier e])  
  = return e
doElim (HsDo (HsQualifier e:stmts))    
  = return (HsInfixApp e (HsQVarOp (hsSymbol ">>")) (HsDo stmts))
doElim (HsDo (HsGenerator _ p e:stmts))
  = do ok <- new_name
       return (letPattern ok p e stmts)
doElim (HsDo (HsLetStmt decls:stmts))
  = return (HsLet decls (HsDo stmts))
doElim _                      
  = Nothing

letPattern ok p e stmts
  = let fail    = hsIdent "fail"        
        failmsg = "Error: pattern-match failure in do-expression."
        mthen   = HsQVarOp (hsSymbol ">>=")
    in (HsLet [HsFunBind [HsMatch noSrcLoc (HsIdent ok) [p] (HsUnGuardedRhs (HsDo stmts)) []],HsFunBind [HsMatch noSrcLoc (HsIdent ok) [HsPWildCard] (HsUnGuardedRhs (HsApp (HsVar fail) (HsLit (HsString failmsg)))) []]] (HsInfixApp e mthen (HsVar (hsIdent ok))))

noSrcLoc = SrcLoc "" 0 0 

hsSymbol s = (UnQual (HsSymbol s))
hsIdent s  = (UnQual (HsIdent s))

new_name :: Monad m => m String
new_name = return "ok"

----------
----
--- List Comprehension Elimination  
----
----------

-- we can test with this
{-
transformLC :: String -> String
transformLC s = transformWith listCompStr s 
 where listCompStr t = fmap fromZipper $ applyTP (innermost step) (toZipper t)
       step = failTP `adhocTP` listComp
-}


listComp :: HsExp -> Maybe HsExp
listComp x@(HsListComp lhs qualifiers) = Just $ desugarListComp x
listComp _ = Nothing 

-- Note:  [x+1 | x <- [1..5], y>2, let y=x+4]
-- Gets desugared into a case where y is tested before it is defined, which leads to believe that the desugaring is incorrect
-- HOWEVER, the original expression is NOT valid haskell code, because apparently the qualifier order matters! 

-- Desugar List Comprehensions according to: 
-- https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11
-- each line corresponds to one rule of the Translation block
-- We test with the input at https://qr.ae/pGl9Gl and it works perfectly
desugarListComp :: HsExp -> HsExp
desugarListComp (HsListComp lhs [HsQualifier (HsCon (UnQual (HsIdent "True")))]) = HsList [lhs]
desugarListComp (HsListComp lhs [q]) = desugarListComp $ HsListComp lhs [q, HsQualifier (HsCon (UnQual (HsIdent "True")))]
desugarListComp (HsListComp lhs ((HsQualifier q) : qs)) = HsIf q (desugarListComp $ HsListComp lhs qs) (HsList [])
desugarListComp (HsListComp lhs ((HsGenerator srcLoc hsPat hsExp) : qs)) = HsLet ([HsFunBind [h1, h2]]) inClause
 where h1 = HsMatch noSrcLoc (HsIdent "desugarOk") [hsPat] (HsUnGuardedRhs (desugarListComp $ HsListComp lhs qs)) []
       h2 = HsMatch noSrcLoc (HsIdent "desugarOk") [HsPWildCard] (HsUnGuardedRhs (HsList [])) []
       inClause = HsApp (HsApp (HsVar (UnQual (HsIdent "concatMap"))) (HsVar (UnQual (HsIdent "desugarOk")))) hsExp
desugarListComp (HsListComp lhs ((HsLetStmt decls) : qs)) = HsLet decls (desugarListComp $ HsListComp lhs qs)


-- ----
-- For benchmarking purposes
-- We trace a character into std output each time we successfully apply, so that we can cheat IO and count 
-- the number of times it was applied
-- More efficient than using different functions for the same node type, but less readable
aux :: HsExp -> Maybe HsExp
aux (HsInfixApp (HsList [h]) (HsQVarOp (UnQual (HsSymbol "++"))) (HsList t)) = {- trace "A" $ -} Just $ HsInfixApp h (HsQConOp (Special HsCons)) (HsList t)
aux (HsInfixApp (HsApp (HsVar (UnQual (HsIdent "length"))) a) (HsQVarOp (UnQual (HsSymbol "=="))) (HsLit (HsInt 0))) = {- trace "A" $ -} Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
aux (HsInfixApp (HsLit (HsInt 0)) (HsQVarOp (UnQual (HsSymbol "=="))) (HsApp (HsVar (UnQual (HsIdent "length"))) a)) = {- trace "A" $ -} Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
aux  (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsList [])) = {- trace "A" $ -} Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
aux  (HsInfixApp (HsList []) (HsQVarOp (UnQual (HsSymbol "=="))) a) = {- trace "A" $ -} Just $ 
         HsApp (HsVar (UnQual (HsIdent "null"))) a
aux (HsInfixApp (HsCon (UnQual (HsIdent "True"))) (HsQVarOp (UnQual (HsSymbol "=="))) a) = {- trace "A" $ -} Just a
aux (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsCon (UnQual (HsIdent "True")))) = {- trace "A" $ -} Just a
aux (HsInfixApp (HsCon (UnQual (HsIdent "False"))) (HsQVarOp (UnQual (HsSymbol "=="))) a) = {- trace "A" $ -} Just $ 
        (HsApp (HsVar (UnQual (HsIdent "not"))) a)
aux (HsInfixApp a (HsQVarOp (UnQual (HsSymbol "=="))) (HsCon (UnQual (HsIdent "False")))) = {- trace "A" $ -} Just $ 
        (HsApp (HsVar (UnQual (HsIdent "not"))) a)
aux (HsIf a (HsCon (UnQual (HsIdent "True"))) (HsCon (UnQual (HsIdent "False")))) = {- trace "A" $ -} Just a
aux (HsIf a (HsCon (UnQual (HsIdent "False"))) (HsCon (UnQual (HsIdent "True")))) = {- trace "A" $ -} Just $ HsApp (HsVar (UnQual (HsIdent "not"))) a
aux _ = Nothing
