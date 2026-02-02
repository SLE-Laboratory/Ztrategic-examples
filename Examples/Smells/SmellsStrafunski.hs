module Examples.Smells.SmellsStrafunski where 


import Data.Generics.Zipper
import Language.Haskell.Syntax 
import Data.Generics.Strafunski.StrategyLib.StrategyLib
import Examples.Smells.Common

----------
----
--- Smell Elimination Strategy  
----
----------

elim filename = do 
    a <- readFile filename 
    let r = transformWith smells a   
    return r 


smells :: HsModule -> Maybe HsModule
smells h = applyTP (innermost step) h
 where step = failTP `adhocTP` aux