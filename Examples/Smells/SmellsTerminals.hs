module Examples.Smells.SmellsTerminals where 


import Data.Generics.Zipper
import Language.Haskell.Syntax 

import Language.Ztrategic
import Language.StrategicData
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
smells h = fmap fromZipper $ applyTP (innermost step) $ toZipper h
 where step = failTP `adhocTP` aux


----------
----
--- definition of terminal symbols  
----
----------

instance StrategicData HsModule where 
  isTerminal z = isJust (getHole z :: Maybe String)