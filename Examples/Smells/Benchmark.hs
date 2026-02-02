module Examples.Smells.Benchmark (
    smellsNone,
    smells,
    smellsTerminals,
    smellsStrafunski
    ) where 

import qualified Examples.Smells.Smells as Smells
import qualified Examples.Smells.SmellsTerminals as SmellsTerminals
import qualified Examples.Smells.SmellsStrafunski as SmellsStrafunski


smellsNone       size = do
    let f = "Smells/Inputs/Gathers_" ++ (show size) ++ ".hs"
    s <- readFile f
    putStrLn s 


smells           size = do
    let f = "Smells/Inputs/Gathers_" ++ (show size) ++ ".hs"
    s <- Smells.elim f
    putStrLn s 

smellsTerminals  size = do 
    let f = "Smells/Inputs/Gathers_" ++ (show size) ++ ".hs"
    s <- SmellsTerminals.elim f
    putStrLn s 

smellsStrafunski size = do 
    let f = "Smells/Inputs/Gathers_" ++ (show size) ++ ".hs"
    s <- SmellsStrafunski.elim f
    putStrLn s 