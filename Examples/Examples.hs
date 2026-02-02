module Examples.Examples where

import System.Environment
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Examples.RepMin.Benchmark
import Examples.Let.Benchmark
import Examples.Smells.Benchmark

validProgs = [ ("repminNone", repminNone)
              , ("repminTUTP", repminTUTP)
              , ("repminSTUTP", repminSTUTP)
              , ("repminAG", repminAG)
              , ("repminMemo", repminMemo)
              , ("repminZtrategic", repminZtrategic)
              , ("repminZtrategicS", repminZtrategicS)
              , ("letOptNone", letOptNone)
              , ("letOptZtrategic", letOptZtrategic)
              , ("letOptMemoZtrategic", letOptMemoZtrategic)
              , ("letOptMemoZtrategicS", letOptMemoZtrategicS)
              , ("letOptAG", letOptAG)
              , ("letOptMemoZtrategicClearing", letOptMemoZtrategicClearing)
              , ("smellsNone", smellsNone)
              , ("smells", smells)
              , ("smellsTerminals", smellsTerminals)
              , ("smellsStrafunski", smellsStrafunski)
              , ("letPrint", letPrint)
              , ("letPrintMemo", letPrintMemo)

              , ("letOptZtrategicLocal", letOptZtrategicLocal)
              , ("letOptZtrategicAdhoc", letOptZtrategicAdhoc)
              , ("letOptZtrategicAdhocLocal", letOptZtrategicAdhocLocal)
              ]


-- compile with: 
-- ghc Examples.hs -main-is Examples -o Examples -no-keep-hi-files -no-keep-o-files


main = do
    args <- getArgs
    if length args /= 2 then help () else do
        let [prog_s, size_s] = args
            size = read size_s
        let prog = fromMaybe help (lookup prog_s validProgs)
        prog size


help _ = do
    putStrLn "Invalid arguments"
    putStrLn "Expected name of program and input size"
    putStrLn ""
    putStrLn "Correct usage example:"
    putStrLn "> ./Examples repminMemo 1000"
    putStrLn ""
    putStrLn "Valid programs:"
    putStrLn $ "< "  ++ intercalate ", " (map fst validProgs) ++ " >"
    putStrLn ""

