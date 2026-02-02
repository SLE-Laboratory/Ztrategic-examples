module Examples.Let.Benchmark (
    letOptNone,
    letOptZtrategic,
    letOptMemoZtrategic,
    letOptMemoZtrategicS, 
    letOptAG,
    letOptMemoZtrategicClearing,
    letPrint,
    letPrintMemo,
    generator, 

    letOptZtrategicLocal,
    letOptZtrategicAdhoc,
    letOptZtrategicAdhocLocal
    ) where 

import qualified Examples.Let.LetZtrategic as LetZtrategic 
import qualified Examples.Let.LetMemoZtrategic as LetMemoZtrategic 
import qualified Examples.Let.LetMemoZtrategicS as LetMemoZtrategicS
import qualified Examples.Let.LetAG as LetAG
import qualified Examples.Let.Clearing.LetMemoZtrategic as LetMemoZtrategicClearing 

import qualified Examples.Let.Pretty as LetPrint
import qualified Examples.Let.PrettyMemo as LetPrintMemo

import Examples.Let.Shared


generator = genLetProg

-- let generation and printing - no optimization
letOptNone        size = print $ id $ generator size

-- let generation, optimization and printing
letOptZtrategic      size = print $ LetZtrategic.opt      $ generator size  
letOptMemoZtrategic  size = print $ LetMemoZtrategic.opt  $ generator size  
letOptMemoZtrategicS size = print $ LetMemoZtrategicS.opt $ generator size  
letOptAG             size = print $ LetAG.circ            $ generator size 
letOptMemoZtrategicClearing  size = print $ LetMemoZtrategicClearing.opt  $ generator size  

-- comparing attributes
letOptZtrategicLocal      size = print $ LetZtrategic.optLocal      $ generator size  
letOptZtrategicAdhoc      size = print $ LetZtrategic.optAdhoc      $ generator size  
letOptZtrategicAdhocLocal size = print $ LetZtrategic.optAdhocLocal $ generator size  


-- let generation and pretty printing
letPrint             size = LetPrint.pretty $ generator size
letPrintMemo         size = LetPrintMemo.pretty $ generator size

----------
----
--- Generation of Nested Lets v1  
----
----------

t = (Let
        (Assign "a" (Add (Var "b") (Const 3)) 
        (Assign "c" (Const 8) 
        (NestedLet    "w" (Let (Assign "z" (Add (Var "a") (Var "b")) EmptyList) 
                        (Add (Var "z") (Var "b")))
        (Assign "b" (Sub (Add (Var "c") (Const 3)) (Var "c")) EmptyList))))
    (Sub (Add (Var "e") (Var "w")) (Var "a")))

testTree a = Root $ repeeat a
  where
    repeeat 0 = t
    repeeat x = (Let
                    (Assign "a" (Add (Var "b") (Const 3)) 
                    (Assign "c" (Const 8) 
                    (NestedLet    "w" (repeeat (x-1))
                    (Assign "b" (Sub (Add (Var "c") (Const 3)) (Var "c")) EmptyList))))
                (Sub (Add (Var "c") (Var "w")) (Var "a")))
        
testTree' :: Int -> Root
testTree' a = Root (Let (aux a) (Var ("a_" ++ (show a))))
  where aux :: Int -> List
        aux 0 =  Assign "a_0" (Const 10) EmptyList
        aux n = Assign ("a_" ++ show n)
                       (Add (Var ("a_" ++ show (n-1))) (Const 1))
               (aux (n - 1))
        
testTree'' :: Int -> Root
testTree'' n = Root (Let
                         (Assign "a" (Add (Var "b") (Const 3)) 
                         (Assign "c" (Const 8) 
                         (NestedLet    "w" (Let ((letGeneratorAux n) EmptyList) 
                                         (Add (Var "z") (Var "b")))
                         (Assign "b" (Sub (Add (Var "c") (Const 3)) (Var "c")) EmptyList))))
                     (Sub (Add (Var "c") (Var "w")) (Var "a")))

letGeneratorAux 0 = Assign "a" (Add (Var "b") (Const 3))
letGeneratorAux n = NestedLet    "w" (Let ((letGeneratorAux (n-1)) EmptyList)
                                   (Add (Var "z") (Var "b")))


-- --------
-- --
-- - Generation of Nested Lets v2  
-- --
-- --------

genLetProg :: Int -> Root
genLetProg n = Root (Let (genLetTree n)
                     (Var ("n_" ++ show n))
                    )

genLetTree :: Int -> List
genLetTree n = addList (genNestedLets n)
 where addList (NestedLet s l ll) = NestedLet s l (addList' ll)

       addList' EmptyList = Assign "va" (Const 10)
                             (Assign "vb" (Const 20) EmptyList)
       addList' (NestedLet s l ll) = NestedLet s l (addList' ll)
       addList' (Assign s a ll) = Assign s a (addList' ll)


genNestedLets :: Int -> List
genNestedLets 0 = EmptyList
genNestedLets n
  | n == 1 =  NestedLet ("n_"++(show 1))
                          ( Let oneList 
                (Add (Var ("z_" ++ (show 10)))
                          (Var ("z_" ++ (show 9)))
                        )
              )
              (genListAssign (n*10)) 
  | n > 1 = NestedLet ("n_"++(show n))
                          ( Let oneList 
                (Add (Var ("n_" ++ (show (n-1))))
                          (Var ("z_" ++ (show ((n*10)-1))))
                        )
              )
              (genListAssign (n*10))
  where
        oneList = Assign ("zz_" ++ (show n)) (Const 10)
                   (Assign ("zz_"++ (show (n-1))) (Var "va")
                      (genNestedLets (n-1))
                   )

        

genListAssign :: Int -> List
genListAssign 0 = Assign "z_0" (Const 10) EmptyList
genListAssign n
  | n `mod` 9 == 0 = Assign ("z_" ++ show n) (Var "va")
                           (genListAssign (n - 1))
  | otherwise       = Assign ("z_" ++ show n)
                       (Add (Var ("z_" ++ show (n-1))) (Const 1))
               (genListAssign (n - 1))