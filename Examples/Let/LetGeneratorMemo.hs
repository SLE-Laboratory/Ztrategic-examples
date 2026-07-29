{-# LANGUAGE RecursiveDo, FlexibleInstances #-}
module Examples.Let.LetGeneratorMemo (
    genCircFaultyMemo, 
    genRootCircMemo    
) 
    where
import Test.QuickCheck
import qualified Examples.Let.Shared as S
import qualified Examples.Let.Let_Zippers as LZ
import Data.Generics.Zipper
import Language.ZipperAG

import Examples.Let.LetMemo
import Language.Memo.AGMemo

-- instance Arbitrary S.Root where
--     arbitrary = genRootCirc

instance Arbitrary (Let MemoTable) where
    arbitrary = genRootCircMemo


-- Loops if not using genExpCirc', which is faulty
genCircFaulty :: Int -> Gen S.Root
genCircFaulty n = genRootCirc `suchThat` \t -> length (LZ.errs (toZipper t)) == n

genCircFaultyMemo :: Int -> Gen (Let MemoTable)
genCircFaultyMemo n = genRootCircMemo `suchThat` \t -> length (fst $ errs (toZipper t)) == n

-- we only convert from letMemo to Let here, using letToRoot
genRootCirc :: Gen S.Root 
genRootCirc = letToRoot <$> genRootCircMemo

genRootCircMemo :: Gen (Let MemoTable)
genRootCircMemo = sized (\n -> mdo
    zCirc <- (`Root` emptyMemo) . fst <$> genLetCirc n (toZipper zCirc.$1)
    return zCirc)

genLetCirc :: Int -> Zipper (Let MemoTable) -> Gen (Let MemoTable, Zipper (Let MemoTable))
genLetCirc n z = do
    -- randomV <- choose (1, max 1 n)
    let randomV = n
    (zList, z') <- genListCirc randomV .:@:. (z.$1)
    (zExp, z'') <- genExpCirc .:@:. (z'.$2)
    let newNode = Let zList zExp emptyMemo
    return (newNode, setHole newNode z'')

genListCirc :: Int -> Zipper (Let MemoTable) -> Gen (Let MemoTable, Zipper (Let MemoTable))
genListCirc 0 z = let newNode = EmptyList emptyMemo :: Let MemoTable 
                  in return (newNode, setHole newNode z)
genListCirc n z = do
    frequency [
            (1, genNestedLetCirc n z),
            (4, genAssignCirc n z)
            ]

genAssignCirc :: Int -> Zipper (Let MemoTable) -> Gen (Let MemoTable, Zipper (Let MemoTable))
genAssignCirc n z =  do
    let (dcliVal, z') = dcli z 
    name <- genName `suchThat` flip notElem (map (\(a,b,c) -> a) dcliVal)
    -- name <- genName `suchThat` flip notElem (map fst $ dcliBlock z)
    (nExp, z'') <- genExpCirc .:@:. (z'.$2)
    (list, z''') <- genListCirc (n-1) .:@:. (z''.$3)
    let newNode = Assign name nExp list emptyMemo
    return (newNode, setHole newNode z''')

genNestedLetCirc :: Int -> Zipper (Let MemoTable) -> Gen (Let MemoTable, Zipper (Let MemoTable))
genNestedLetCirc n z = do
    let (dcliVal, z') = dcli z 
    named <- genName `suchThat` flip notElem (map (\(a,b,c) -> a) dcliVal)
    -- named <- genName `suchThat` flip notElem (map fst $ dcliBlock z)
    nestedSize <- choose (1,5)
    (nLet, z'') <- genLetCirc nestedSize .:@:. (z'.$2)
    (list, z''') <- genListCirc (n-1) .:@:. (z''.$3)
    let newNode = NestedLet named nLet list emptyMemo
    return (newNode, setHole newNode z''')

genExpCirc :: Zipper (Let MemoTable) -> Gen (Let MemoTable, Zipper (Let MemoTable))
genExpCirc z =
    let (envVal, z') = env z 
        decls = map (\(a,b,c) -> a) envVal
    in frequency [
                  (1, do 
                        (left, z'')   <- genExpCirc .:@:. (z'.$1)
                        (right, z''') <- genExpCirc .:@:. (z''.$2)
                        let thisMemoTable = getMemoTable $ fromZipper z'''
                            newNode = Add left right emptyMemo
                        return (newNode, setHole newNode z''')),
                  (1, do 
                        (left, z'')   <- genExpCirc .:@:. (z'.$1)
                        (right, z''') <- genExpCirc .:@:. (z''.$2)
                        let thisMemoTable = getMemoTable $ fromZipper z'''
                            newNode = Sub left right emptyMemo
                        return (newNode, setHole newNode z''')),
                  (5, do 
                        (subExp, z'')   <- genExpCirc .:@:. (z'.$1)
                        let thisMemoTable = getMemoTable $ fromZipper z''
                            newNode = Neg subExp emptyMemo
                        return (newNode, setHole newNode z'')),
                  (5, do 
                        r <- arbitrary
                        let newNode = Const r emptyMemo :: Let MemoTable
                        return (newNode, setHole newNode z)),
                  (5, do 
                        r <- elements decls 
                        let newNode = Var r emptyMemo :: Let MemoTable 
                        return (newNode, setHole newNode z))
                  ]


{- 
-- hardcoded N% chance to generate an error
errorFreq = 10 

genExpCirc' :: Zipper (Let MemoTable) -> Gen (Let MemoTable)
genExpCirc' z =
    let decls = map (\(a,b,c) -> a) $ env z
    in frequency [
                  (1, Add   <$> genExpCirc' (z.$1) <*> genExpCirc' (z.$2)),
                  (1, Sub   <$> genExpCirc' (z.$1) <*> genExpCirc' (z.$2)),
                  (5, Neg   <$> genExpCirc' (z.$1)),
                  (5, Const <$> arbitrary),
                  (5, Var <$> frequency [(90, elements decls),
                                         (errorFreq, vectorOf 10 (return 'X') `suchThat` flip notElem decls)])]
-}



----- 
--- Auxiliary
-----
genName :: Gen S.Name
genName = vectorOf 4 $ choose ('a', 'z')