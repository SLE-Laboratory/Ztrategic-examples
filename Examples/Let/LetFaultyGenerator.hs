{-# LANGUAGE RecursiveDo #-}
module Examples.Let.LetFaultyGenerator where
import Test.QuickCheck hiding (shrinkList)
import Examples.Let.Shared
import Data.Generics.Zipper
import Examples.Let.Let_Zippers
import Language.ZipperAG
import Examples.Let.LetShrink


instance Arbitrary Root where
    arbitrary = genCircFaulty 3
    -- shrink    = shrinkRoot

genCircFaulty :: Int -> Gen Root 
genCircFaulty n = genRootCirc `suchThat` \t -> length (errs (toZipper t)) == n 

genRootCirc :: Gen Root
genRootCirc = sized (\n -> mdo
    zCirc <- Root <$> genLetCirc n ((toZipper zCirc).$1)
    return zCirc)

genLetCirc :: Int -> Zipper Root -> Gen Let
genLetCirc n z = do
    -- randomV <- choose (1, max 1 n)
    let randomV = max 1 n
    zList <- genListCirc randomV (z.$1)
    zExp <- genExpCirc (z.$2)
    return (Let zList zExp)

genListCirc :: Int -> Zipper Root -> Gen List
genListCirc 0 z = return EmptyList
genListCirc n z = do
    frequency [
            (1, genNestedLetCirc n z),
            (4, genAssignCirc n z)
            ]

genAssignCirc :: Int -> Zipper Root -> Gen List
genAssignCirc n z =  do
    name <- genName `suchThat` flip notElem (map (\(a,b,c) -> a) $ dcli z)
    -- name <- genName `suchThat` flip notElem (map fst $ dcliBlock z)
    nExp <- genExpCirc (z.$2)
    list <- genListCirc (n-1) (z.$3)
    return $ Assign name nExp list

genNestedLetCirc :: Int -> Zipper Root -> Gen List
genNestedLetCirc n z = do
    named <- genName `suchThat` flip notElem (map (\(a,b,c) -> a) $ dcli z)
    -- named <- genName `suchThat` flip notElem (map fst $ dcliBlock z)
    nestedSize <- choose (1,5)
    nLet <- genLetCirc nestedSize (z.$2)
    list <- genListCirc (n-1) (z.$3)
    return $ NestedLet named nLet list

genExpCirc :: Zipper Root -> Gen Exp
genExpCirc z =
    let decls = map (\(a,b,c) -> a) $ env z
    in frequency [
                  (1, Add   <$> genExpCirc (z.$1) <*> genExpCirc (z.$2)),
                  (1, Sub   <$> genExpCirc (z.$1) <*> genExpCirc (z.$2)),
                  (5, Neg   <$> genExpCirc (z.$1)),
                  (5, Const <$> arbitrary),
                  (5, Var <$> frequency [(90, elements decls), 
                                         -- (10, listOf1 (choose ('a', 'z')) `suchThat` flip notElem decls)])]
                                         (10, vectorOf 10 (choose ('X', 'X')) `suchThat` flip notElem decls)])]

genName :: Gen Name
genName = vectorOf 4 $ choose ('a', 'z')