{-# LANGUAGE RecursiveDo #-}
module Examples.Let.LetGenerator where
import Test.QuickCheck
import Examples.Let.Shared
import Data.Generics.Zipper
import Examples.Let.Let_Zippers
import Control.Monad ((>=>))
import Data.Data (cast)
import Data.Maybe (fromJust)
import Language.ZipperAG

instance Arbitrary Root where
-- if we want faulty generation, we must swap uses of genExpCirc into genExpCirc'
--    arbitrary = (genCircFaulty 3) 
    arbitrary = genRootCirc 

instance Arbitrary Let where
    arbitrary = sized $ genLet []

genRoot = Root <$> sized (genLet [])

genLet :: [Name] -> Int -> Gen Let
genLet names n = do
    size <- choose (1, max 1 n)
    (newNames, list) <- genList names size
    expr <- genExp newNames
    return $ Let list expr

genList :: [Name] -> Int -> Gen ([Name], List)
genList names 0 = return (names, EmptyList)
genList names n = do
    frequency [
            (1, genNestedLet names n),
            (4, genAssign    names n)
        ]

genNestedLet :: [Name] -> Int -> Gen ([Name], List)
genNestedLet names n = do
    name <- genName `suchThat` (not . flip elem names)
    nLet <- genLet names n
    (newNames, list) <- genList (name:names) (n-1)
    return (newNames, NestedLet name nLet list)

genAssign :: [Name] -> Int -> Gen ([Name], List)
genAssign names n = do
    name <- genName `suchThat` (not . flip elem names)
    nExp <- genExp names
    (newNames, list) <- genList (name:names) (n-1)
    return (newNames, Assign name nExp list)

genExp :: [Name] -> Gen Exp
genExp names = frequency $ [
    (25, Add   <$> genExp names <*> genExp names),
    (25, Sub   <$> genExp names <*> genExp names),
    (50, Neg   <$> genExp names),
    (50, Const <$> arbitrary)]
    ++ if null names then []
       else [(50, Var <$> elements names)]

genName :: Gen Name
genName = vectorOf 2 $ choose ('a', 'z')

---
--- Instead we attempt circularity
---

genCircFaulty :: Int -> Gen Root 
genCircFaulty n = genRootCirc `suchThat` \t -> length (errs (toZipper t)) == n 

genRootCirc :: Gen Root
genRootCirc = sized (\n -> mdo
    zCirc <- Root <$> genLetCirc n ((toZipper zCirc).$1)
    return zCirc)

genLetCirc :: Int -> Zipper Root -> Gen Let
genLetCirc n z = do
    -- randomV <- choose (1, max 1 n)
    let randomV = n
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
                  (5, Var <$> elements decls)]

genExpCirc' :: Zipper Root -> Gen Exp
genExpCirc' z =
    let decls = map (\(a,b,c) -> a) $ env z
    in frequency [
                  (1, Add   <$> genExpCirc' (z.$1) <*> genExpCirc' (z.$2)),
                  (1, Sub   <$> genExpCirc' (z.$1) <*> genExpCirc' (z.$2)),
                  (5, Neg   <$> genExpCirc' (z.$1)),
                  (5, Const <$> arbitrary),
                  (5, Var <$> frequency [(90, elements decls), 
                                         -- (10, listOf1 (choose ('a', 'z')) `suchThat` flip notElem decls)])]
                                         (10, vectorOf 10 (choose ('X', 'X')) `suchThat` flip notElem decls)])]


---------------
---------------
---------------
---------------
--------- Below we have other implementations that don't quite work out
---------------
---------------
---------------
---------------


-- 
-- Using Attributes + zipper with holes that are filled gradually
-- Not a very good solution. Very ugly, and can't solve loops easily, for example!
--

genRoot' :: Gen Root
genRoot' = sized $ \n ->
    let z = toZipper $ Root undefined in
    fromZipper <$> moveM down up (return z) (genLet' n) z

genLet' :: Int -> Zipper Root -> Gen (Zipper Root)
genLet' n z = do
    let z' = setHole (Let EmptyList (Const 0)) z
    randomV <- choose (1, n+1)
    zList <- moveM down' up (return z') (genList' randomV) z'
    moveM down up (return zList) genExp' zList

genList' :: Int -> Zipper Root -> Gen (Zipper Root)
genList' 0 z = return $ setHole EmptyList z
genList' n z = do
    z' <- frequency [
                    (1, genNestedLet' z),
                    (4, genAssign' z)
                    ]
    moveM down up (return z') (genList' (n-1)) z'


genAssign' :: Zipper Root -> Gen (Zipper Root)
genAssign' z =  do
    let (Just z') = setHole' (Assign undefined (Const 0) EmptyList) z
    named <- moveM down' up (return z') genName' z'
    moveM (down' >=> right) up (return named) genExp' named


genExp' :: Zipper Root -> Gen (Zipper Root)
genExp' z = frequency $ [
                    (25, do
                        let z' = setHole (Add (Const 0) (Const 0)) z
                        z'' <- moveM down' up (return z') genExp' z'
                        moveM down up (return z'') genExp' z''),
                    (25, do
                        let z' = setHole (Sub (Const 0) (Const 0)) z
                        z'' <- moveM down' up (return z') genExp' z'
                        moveM down up (return z'') genExp' z''),
                    (50, do
                        let z' = setHole (Neg (Const 0)) z
                        moveM down' up (return z')  genExp' z'),
                    (50, do
                        n <- arbitrary
                        return $ setHole (Const n) z)
                        ]
               ++ ([(50, do
                        v <- elements $ map (\(a,b,c) -> a) $ env z
                        return $ setHole (Var v) z) | not (null (env z))])

genName' :: Zipper Root -> Gen (Zipper Root)
genName' z = flip setHole z <$> genName

genNestedLet' :: Zipper Root -> Gen (Zipper Root)
genNestedLet' z = do
    let (Just z') = setHole' (NestedLet undefined undefined EmptyList) z
    named <- moveM down' up (return z') genName' z'
    n <- choose (1,5)
    moveM (down' >=> right) up (return named) (genLet' n) named


-- Using Attributes
-- This time, we define new attributes just for the generation of Lets
-- However, they are still similar to the existing ones

genRoot'' :: Gen Root
genRoot'' = Root <$> genLet''

genLet'' :: Gen Let
genLet'' = do
    l <- genList'' 5
    -- We build a Root, with a Let containing the generated Let and Expression. 
    -- We compute the errors, and "solve" them by injecting them into the Let. 
    -- To solve the errors, we must generate expressions that use no variables, hence genSafeExp.
    newExp <- genExp''
    let e = errs_uses $ toZipper (Root $ Let l newExp)
    Let <$> addVars e l <*> return newExp

addVars :: Errors -> List -> Gen List
addVars [] l = return l
addVars (n:ns) l = do
    nExp <- genSafeExp
    addVars ns $ Assign n nExp l


genList'' :: Int -> Gen List
genList'' 0 = return EmptyList
genList'' n = frequency [
                    (1, genNestedLet'' n),
                    (4, genAssign'' n)
                    ]

genNestedLet'' :: Int -> Gen List
genNestedLet'' n = do
    name <- genName
    nLet <- genLet''
    list <- genList'' (n-1)
    return $ NestedLet name nLet list

genAssign'' :: Int -> Gen List
genAssign'' n = do
    name <- genName
    nExp <- genExp''
    list <- genList'' (n-1)
    return $ Assign name nExp list

genExp'' :: Gen Exp
genExp'' = frequency [
                    (25, Add   <$> genExp'' <*> genExp''),
                    (25, Sub   <$> genExp'' <*> genExp''),
                    (50, Neg   <$> genExp''),
                    (50, Const <$> arbitrary),
                    (50, Var <$> genName)]

genSafeExp :: Gen Exp
genSafeExp = frequency [
                    (25, Add   <$> genSafeExp <*> genSafeExp),
                    (25, Sub   <$> genSafeExp <*> genSafeExp),
                    (50, Neg   <$> genSafeExp),
                    (50, Const <$> arbitrary)]