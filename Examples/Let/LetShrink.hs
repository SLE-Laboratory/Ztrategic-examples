module Examples.Let.LetShrink where
import Test.QuickCheck hiding (shrinkList)
import Examples.Let.Shared


---------------
---------------
---------------
---------------
--------- Below we define the shrink function 
---------------
---------------
---------------
---------------

shrinkRoot :: Root -> [Root]
shrinkRoot (Root l) = [Root l' | l' <- shrinkLet l ]

shrinkLet :: Let -> [Let]
shrinkLet (Let l e) = [ Let l' e' | l' <- shrinkList l , e' <- shrinkExp e ]

shrinkList :: List -> [List]
shrinkList EmptyList = [] 
shrinkList (NestedLet n l lst) = [ NestedLet n' l' EmptyList
                                 | n' <- shrink n , l' <- shrinkLet l , not (null n')  ] 
                                 ++ shrinkList lst
shrinkList (Assign    n e lst) = [ Assign n' e' EmptyList
                                 | n' <- shrink n , e' <- shrinkExp e , not (null n')  ] 
                                 ++ shrinkList lst

shrinkExp :: Exp -> [Exp]
shrinkExp (Add e1 e2)   = [e1, e2] ++
                          [Add e1' e2  | e1' <- shrinkExp e1] ++
                          [Add e1 e2'  | e2' <- shrinkExp e2]
shrinkExp (Sub e1 e2)   = [e1, e2] ++
                          [Sub e1' e2  | e1' <- shrinkExp e1] ++
                          [Sub e1 e2'  | e2' <- shrinkExp e2]
shrinkExp (Neg e)       = [e] ++
                          [Neg e'      | e'  <- shrinkExp e] 
shrinkExp (Var n)       = [Var n'      | n'  <- shrink n
                                       , not (null n')     ]
shrinkExp (Const c)     = [Const c'    | c'  <- shrink c ]
