data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2


main = do
    print $ eval $ Lit 5
    print $ eval $ Add (Lit 5) (Lit 5)
    let a1 = Add (Lit 9001) (Lit 1)
    let a2 = Add a1 (Lit 20001)
    let a3 = Add (Lit 1) a2
    print $ printExpr a3
