
data Expr = Lit Integer
          | Add Expr Expr deriving (Show)

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b

testEval =  eval (Add (Lit 1) (Lit 9001)) == 9002

e1 = (Add (Lit 1) (Lit 9001))
e2 = (Add (Lit 1) (Lit 9001)) 

testEval' = eval (Add e1 e2)

-- 2. Write a printer for the expressions.

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b

testPrintExpr1 = printExpr (Add (Lit 1) (Lit 9001)) == "1 + 9001"

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

testPrintExpr2 =  printExpr a3 == "1 + 9001 + 1 + 20001"

