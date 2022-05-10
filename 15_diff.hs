data Expr =  Expr :+: Expr | Exp Expr| Expr :*: Expr | Expr :-: Expr | Expr :/: Expr | Const Integer |Expr :^: Expr| X | Cos Expr | Sin Expr | Ln Expr | Tan Expr | Cot Expr | Neg Expr
  deriving (Eq, Read)

simp ( (Const n) :+: (Const m)) = Const $ n+m
simp ( (Const n) :-: (Const m)) = Const $ n-m
simp ( (Const n) :*: (Const m)) = Const $ n*m
simp ( (Const n) :^: (Const m)) = Const $ n^m
simp ( Neg (Const n)) = Const $ (-n)
simp ( x :-: Neg y) = x :+: y
simp ( x :+: Neg y) = x :-: y
simp ( (Const 0) :+: y) = y
simp ( x :+: (Const 0)) = x
simp ( (Const 0) :*: _) = Const 0
simp ( (Const 0) :/: _) = Const 0
simp ( _ :*: (Const 0)) = Const 0
simp ( (Const 1) :*: y) = y
simp ( x :*: (Const 1)) = x
simp ( (Const (-1)) :*: y) = Neg y
simp ( x :*: (Const (-1))) = Neg x
simp ( x :-: (Const 0)) = x
simp ( (Const 0) :-: y) = Neg y
simp ( x :/: (Const 1)) = x
simp ( x :^: (Const 1)) = x
simp ( (Const 1) :^: _ ) = Const 1
simp ( (Const 0) :^: _ ) = Const 0
simp ( _ :^: (Const 0)) = Const 1
simp (x :/: y) = if (y==x) then Const 1 else x :/: y
simp ( x :-: y) = if (y==x) then Const 0 else x :-: y
simp (e) = e

diff' :: Expr -> Expr
diff' (Const _) = Const 0
diff' (X) = Const 1
diff' ((Const n) :*: x) = simp $ ((Const n) :*: (simp $ diff' x))
diff' (x :*: (Const n)) = simp $ (Const n :*: (simp $ diff' x))
diff' (x :^: (Const n)) = simp $((Const n) :*: ( simp $ x :^: (Const (n-1))))
diff' (x :+: y) = simp $ diff' x :+: (simp $ diff' y)
diff' (x :-: y) = simp $ diff' x :-: diff' y
diff' (x :*: y) = simp $ (simp $ y :*: (simp $ diff' x)) :+: (simp $ x :*: (simp $ diff' y))
diff' (x :/: y) = simp $ (simp $ (simp $(simp $ y :*: diff' x) :-: (simp $ simp $ x :*: diff' y)) :/: (simp $ y :^: Const 2))
diff' (Exp x) = simp $ (simp $ diff' x :*: Exp x)
diff' (Cos x) = simp $ (diff' x :*: Neg(Sin x))
diff' (Sin x) = simp $ diff' x :*: Cos x
diff' (Ln x) = simp $ (simp $ diff' x :*: Const 1) :/: x
diff' (Tan x) = simp $ (simp $ diff' x :*: Const 1) :/: (Cos x :^: Const 2)
diff' (Cot x) = simp $(simp $ diff' x :*: Const (-1)) :/: ((Sin x) :^: Const 2)


instance Show Expr where
    show (Const n) = if n >= 0 then show n else "(" ++ show n ++ ")"
    show (X) = "x"
    show (Neg e) = "(- " ++ show e ++ ")"
    show (x :+: y) = '(' : show x ++ "+" ++ show y ++ ")"
    show (x :-: y) = '(' : show x ++ " - " ++ show y ++ ")"
    show (x :*: y) = show x ++ "*" ++ show y
    show (x :/: y) = show x ++ "/" ++ show y 
    show (x :^: y) = show x ++ "^" ++ show y
    show (Sin x) = "sin(" ++ show x ++")"
    show (Cos x) = "cos(" ++ show x ++")"
    show (Tan x) = "tan(" ++ show x ++")"
    show (Cot x) = "cot(" ++ show x ++")"
    show (Exp x) = "exp(" ++ show x ++")"
    show (Ln x) = "ln(" ++ show x ++")"

main:: IO ()  
main = do
    putStrLn "Enter an expression like a \n(Exp ((X :^: (Const 2)) :*: (Const 2)) :*: Sin(Const 1 :/: X)) :-: Const 3\n means that exp(x^2 * 2)*sin(1/x)-3\n" 
    putStrLn "You can use this expressions with variables and constants: x :+: y, x :-: y, x :*: y, x :/: y, x :^: y, Sin(x), Cos(x), Tan(x), Cot(x), Ln(x), Exp(x)\n"
    s <- getLine
    putStrLn "\nDerivative:"
    print $ diff' $ read s