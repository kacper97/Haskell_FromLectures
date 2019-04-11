--Exercise 1
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
data Op = EVALA Expr | ADD Int | MULT Int | EVALM Expr
data Nat = Zero | Succ Nat
type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n ) c = exec c n
eval (Add x y ) c = eval x (EVALA y : c )
eval (Mult x y ) c = eval x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [ ] n = n
exec (EVALA y : c ) n = eval y (ADD n : c )
exec (EVALM y : c ) n = eval y (MULT n : c )
exec (ADD n : c ) m = exec c ( n + m)
exec (MULT n : c ) m = exec c ( n * m)

val :: Expr -> Int
val e = eval e [ ]

--Exercise 2 added mult
--Exercise 3 added Nat 
mult :: Nat -> Nat -> Nat
mult m Zero     = Zero
mult m (Succ n) = add m ( mult m n)

--Exercise 4 
occurs x (Leaf y)      = x == y
occurs x (Node l y r)  = case compare x y of
                        LT -> occurs x l
                        EQ -> True
                        GT -> occurs x r

--Exercise 5
data Tree a = Leaf a | Node (Tree a) (Tree a)
--leaves :: Tree a -> Int
--balanced :: Tree a -> Bool
leaves ( Leaf _) = 1
leaves ( Node l r ) = leaves l + leaves r
balanced ( Leaf  _ ) = True
balanced ( Node l r ) = abs (leaves l - leaves r ) <= 1
                      && balanced l && balanced r
--leaves (Leaf x) = [x]
--leaves (Node l r) = leaves l ++ leaves r


