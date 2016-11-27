{-# LANGUAGE GADTs #-}

data Op a where 
  I      :: Int -> Op Int -- all the possible numbers
  IL     :: [Int] -> Op [Int]
  Lambda :: (a -> b) -> Op (a -> b)
  Add    :: Op Int -> Op Int -> Op Int

instance (Show a) => Show (Op a) where
  show (I n) = show n
  show (IL l) = show l
  show (Add o1 o2) = "Add " ++ show o1 ++ show o2

eval :: Op a -> a
eval (IL l) = l
eval (I n) = n
eval (Add o1 o2) = eval o1 + eval o2

data Expr = Map (Op [Int]) (Op Int -> Op Int) (Op [Int]) 

instance Show Expr where
  show (Map a b c) = "Map" ++ show a ++ show c

run :: Expr -> Expr
run a@(Map _ _ (IL [])) = a
run (Map (cur) (exp) (args)) = Map (IL ((eval cur) ++ [eval(exp(I (head (eval args))))])) (exp) (IL (drop 1 (eval args)))

m = Map (IL []) (Add (I 10)) (IL [1,2,3,4])


-- Op class that contains all the basic types, operations that can be performed on them and lambda
-- Expr class that contains the higher-order functions