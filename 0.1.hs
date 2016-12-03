{-# LANGUAGE GADTs, BangPatterns #-}

data Op a where 
    I      :: Int -> Op Int -- all the possible numbers
    IL     :: [Int] -> Op [Int]
    Add    :: Op Int -> Op Int -> Op Int

instance (Show a) => Show (Op a) where
    show (I n) = show n
    show (IL l) = show l
    show (Add o1 o2) = show o1 ++ " + " ++ show o2


eval :: Op a -> a
eval (IL l) = l
eval (I n) = n
eval (Add o1 o2) = eval o1 + eval o2

data Expr = Map (Op [Int]) (Op Int -> Op Int) (Op [Int]) 

instance Show Expr where
    show (Map a b c) = "Map" ++ show a ++ show c

run' :: Expr -> Expr
run' a@(Map _ _ (IL [])) = a
run' m@(Map (cur) (exp) (args)) = Map (IL (baseCase ++ [ eval . exp $ nextVal ])) exp (IL remainder)
    where baseCase = eval cur
          nextVal  = I (head (eval args))
          remainder = drop 1 (eval args)

run :: Expr -> IO ()
run (Map cur _ (IL [])) = putStrLn $ show cur
run m@(Map (cur) (exp) (args)) = do
    let bc = eval cur
    let nextVal = I (head (eval args))
    let remainder = drop 1 (eval args)
    let state = run' m
    putStrLn $ show bc ++ " : " ++ show (exp nextVal) ++ " : map " ++ show remainder 
    putStrLn $ show (bc ++ [eval . exp $ nextVal]) ++ " : map " ++ show remainder
    run state

m = Map (IL []) (Add (I 10)) (IL [1,2,3,4])

