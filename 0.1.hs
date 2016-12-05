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


-- lambda
data LExpr where
    LAdd :: Op Int -> LExpr

instance Show LExpr where
    show (LAdd op) = "(+ " ++ show op ++ ")"

lAddToAdd :: LExpr -> (Op Int -> Op Int)
lAddToAdd (LAdd n) = Add n



-- map
data Expr = Map (Op [Int]) LExpr (Op [Int])

instance Show Expr where
    show (Map a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c

run' :: Expr -> Expr
run' a@(Map _ _ (IL [])) = a
run' m@(Map cur lExp args) = Map (IL (baseCase ++ [ eval . exp $ nextVal ])) lExp (IL remainder)
    where baseCase = eval cur
          nextVal  = I (head (eval args))
          remainder = drop 1 (eval args)
          exp = lAddToAdd lExp


run :: Expr -> IO ()
run (Map cur _ (IL [])) = putStrLn $ show cur
run m@(Map cur lExp args) = do
    let bc = eval cur
    let nextVal = I (head (eval args))
    let remainder = drop 1 (eval args)
    let op = lAddToAdd lExp
    let exp = op nextVal
    let evalExp = eval exp
    let state = run' m

    putStrLn $ show m
    putStrLn $ "   1. " ++ show bc ++ " : " ++ show exp ++ " : map " ++ show lExp ++ " " ++ show remainder 
    putStrLn $ "   2. " ++ show bc ++ " : " ++ show evalExp ++ " : map " ++ show lExp ++ " " ++ show remainder
    putStrLn $ "   3. " ++ show (bc ++ [evalExp]) ++ " : map " ++ show lExp ++ " "  ++ show remainder ++ "\n"
    run state

m = Map (IL []) (LAdd (I 10)) (IL [1,2,3,4])

