{-# LANGUAGE GADTs #-}

{- Davin Chia, Kit Tse
 - CS413 Final Project
 -
 - MapExpr.hs
 -}
module MapExpr where

data MapOp a where
    I    :: Integer -> MapOp Integer
    IL   :: [Integer] -> MapOp [Integer]
    Add  :: MapOp Integer -> MapOp Integer -> MapOp Integer
    Mult :: MapOp Integer -> MapOp Integer -> MapOp Integer

instance (Show a) => Show (MapOp a) where
    show (I n) = show n
    show (IL l) = show l
    show (Add o1 o2) = show o1 ++ " + " ++ show o2
    show (Mult o1 o2) = show o1 ++ " * " ++ show o2

eval :: MapOp a -> a
eval (IL l) = l
eval (I n) = n
eval (Add o1 o2) = eval o1 + eval o2
eval (Mult o1 o2) = eval o1 * eval o2

-- lambda
data LExpr where
    LAdd :: MapOp Integer -> LExpr
    LMult :: MapOp Integer -> LExpr

instance Show LExpr where
    show (LAdd op) = "(+ " ++ show op ++ ")"
    show (LMult op) = "(* " ++ show op ++ ")"

convertLExprToExpr :: LExpr -> (MapOp Integer -> MapOp Integer)
convertLExprToExpr (LAdd n) = Add n
convertLExprToExpr (LMult n) = Mult n

-- map
data MapExpr = Map (MapOp [Integer]) LExpr (MapOp [Integer])

instance Show MapExpr where
    show (Map a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c

