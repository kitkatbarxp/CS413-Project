{-# LANGUAGE GADTs #-}

{- Davin Chia, Kit Tse
 - CS413 Final Project
 -
 - MapExpr.hs
 -}
module MapExpr where

data MapOp a where
    I     :: Integer -> MapOp Integer
    B     :: Bool -> MapOp Bool
    D     :: Double -> MapOp Double
    IL    :: [Integer] -> MapOp [Integer]
    DL    :: [Double]  -> MapOp [Double]

    Add   :: MapOp Integer -> MapOp Integer -> MapOp Integer
    Mult  :: MapOp Integer -> MapOp Integer -> MapOp Integer
    Subt  :: MapOp Integer -> MapOp Integer -> MapOp Integer
    
    Eql   :: MapOp Integer -> MapOp Integer -> MapOp Bool

    DivdF :: MapOp Double -> MapOp Double -> MapOp Double
    DivdS :: MapOp Double -> MapOp Double -> MapOp Double


instance (Show a) => Show (MapOp a) where
    show (I n) = show n
    show (IL l) = show l
    show (D n) = show n
    show (DL l) = show l
    show (Add o1 o2) = show o1 ++ " + " ++ show o2
    show (Mult o1 o2) = show o1 ++ " * " ++ show o2
    show (Subt o1 o2) = show o1 ++ " - " ++ show o2
    show (DivdF o1 o2) = show o1 ++ " / " ++ show o2
    show (DivdS o1 o2) = show o2 ++ " / " ++ show o1
    show (Eql o1 o2) = show o1 ++ " == " ++ show o2

eval :: MapOp a -> a
eval (I n) = n
eval (IL l) = l
eval (D n) = n
eval (DL l) = l
eval (Add o1 o2) = eval o1 + eval o2
eval (Mult o1 o2) = eval o1 * eval o2
eval (Subt o1 o2) = eval o1 - eval o2
eval (DivdF o1 o2) = eval o1 / eval o2
eval (DivdS o1 o2) = eval o2 / eval o1
eval (Eql o1 o2) = eval o2 == eval o1

-- lambda
data LExpr where
    LAdd   :: MapOp Integer -> LExpr
    LMult  :: MapOp Integer -> LExpr
    LSubt  :: MapOp Integer -> LExpr
    LDivdF :: MapOp Double -> LExpr
    LDivdS :: MapOp Double -> LExpr
    LEql   :: MapOp Integer -> LExpr

instance Show LExpr where
    show (LAdd op)   = "(+ " ++ show op ++ ")"
    show (LMult op)  = "(* " ++ show op ++ ")"
    show (LSubt op)  = "(" ++ show op ++ "-)"
    show (LDivdF op) = "(" ++ show op ++ "/)"
    show (LDivdS op) = "(/" ++ show op ++ ")"
    show (LEql op)   = "(==" ++ show op ++ ")"

convertILExprToExpr :: LExpr -> (MapOp Integer -> MapOp Integer)
convertILExprToExpr (LAdd n) = Add n
convertILExprToExpr (LMult n) = Mult n
convertILExprToExpr (LSubt n) = Subt n

convertBLExprToExpr :: LExpr -> (MapOp Integer -> MapOp Bool)
convertBLExprToExpr (LEql n) = Eql n

convertDLExprToExpr :: LExpr -> (MapOp Double -> MapOp Double)
convertDLExprToExpr (LDivdF n) = DivdF n
convertDLExprToExpr (LDivdS n) = DivdS n

-- map
data Expr = Map MapExpr
          | Filter FilExpr

instance Show Expr where
    show (Map m)    = show m
    show (Filter f) = show f

data MapExpr = IMap (MapOp [Integer]) LExpr (MapOp [Integer])
             | DMap (MapOp [Double]) LExpr (MapOp [Double])

instance Show MapExpr where
    show (IMap a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (DMap a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c

data FilExpr = IFilter (MapOp [Integer]) LExpr (MapOp [Integer])

instance Show FilExpr where
    show (IFilter a b c) = "filter " ++ show a ++ " " ++ show b ++ " " ++ show c