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
    BL    :: [Bool]  -> MapOp [Bool]

    Add   :: MapOp Integer -> MapOp Integer -> MapOp Integer
    Mult  :: MapOp Integer -> MapOp Integer -> MapOp Integer
    Subt  :: MapOp Integer -> MapOp Integer -> MapOp Integer
    
    LessF    :: MapOp Integer -> MapOp Integer -> MapOp Bool 
    LessS    :: MapOp Integer -> MapOp Integer -> MapOp Bool
    GreatF   :: MapOp Integer -> MapOp Integer -> MapOp Bool 
    GreatS   :: MapOp Integer -> MapOp Integer -> MapOp Bool
    Eql      :: MapOp Integer -> MapOp Integer -> MapOp Bool
    NEql     :: MapOp Integer -> MapOp Integer -> MapOp Bool
    GreatEqF :: MapOp Integer -> MapOp Integer -> MapOp Bool 
    GreatEqS :: MapOp Integer -> MapOp Integer -> MapOp Bool
    LessEqF  :: MapOp Integer -> MapOp Integer -> MapOp Bool 
    LessEqS  :: MapOp Integer -> MapOp Integer -> MapOp Bool

    DivdF :: MapOp Double -> MapOp Double -> MapOp Double
    DivdS :: MapOp Double -> MapOp Double -> MapOp Double


instance (Show a) => Show (MapOp a) where
    show (I n)  = show n
    show (IL l) = show l
    
    show (D n)  = show n
    show (DL l) = show l
    
    show (B b)  = show b
    show (BL b) = show b

    show (Add o1 o2)  = show o1 ++ " + " ++ show o2
    show (Mult o1 o2) = show o1 ++ " * " ++ show o2
    show (Subt o1 o2) = show o1 ++ " - " ++ show o2
    
    show (LessF o1 o2)    = show o1 ++ " < " ++ show o2
    show (LessS o1 o2)    = show o2 ++ " < " ++ show o1
    show (GreatF o1 o2)   = show o1 ++ " > " ++ show o2
    show (GreatS o1 o2)   = show o2 ++ " > " ++ show o1
    show (Eql o1 o2)      = show o1 ++ " == " ++ show o2
    show (NEql o1 o2)     = show o1 ++ " /= " ++ show o2
    show (GreatEqF o1 o2) = show o1 ++ " >= " ++ show o2
    show (GreatEqS o1 o2) = show o2 ++ " >= " ++ show o1
    show (LessEqF o1 o2)  = show o1 ++ " <= " ++ show o2
    show (LessEqS o1 o2)  = show o2 ++ " <= " ++ show o1

    show (DivdF o1 o2) = show o1 ++ " / " ++ show o2
    show (DivdS o1 o2) = show o2 ++ " / " ++ show o1



eval :: MapOp a -> a
eval (I n)  = n
eval (IL l) = l

eval (D n)  = n
eval (DL l) = l

eval (B l)  = l
eval (BL l) = l

eval (Add o1 o2)   = eval o1 + eval o2
eval (Mult o1 o2)  = eval o1 * eval o2
eval (Subt o1 o2)  = eval o1 - eval o2

eval (LessF o1 o2)    = eval o1 < eval o2
eval (LessS o1 o2)    = eval o2 < eval o1
eval (GreatF o1 o2)   = eval o1 > eval o2
eval (GreatS o1 o2)   = eval o2 > eval o1
eval (Eql o1 o2)      = eval o2 == eval o1
eval (NEql o1 o2)     = eval o1 /= eval o2
eval (GreatEqF o1 o2) = eval o1 >= eval o2
eval (GreatEqS o1 o2) = eval o2 >= eval o1
eval (LessEqF o1 o2)  = eval o1 <= eval o2
eval (LessEqS o1 o2)  = eval o2 <= eval o1

eval (DivdF o1 o2) = eval o1 / eval o2
eval (DivdS o1 o2) = eval o2 / eval o1

-- lambda
data LExpr where
    LAdd   :: MapOp Integer -> LExpr
    LMult  :: MapOp Integer -> LExpr
    LSubt  :: MapOp Integer -> LExpr

    LLessF    :: MapOp Integer -> LExpr
    LLessS    :: MapOp Integer -> LExpr
    LGreatF   :: MapOp Integer -> LExpr
    LGreatS   :: MapOp Integer -> LExpr
    LEql      :: MapOp Integer -> LExpr
    LNEql     :: MapOp Integer -> LExpr
    LGreatEqF :: MapOp Integer -> LExpr
    LGreatEqS :: MapOp Integer -> LExpr
    LLessEqF  :: MapOp Integer -> LExpr
    LLessEqS  :: MapOp Integer -> LExpr

    LDivdF :: MapOp Double -> LExpr
    LDivdS :: MapOp Double -> LExpr

instance Show LExpr where
    show (LAdd op)   = "(+ " ++ show op ++ ")"
    show (LMult op)  = "(* " ++ show op ++ ")"
    show (LSubt op)  = "(" ++ show op ++ "-)"

    show (LLessF op)    = "(" ++ show op ++ "<)"
    show (LLessS op)    = "(<" ++ show op ++ ")"
    show (LGreatF op)   = "(" ++ show op ++ ">)"
    show (LGreatS op)   = "(>" ++ show op ++ ")"
    show (LEql op)      = "(==" ++ show op ++ ")"
    show (LNEql op)     = "(/=" ++ show op ++ ")"
    show (LGreatEqF op) = "(" ++ show op ++ ">=)"
    show (LGreatEqS op) = "(>=" ++ show op ++ ")"
    show (LLessEqF op)  = "(" ++ show op ++ "<=)"
    show (LLessEqS op)  = "(<=" ++ show op ++ ")"

    show (LDivdF op) = "(" ++ show op ++ "/)"
    show (LDivdS op) = "(/" ++ show op ++ ")"

data InfixLExpr = InfixAdd
                | InfixSub

instance Show InfixLExpr where
    show InfixAdd = "(+)"
    show InfixSub = "(-)"


convertILExprToExpr :: LExpr -> (MapOp Integer -> MapOp Integer)
convertILExprToExpr (LAdd n)  = Add n
convertILExprToExpr (LMult n) = Mult n
convertILExprToExpr (LSubt n) = Subt n

convertBLExprToExpr :: LExpr -> (MapOp Integer -> MapOp Bool)
convertBLExprToExpr (LEql n)      = Eql n
convertBLExprToExpr (LNEql n)     = NEql n
convertBLExprToExpr (LLessF n)    = LessF n
convertBLExprToExpr (LLessS n)    = LessS n
convertBLExprToExpr (LGreatF n)   = GreatF n
convertBLExprToExpr (LGreatS n)   = GreatS n
convertBLExprToExpr (LGreatEqF n) = GreatEqF n
convertBLExprToExpr (LGreatEqS n) = GreatEqS n
convertBLExprToExpr (LLessEqF n)  = LessEqF n
convertBLExprToExpr (LLessEqS n)  = LessEqS n

convertDLExprToExpr :: LExpr -> (MapOp Double -> MapOp Double)
convertDLExprToExpr (LDivdF n) = DivdF n
convertDLExprToExpr (LDivdS n) = DivdS n

convertInfixLExprToExpr :: InfixLExpr -> (MapOp Integer -> MapOp Integer -> MapOp Integer)
convertInfixLExprToExpr InfixAdd = Add
convertInfixLExprToExpr InfixSub = Subt

-- Specific Types for Higher-Order Functions
data Expr = Map MapExpr
          | Filter FilExpr
          | Foldl FoldLExpr

instance Show Expr where
    show (Map m)    = show m
    show (Filter f) = show f
    show (Foldl f) = show f

data MapExpr = IMap (MapOp [Integer]) LExpr (MapOp [Integer])
             | DMap (MapOp [Double]) LExpr (MapOp [Double])
             | BMap (MapOp [Bool]) LExpr (MapOp [Integer])

instance Show MapExpr where
    show (IMap a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (DMap a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c
    show (BMap a b c) = "map " ++ show a ++ " " ++ show b ++ " " ++ show c

data FilExpr = IFilter (MapOp [Integer]) LExpr (MapOp [Integer])

instance Show FilExpr where
    show (IFilter a b c) = "filter " ++ show a ++ " " ++ show b ++ " " ++ show c

data FoldLExpr = IFoldL InfixLExpr (MapOp Integer) (MapOp [Integer])

instance Show FoldLExpr where
    show (IFoldL a b c) = "filter " ++ show a ++ " " ++ show b ++ " " ++ show c




