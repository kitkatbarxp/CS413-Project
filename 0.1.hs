{-# LANGUAGE GADTs, BangPatterns, TemplateHaskell #-}
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Data.Typeable
import Language.Haskell.TH.Syntax
import Control.Monad
import System.Exit

main :: IO ()
main = do putStrLn "Please enter source code: "
          code <- getLine
          let Right ast = parseExp code
            
          putStrLn $ show ast
          -- checks to make sure AST produced from source code is as expected;
          -- program quits if not
          let validStructure = validateASTStructure ast
          when (not validStructure) exitFailure

          -- checks to make sure source code deals with Integer type (for now);
          -- quits if not
          let validType = validateIntegerType ast
          when (not validType) exitFailure

          -- checks to make sure source code uses the addition operator;
          -- quits if not
          let validOp = validateOp ast
          when (not validOp) exitFailure

          -- turns Exp from source code into our own Expr;
          -- if conversation/translation is successful, evaluate it step by
          -- step
          let result = parseAST ast
          case result of
                Nothing -> exitFailure
                Just x -> run x
          
          -- asks user for another line of source code
          main

-- validate the abstract syntax tree structure
validateASTStructure :: Exp -> Bool
validateASTStructure (AppE (AppE (VarE map) (InfixE (Just _) (VarE _) Nothing)) 
    (ListE list)) = True
validateASTStructure (AppE (AppE (VarE map) (InfixE Nothing (VarE _) (Just _))) 
    (ListE list)) = True
validateASTStructure _ = False

-- type checks
validateIntegerType :: Exp -> Bool
validateIntegerType (AppE (AppE _ (InfixE (Just (LitE (IntegerL _))) _ _)) 
    (ListE xs)) = validateIntegerType' xs
validateIntegerType (AppE (AppE _ (InfixE _ _ (Just (LitE (IntegerL _)))))
    (ListE xs)) = validateIntegerType' xs
validateIntegerType _ = False

-- validates the operation
validateOp :: Exp -> Bool
validateOp (AppE (AppE _ (InfixE _ x _)) _) = x == VarE (mkName "+")

-- checks that all elemnts in a list are all Integers
validateIntegerType' :: [Exp] -> Bool
validateIntegerType' [] = True
validateIntegerType' ((LitE (IntegerL _)): xs) = validateIntegerType' xs
validateIntegerType' _ = False


-- Need to check if op can run with list type 
-- Need to lift out of context
-- How to return null type if constructor gets Nothing - Smart Constructor?
parseAST :: Exp -> Maybe Expr 
parseAST (AppE (AppE (VarE map) op) l) = Just (Map (IL []) (parseOP op) (parseList l))
parseAST _ = Nothing

-- What if op is not valid?
parseOP :: Exp -> LExpr
parseOP (InfixE Nothing (VarE (+)) (Just (LitE v))) = LAdd (parseV v)
parseOP (InfixE (Just (LitE v)) (VarE (+)) Nothing) = LAdd (parseV v)
-- parseOP _ = Nothing

-- TH variable to custom data type
-- How to return Op of different type?
parseV :: Lit -> Op Integer
parseV (IntegerL v) = I v

-- How to return Op of different array types?
parseList :: Exp -> Op [Integer]
parseList (ListE a) = IL (map (\(LitE (IntegerL e)) -> e) a)

data Op a where 
    I      :: Integer -> Op Integer -- all the possible numbers
    IL     :: [Integer] -> Op [Integer]
    Add    :: Op Integer -> Op Integer -> Op Integer

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
    LAdd :: Op Integer -> LExpr

instance Show LExpr where
    show (LAdd op) = "(+ " ++ show op ++ ")"

lAddToAdd :: LExpr -> (Op Integer -> Op Integer)
lAddToAdd (LAdd n) = Add n

-- map
data Expr = Map (Op [Integer]) LExpr (Op [Integer])

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
