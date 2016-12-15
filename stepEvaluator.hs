{-# LANGUAGE GADTs, TemplateHaskell #-}

{- Davin Chia, Kit Tse
 - CS413 Final Project
 - 
 - stepMachine.hs
 -}

import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax

import MapParser
import MapExpr

main :: IO ()
main = do putStrLn "Please enter source code: "
          code <- getLine
          let Right ast = parseExp code
          putStrLn (show ast)
          -- checks to make sure AST produced from source code is as expected;
          -- program quits if not
          let validStructure = validateASTStructure ast
          if (not validStructure)
             then do putStrLn "Unexpected src code format. Please try again.\n"
                     main
          else do putStrLn "Structure: check."

          -- checks to make sure source code deals with Integer type (for now);
          -- quits if not
          let validType = validateIntegerType ast
          if (not validType)
             then do putStrLn "Invalid type. Please try again.\n"
                     main
          else do putStrLn "Type: check."

          -- checks to make sure source code uses allowed operator;
          -- currently supports: +, *, -
          -- quits if not
          let validOp = validateOp ast
          if (not validOp)
             then do putStrLn "Invalid operation. Please try again.\n"
                     main
          else do putStrLn "Operation: check.\n"

          -- turns Exp from source code into our own Expr;
          -- if conversation/translation is successful, evaluate it step by
          -- step
          let result = parseAST ast
          run result
          
          -- asks user for another line of source code
          main

nextState :: Expr -> Expr
nextState m@(Map (IMap cur lExp args)) = Map iMap
    where baseCase  = eval cur
          nextVal   = I (head (eval args))
          remainder = drop 1 (eval args)
          exp       = convertILExprToExpr lExp
          iMap      = IMap (IL (baseCase ++ [ eval . exp $ nextVal ])) lExp
                        (IL remainder)
nextState m@(Map (DMap cur lExp args)) = Map dMap
    where baseCase  = eval cur
          nextVal   = D (head (eval args))
          remainder = drop 1 (eval args)
          exp       = convertDLExprToExpr lExp
          dMap      = DMap (DL (baseCase ++ [ eval . exp $ nextVal ])) lExp
                        (DL remainder)
nextState m@(Filter (IFilter cur lExp args)) = Filter iFil
    where baseCase  = eval cur
          nextVal   = I (head (eval args))
          remainder = drop 1 (eval args)
          predicate = convertBLExprToExpr lExp
          result    = eval . predicate $ nextVal
          iFil      = IFilter (IL ((\r -> if r then baseCase ++ [ eval $ nextVal ] else baseCase) result)) lExp
                        (IL remainder)

-- run prints the current state and calculates the next state
run :: Expr -> IO ()
run (Map (IMap cur _ (IL []))) = putStrLn $ show cur
run e@(Map m@(IMap cur lExp args)) = do
    let bc = eval cur
    let nextVal = I (head (eval args))
    let remainder = drop 1 (eval args)
    let op = convertILExprToExpr lExp
    let exp = op nextVal
    let evalExp = eval exp
    let state = nextState e

    putStrLn $ show m
    putStrLn $ "   1. " ++ show bc ++ " : " ++ show exp ++ " : map "
               ++ show lExp ++ " " ++ show remainder 
    putStrLn $ "   2. " ++ show bc ++ " : " ++ show evalExp ++ " : map "
               ++ show lExp ++ " " ++ show remainder
    putStrLn $ "   3. " ++ show (bc ++ [evalExp]) ++ " : map " ++ show lExp
               ++ " "  ++ show remainder ++ "\n"
    run state

run (Map (DMap cur _ (DL []))) = putStrLn $ show cur
run e@(Map m@(DMap cur lExp args)) = do
    let bc = eval cur
    let nextVal = D (head (eval args))
    let remainder = drop 1 (eval args)
    let op = convertDLExprToExpr lExp
    let exp = op nextVal
    let evalExp = eval exp
    let state = nextState e

    putStrLn $ show m
    putStrLn $ "   1. " ++ show bc ++ " : " ++ show exp ++ " : map "
               ++ show lExp ++ " " ++ show remainder 
    putStrLn $ "   2. " ++ show bc ++ " : " ++ show evalExp ++ " : map "
               ++ show lExp ++ " " ++ show remainder
    putStrLn $ "   3. " ++ show (bc ++ [evalExp]) ++ " : map " ++ show lExp
               ++ " "  ++ show remainder ++ "\n"
    run state

run (Filter (IFilter cur _ (IL []))) = putStrLn $ show cur
run e@(Filter m@(IFilter cur lExp args)) = do
    let bc = eval cur
    let nextVal = I (head (eval args))
    let remainder = drop 1 (eval args)

    let predicate = convertBLExprToExpr lExp
    let exp = predicate $ nextVal
    let result = eval exp
    let state = nextState e

    putStrLn $ show m
    putStrLn $ "   1. " ++ show bc ++ " : " ++ show exp ++ " : filter "
              ++ show lExp ++ " " ++ show remainder 
    putStrLn $ "   2. " ++ show bc ++ " : " ++ show result ++ " : filter "
              ++ show lExp ++ " " ++ show remainder
    if (result)
      then do
            putStrLn $ "   3. " ++ show bc ++ " : " ++ show nextVal ++ " : filter " ++ show lExp
              ++ " "  ++ show remainder
            putStrLn $ "   4. " ++ show (bc ++ [eval nextVal]) ++ " : filter " ++ show lExp
              ++ " "  ++ show remainder ++ "\n"
      else do
            putStrLn $ "   3. " ++ show bc ++ " : None : filter " ++ show lExp
              ++ " "  ++ show remainder
            putStrLn $ "   4. " ++ show bc ++ " : filter " ++ show lExp
              ++ " "  ++ show remainder ++ "\n"

    run state



