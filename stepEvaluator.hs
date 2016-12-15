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

          -- checks to make sure source code uses the addition operator;
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

run' :: MapExpr -> MapExpr
run' a@(Map _ _ (IL [])) = a
run' m@(Map cur lExp args) = Map (IL (baseCase ++ [ eval . exp $ nextVal ]))
                                 lExp (IL remainder)
    where baseCase = eval cur
          nextVal  = I (head (eval args))
          remainder = drop 1 (eval args)
          exp = convertLExprToExpr lExp


run :: MapExpr -> IO ()
run (Map cur _ (IL [])) = putStrLn $ show cur
run m@(Map cur lExp args) = do
    let bc = eval cur
    let nextVal = I (head (eval args))
    let remainder = drop 1 (eval args)
    let op = convertLExprToExpr lExp
    let exp = op nextVal
    let evalExp = eval exp
    let state = run' m

    putStrLn $ show m
    putStrLn $ "   1. " ++ show bc ++ " : " ++ show exp ++ " : map "
               ++ show lExp ++ " " ++ show remainder 
    putStrLn $ "   2. " ++ show bc ++ " : " ++ show evalExp ++ " : map "
               ++ show lExp ++ " " ++ show remainder
    putStrLn $ "   3. " ++ show (bc ++ [evalExp]) ++ " : map " ++ show lExp
               ++ " "  ++ show remainder ++ "\n"
    run state

m = Map (IL []) (LAdd (I 10)) (IL [1,2,3,4])
