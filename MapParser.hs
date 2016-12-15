{-# LANGUAGE GADTs, TemplateHaskell #-}

{- Davin Chia, Kit Tse
 - CS413 Final Project
 - 
 - Parser.hs
 -}

module MapParser where

import Language.Haskell.TH
import GHC.Num
import MapExpr

-- input validation stuff: validateASTStructure
--                         validateIntegerType
--                         validateOp

-- validates the abstract syntax tree structure
validateASTStructure :: Exp -> Bool
validateASTStructure (AppE (AppE (VarE name) (InfixE (Just _) (VarE _) Nothing)) 
  (ListE list)) = name == mkName "map" || name == mkName "filter"
validateASTStructure (AppE (AppE (VarE name) (InfixE Nothing (VarE _) (Just _)))
   (ListE list)) = name == mkName "map" || name == mkName "filter"
validateASTStructure _ = False

-- type checks                                                                  
validateIntegerType :: Exp -> Bool
validateIntegerType (AppE (AppE _ (InfixE (Just (LitE (IntegerL _))) _ _))
    (ListE xs)) = validateIntegerType' xs
validateIntegerType (AppE (AppE _ (InfixE _ _ (Just (LitE (IntegerL _)))))
    (ListE xs)) = validateIntegerType' xs
validateIntegerType _ = False

-- checks that all elemnts in a list are all Integers
validateIntegerType' :: [Exp] -> Bool
validateIntegerType' [] = True
validateIntegerType' ((LitE (IntegerL _)): xs) = validateIntegerType' xs
validateIntegerType' _ = False

-- validates operation
validateOp :: Exp -> Bool
validateOp (AppE (AppE _ (InfixE _ (VarE x) _)) _) = x == mkName "+"
                                                  || x == mkName "*"
                                                  || x == mkName "-"
                                                  || x == mkName "/"
                                                  || x == mkName "=="
                                                  || x == mkName "/="
                                                  || x == mkName "<"
                                                  || x == mkName ">"

-- Parsing stuff
parseAST :: Exp -> Expr
parseAST (AppE (AppE (VarE func) op@(InfixE _ (VarE x) _)) l) 
  | func == mkName "map" && 
    (x == mkName "-" || x == mkName "+" || x == mkName "==") 
      = Map (IMap (IL []) (parseOP op) (parseIList l))
  | func == mkName "map" && x == mkName "/" 
      = Map (DMap (DL []) (parseOP op) (parseDList l))
  | func == mkName "filter" && 
     (x == mkName "==" || x == mkName "/=" || x == mkName "<" || x == mkName ">")
      = Filter (IFilter (IL []) (parseOP op) (parseIList l))

parseOP :: Exp -> LExpr
parseOP (InfixE Nothing (VarE x) (Just (LitE v)))
    | x == mkName "+"  = LAdd   (parseIV v)
    | x == mkName "*"  = LMult  (parseIV v)
    | x == mkName "-"  = LSubt  (parseIV v)
    | x == mkName "/"  = LDivdS (parseDV v)
    | x == mkName "==" = LEql   (parseIV v)
    | x == mkName "/=" = LNEql  (parseIV v)
    | x == mkName "<"  = LLessS  (parseIV v)
    | x == mkName ">"  = LGreatS  (parseIV v)
parseOP (InfixE (Just (LitE v)) (VarE x) Nothing)
    | x == mkName "+"  = LAdd   (parseIV v)
    | x == mkName "*"  = LMult  (parseIV v)
    | x == mkName "-"  = LSubt  (parseIV v)
    | x == mkName "/"  = LDivdF (parseDV v)
    | x == mkName "==" = LEql   (parseIV v)
    | x == mkName "/=" = LNEql  (parseIV v)
    | x == mkName ">"  = LGreatF  (parseIV v)

-- TH variable to custom data type
-- How to return Op of different type?
parseIV :: Lit -> MapOp Integer
parseIV (IntegerL v) = I v

parseDV :: Lit -> MapOp Double
parseDV (IntegerL v) = D (fromIntegral v)

-- How to return Op of different array types?
parseIList :: Exp -> MapOp [Integer]
parseIList (ListE a) = IL (map (\(LitE (IntegerL e)) -> e) a)

parseDList :: Exp -> MapOp [Double]
parseDList (ListE a) = DL (map (\(LitE (IntegerL e)) -> fromIntegral e) a)
