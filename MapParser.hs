{-# LANGUAGE GADTs, TemplateHaskell #-}

{- Davin Chia, Kit Tse
 - CS413 Final Project
 - 
 - Parser.hs
 -}

module MapParser where

import Language.Haskell.TH
import MapExpr

-- input validation stuff: validateASTStructure
--                         validateIntegerType
--                         validateOp

-- validates the abstract syntax tree structure
validateASTStructure :: Exp -> Bool
validateASTStructure (AppE (AppE (VarE name) (InfixE (Just _) (VarE e) Nothing)) 
  (ListE list))
  | name == mkName "filter" && 
    (e == mkName "+" || e == mkName "-" || e == mkName "/" || e == mkName "*")
    = False
  | name == mkName "map" || name == mkName "filter" = True
validateASTStructure (AppE (AppE (VarE name) (InfixE Nothing (VarE e) (Just _)))
  (ListE list))
  | name == mkName "filter" && 
    (e == mkName "+" || e == mkName "-" || e == mkName "/" || e == mkName "*")
    = False
  | name == mkName "map" || name == mkName "filter" = True
validateASTStructure (AppE (AppE (AppE (VarE name) (VarE e)) (LitE _)) (ListE list))
  | name == mkName "foldl" = True
validateASTStructure _ = False

-- type checks                                                                  
validateIntegerType :: Exp -> Bool
validateIntegerType (AppE (AppE _ (InfixE (Just (LitE (IntegerL _))) _ _))
    (ListE xs)) = validateIntegerType' xs
validateIntegerType (AppE (AppE _ (InfixE _ _ (Just (LitE (IntegerL _)))))
    (ListE xs)) = validateIntegerType' xs
validateIntegerType (AppE (AppE (AppE (VarE _) (VarE _)) (LitE (IntegerL _))) 
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
                                                  || x == mkName ">="
                                                  || x == mkName "<="
validateOp (AppE (AppE (AppE _ (VarE x)) _) _) = x == mkName "+"
                                              || x == mkName "-"

-- Turn TH Exp into Custom Data Type
parseAST :: Exp -> Expr
parseAST (AppE (AppE (VarE func) op@(InfixE _ (VarE x) _)) l) 
  | func == mkName "map" && 
    (x == mkName "-" || x == mkName "+" || x == mkName "*") 
      = Map (IMap (IL []) (parseOP op) (parseIList l))
  | func == mkName "map" && 
    (x == mkName "==" || x == mkName "/=" || x == mkName "<" || 
      x == mkName ">" || x == mkName ">=" || x == mkName "<=")
      = Map (BMap (BL []) (parseOP op) (parseIList l))
  | func == mkName "map" && x == mkName "/" 
      = Map (DMap (DL []) (parseOP op) (parseDList l))
  | func == mkName "filter" && 
    (x == mkName "==" || x == mkName "/=" || x == mkName "<" || 
      x == mkName ">" || x == mkName ">=" || x == mkName "<=")
      = Filter (IFilter (IL []) (parseOP op) (parseIList l))
parseAST (AppE (AppE (AppE (VarE func) ifx@(VarE x)) (LitE bc)) l) 
  | func == mkName "foldl" && (x == mkName "+" || x == mkName "-") 
    = Foldl (IFoldL (parseInfix ifx) (parseIV bc) (parseIList l))

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
    | x == mkName ">="  = LGreatEqS  (parseIV v)
    | x == mkName "<="  = LLessEqS  (parseIV v)
parseOP (InfixE (Just (LitE v)) (VarE x) Nothing)
    | x == mkName "+"  = LAdd   (parseIV v)
    | x == mkName "*"  = LMult  (parseIV v)
    | x == mkName "-"  = LSubt  (parseIV v)
    | x == mkName "/"  = LDivdF (parseDV v)
    | x == mkName "==" = LEql   (parseIV v)
    | x == mkName "/=" = LNEql  (parseIV v)
    | x == mkName ">"  = LGreatF  (parseIV v)
    | x == mkName ">=" = LGreatEqF  (parseIV v)
    | x == mkName "<=" = LLessEqF  (parseIV v)

parseInfix :: Exp -> InfixLExpr
parseInfix (VarE x)
  | x == mkName "+" = InfixAdd
  | x == mkName "-" = InfixSub

-- TH variable to custom data type
parseIV :: Lit -> MapOp Integer
parseIV (IntegerL v) = I v

parseDV :: Lit -> MapOp Double
parseDV (IntegerL v) = D (fromIntegral v)

parseIList :: Exp -> MapOp [Integer]
parseIList (ListE a) = IL (map (\(LitE (IntegerL e)) -> e) a)

parseDList :: Exp -> MapOp [Double]
parseDList (ListE a) = DL (map (\(LitE (IntegerL e)) -> fromIntegral e) a)
