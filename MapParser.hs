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

-- checks that all elemnts in a list are all Integers
validateIntegerType' :: [Exp] -> Bool
validateIntegerType' [] = True
validateIntegerType' ((LitE (IntegerL _)): xs) = validateIntegerType' xs
validateIntegerType' _ = False

-- validates operation
validateOp :: Exp -> Bool
validateOp (AppE (AppE _ (InfixE _ (VarE x) _)) _) = x == mkName "+"
                                                   || x == mkName "*"


-- Parsing stuff
parseAST :: Exp -> MapExpr
parseAST (AppE (AppE (VarE map) op) l) = Map (IL []) (parseOP op) (parseList l)

parseOP :: Exp -> LExpr
parseOP (InfixE Nothing (VarE x) (Just (LitE v)))
    | x == mkName "+" = LAdd (parseV v)
    | otherwise       = LMult (parseV v)
parseOP (InfixE (Just (LitE v)) (VarE x) Nothing)
    | x == mkName "+" = LAdd (parseV v)
    | otherwise       = LMult (parseV v)

-- TH variable to custom data type
-- How to return Op of different type?
parseV :: Lit -> MapOp Integer
parseV (IntegerL v) = I v

-- How to return Op of different array types?
parseList :: Exp -> MapOp [Integer]
parseList (ListE a) = IL (map (\(LitE (IntegerL e)) -> e) a)
