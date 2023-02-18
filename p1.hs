{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- AST Definition

data CALC where
  Num :: Int -> CALC
  Plus :: CALC -> CALC -> CALC
  Minus :: CALC -> CALC -> CALC
  Mult :: CALC -> CALC -> CALC
  Div :: CALC -> CALC -> CALC
  Sqr :: CALC -> CALC
  deriving (Show,Eq)

-- Evaluation Functions
-- Replace the bodies of these 4 functions with your implementations.
-- Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser (below).

-- Exercise 1
evalErr :: CALC -> Int
evalErr (Num x) = if x<0 
        then error "Number needs to be natural."
        else x
evalErr (Plus l r) = (evalErr l) + (evalErr r)
evalErr (Minus l r) = let x = ( (evalErr l) - (evalErr r) ) in
                      if x<0
                        then error "Result is smaller than 0."
                        else x
evalErr (Mult l r) = (evalErr l) * (evalErr r)
evalErr (Div l r) = if (evalErr r) == 0
                      then error "Cannot be divided by 0."
                      else (evalErr l) `div` (evalErr r)
evalErr (Sqr x) = (evalErr x) * (evalErr x)

-- Exercise 2
evalMaybe :: CALC -> Maybe Int
evalMaybe (Num x) = if x<0 then Nothing else (Just x)
evalMaybe (Plus l r) = case (evalMaybe l) of
          Nothing -> Nothing
          Just l' -> case (evalMaybe r) of
                Nothing -> Nothing
                Just r' -> Just (l'+r')
evalMaybe (Minus l r) = case (evalMaybe l) of 
          Nothing -> Nothing
          Just l' -> case (evalMaybe r) of
                Nothing -> Nothing
                Just r' -> if (l'-r') <0
                              then error "The result is smaller than 0."
                              else Just (l'-r')
evalMaybe (Mult l r) = case (evalMaybe l) of
          Nothing -> Nothing
          Just l' -> case (evalMaybe r) of
                Nothing -> Nothing
                Just r' -> Just (l'*r')
evalMaybe (Div l r) = case (evalMaybe l) of
          Nothing -> Nothing
          Just l' -> case (evalMaybe r) of
                Nothing -> Nothing
                Just 0 -> error "Cannot be divided by 0."
                Just r' -> Just (l' `div` r')
evalMaybe (Sqr x) = case (evalMaybe x) of
          Nothing -> Nothing
          Just x' -> Just (x'*x')

-- Exercise 3
evalM :: CALC -> Maybe Int
evalM (Num x) = if x<0 then Nothing else (Just x)
evalM (Plus l r) = do {l' <- evalM l;
                       r' <- evalM r;
                       return (l'+r')}
evalM (Minus l r) = do {l' <- evalM l;
                        r' <- evalM r;
                        if l'-r'<0 then Nothing else return (l'-r')}
evalM (Mult l r) = do {l' <- evalM l;
                       r' <- evalM r;
                       return (l'*r')}
evalM (Div l r) = do {l' <- evalM l;
                      r' <- evalM r;
                      if r' == 0 then error "Cannot be divided by 0." else return (l' `div` r')}
evalM (Sqr x) = do {x' <- evalM x;
                    return (x'*x')}                                                

-- Exercise 4
interpCALC :: String -> Maybe Int
interpCALC x = evalM (parseCALC x)

--
-- CALC Parser (Requires Parsec included above)
--

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "sqr"]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser CALC
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser CALC
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

sqrExpr :: Parser CALC
sqrExpr = do reserved lexer "sqr"
             x <- expr
             return (Sqr x)
                     

term = parens lexer expr
       <|> numExpr
       <|> sqrExpr

-- Parser invocation
-- Call parseCALC to parse a string into the CALC data structure.

parseCALC = parseString expr
