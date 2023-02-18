{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads
import Control.Monad

-- AST Definition

data TYPELANG = TNum
              | TBool 
              deriving (Show,Eq)

data TERMLANG = Num Int 
              | Plus TERMLANG TERMLANG 
              | Minus TERMLANG TERMLANG 
              | Mult TERMLANG TERMLANG 
              | Div TERMLANG TERMLANG 
              | Boolean Bool 
              | And TERMLANG TERMLANG 
              | Or TERMLANG TERMLANG 
              | Leq TERMLANG TERMLANG 
              | IsZero TERMLANG 
              | If TERMLANG TERMLANG TERMLANG 
              deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Evaluation Functions

-- Exercise 1
evalM :: TERMLANG -> (Maybe TERMLANG)
evalM (Num x) = if x<0 then Nothing else Just (Num x)
evalM (Plus l r) = do {(Num l') <- evalM l;
                       (Num r') <- evalM r;
                       return (Num (l'+r') )}
evalM (Minus l r) = do {(Num l') <- evalM l;
                        (Num r') <- evalM r;
                        if (l'-r')<0 then Nothing else return (Num (l'-r') )}
evalM (Mult l r) = do {(Num l') <- evalM l;
                       (Num r') <- evalM r;
                       return (Num (l'*r') )}
evalM (Div l r) = do {(Num l') <- evalM l;
                      (Num r') <- evalM r;
                      if r' == 0 then Nothing else return (Num (l' `div` r') )}
evalM (Boolean x) = Just (Boolean x)
evalM (And l r) = do {(Boolean l') <- evalM l;
                      (Boolean r') <- evalM r;
                      return (Boolean (l'&&r') )}
evalM (Or l r) = do {(Boolean l') <- evalM l;
                     (Boolean r') <- evalM r;
                     return (Boolean (l'||r') )}
evalM (Leq l r) = do {(Num l') <- evalM l;
                      (Num r') <- evalM r;
                      if (l'-r')<=0 then return (Boolean True) else return (Boolean False)}
evalM (IsZero x) = do {(Num x') <- evalM x;
                       if x'==0 then return (Boolean True) else return (Boolean False)} -- It returns Nothing when x<0
evalM (If i t e) = do {(Boolean i') <- evalM i;
                       if i' then return t else return e} -- Do I need to check if t and e are the same type?

-- Exercise 2
typeofM :: TERMLANG -> Maybe TYPELANG
typeofM (Num x) = if x<0 then Nothing else Just TNum
typeofM (Plus l r) = do {l' <- typeofM l;
                         r' <- typeofM r;
                         if (l'==TNum)&&(r'==TNum) then Just TNum else Nothing}
typeofM (Minus l r) = do {(Num l') <- evalM l;
                          (Num r') <- evalM r;
                          if (l'-r')<0 then Nothing else Just TNum}
typeofM (Mult l r) = do {l' <- typeofM l;
                         r' <- typeofM r;
                         if (l'==TNum)&&(r'==TNum) then Just TNum else Nothing}
typeofM (Div l r) = do {l' <- typeofM l;
                       r' <- typeofM r;
                       if (l'==TNum)&&(r'==TNum) then Just TNum else Nothing}
typeofM (Boolean x) = Just TBool
typeofM (And l r) = do {l' <- typeofM l;
                       r' <- typeofM r;
                       if (l'==TBool)&&(r'==TBool) then Just TBool else Nothing}
typeofM (Or l r) = do {l' <- typeofM l;
                      r' <- typeofM r;
                      if (l'==TBool)&&(r'==TBool) then Just TBool else Nothing}
typeofM (Leq l r) = do {l' <- typeofM l;
                       r' <- typeofM r;
                       if (l'==TBool)&&(r'==TBool) then Just TBool else Nothing}
typeofM (IsZero x) = do {x' <- typeofM x;
                        if (x'==TNum) then Just TBool else Nothing}
typeofM (If i t e) = do {(Boolean i') <- evalM i;
                         if i'==True then (typeofM t) else (typeofM e)} -- Do I need to check if t and e are the same type?

-- Exercise 3
interpTypeEval :: TERMLANG -> Maybe TERMLANG
interpTypeEval x = do {x' <- typeofM x;
                       if (x'==TNum)||(x'==TBool) then (evalM x) else Nothing}

-- Part 2: Optimizer

-- Exercise 1
optimize :: TERMLANG -> TERMLANG
optimize (Num n) = (Num n)
optimize (Plus l (Num 0)) = (optimize l)
optimize (Plus l r) = (Plus (optimize l) (optimize r))
optimize (Minus l r) = (Minus (optimize l) (optimize r))
optimize (Boolean b) = (Boolean b)
optimize (Mult l (Num 0)) = (Num 0)
optimize (Mult l (Num 1)) = (optimize l)
optimize (Mult l r) = (Mult (optimize l) (optimize r))
optimize (Div l (Num 1)) = (optimize l)
optimize (Div l r) = (if (optimize l)==(optimize r) then (Num 1) else (Div (optimize l) (optimize r)))
optimize (And l (Boolean False)) = (Boolean False)
optimize (And l r) = (And (optimize l) (optimize r))
optimize (Or l (Boolean True)) = (Boolean True)
optimize (Or l r) = (Or (optimize l) (optimize r))
optimize (Leq l r) = (Leq (optimize l) (optimize r)) 
optimize (IsZero x) = (IsZero (optimize x))
optimize (If (Boolean True) t e) = (optimize t)
optimize (If (Boolean False) t e) = (optimize e)
optimize (If i t e) = (If (optimize i) (optimize t) (optimize e))

-- Exercise 2
interpOptEval :: TERMLANG -> Maybe TERMLANG
interpOptEval x = evalM (optimize x)
