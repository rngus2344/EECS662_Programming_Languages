{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions
data TERMLANG where
  Num :: Int -> TERMLANG
  Id :: String -> TERMLANG
  Plus :: TERMLANG -> TERMLANG -> TERMLANG
  Minus :: TERMLANG -> TERMLANG -> TERMLANG
  Mult :: TERMLANG -> TERMLANG -> TERMLANG
  Div :: TERMLANG -> TERMLANG -> TERMLANG
  If0 :: TERMLANG -> TERMLANG -> TERMLANG -> TERMLANG
  Lambda :: String -> TERMLANG -> TERMLANG
  App :: TERMLANG -> TERMLANG -> TERMLANG
  deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  ClosureV :: String -> TERMLANG -> ValueEnv -> VALUELANG
  deriving (Show,Eq)
  
type TermEnv = [(String,TERMLANG)]
type ValueEnv = [(String,VALUELANG)]
 
data EXTLANG where
  NumX :: Int -> EXTLANG
  PlusX :: EXTLANG -> EXTLANG -> EXTLANG
  MinusX :: EXTLANG -> EXTLANG -> EXTLANG
  MultX :: EXTLANG -> EXTLANG -> EXTLANG
  DivX :: EXTLANG -> EXTLANG -> EXTLANG
  If0X :: EXTLANG -> EXTLANG -> EXTLANG -> EXTLANG
  LambdaX :: String -> EXTLANG -> EXTLANG
  AppX :: EXTLANG -> EXTLANG -> EXTLANG
  BindX :: String -> EXTLANG -> EXTLANG -> EXTLANG
  IdX :: String -> EXTLANG
  deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: TERMLANG (no bind)

-- Exercise 1:
evalDyn :: TermEnv -> TERMLANG -> (Maybe TERMLANG)
evalDyn e (Num x) = Just (Num x)
evalDyn e (Id i) = (lookup i e)
evalDyn e (Plus l r) = do {(Num l') <- evalDyn e l;
                          (Num r') <- evalDyn e r;
                          return (Num (l' + r'))}
evalDyn e (Minus l r) = do {(Num l') <- evalDyn e l;
                           (Num r') <- evalDyn e r;
                           return (Num (l' - r'))}
evalDyn e (Mult l r) = do {(Num l') <- evalDyn e l;
                          (Num r') <- evalDyn e r;
                          return (Num (l' * r'))}
evalDyn e (Div l r) = do {(Num l') <- evalDyn e l;
                         (Num r') <- evalDyn e r;
                         if r' == 0 then Nothing else return (Num (l' `div` r'))}
evalDyn e (If0 i t e') = do {(Num i') <- evalDyn e i;
                            if i'==0 then (evalDyn e t) else (evalDyn e e')}
evalDyn e (Lambda i b) = return (Lambda i b)
evalDyn e (App f a) = do {(Lambda i b) <- evalDyn e f;
                         v <- evalDyn e a;
                         evalDyn ((i,v):e) b}

-- Exercise 2:
evalStat :: ValueEnv -> TERMLANG -> (Maybe VALUELANG)
evalStat e (Num x) = Just (NumV x)
evalStat e (Id i) = (lookup i e)
evalStat e (Plus l r) = do {(NumV l') <- evalStat e l;
                           (NumV r') <- evalStat e r;
                           return (NumV (l' + r')) }
evalStat e (Minus l r) = do {(NumV l') <- evalStat e l;
                            (NumV r') <- evalStat e r;
                            return (NumV (l' - r')) }
evalStat e (Mult l r) = do {(NumV l') <- evalStat e l;
                           (NumV r') <- evalStat e r;
                           return (NumV (l' * r'))}
evalStat e (Div l r) = do {(NumV l') <- evalStat e l;
                          (NumV r') <- evalStat e r;
                          if r' == 0 then Nothing else return (NumV (l' `div` r'))}
evalStat e (If0 i t e') = do {(NumV i') <- evalStat e i;
                             if i'==0 then (evalStat e t) else (evalStat e e')}
evalStat e (Lambda i b) = Just (ClosureV i b e)
evalStat e (App f a) = do {(ClosureV i b e) <- evalStat e f;
                          a' <- evalStat e a;
                          evalStat ((i, a'):e) b}

-- Part 2: Elaboration

-- Exercise 1:
elabTerm :: EXTLANG -> TERMLANG
elabTerm (NumX x) = Num x
elabTerm (PlusX l r) = Plus (elabTerm l) (elabTerm r)
elabTerm (MinusX l r) = Minus (elabTerm l) (elabTerm r)
elabTerm (MultX l r) = Mult (elabTerm l) (elabTerm r)
elabTerm (DivX l r) = Div (elabTerm l) (elabTerm r)
elabTerm (If0X i t e) = If0 (elabTerm i) (elabTerm t) (elabTerm e)
elabTerm (LambdaX i b) = Lambda i (elabTerm b)
elabTerm (AppX f a) = App (elabTerm f) (elabTerm a)
elabTerm (BindX i v b) = (App (Lambda i (elabTerm b)) (elabTerm v))
elabTerm (IdX i) = Id i

-- Exercise 2:
evalTerm :: ValueEnv -> EXTLANG -> (Maybe VALUELANG)
evalTerm e x = evalStat e (elabTerm x)


