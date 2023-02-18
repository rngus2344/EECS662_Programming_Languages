{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions

data TYPELANG = TNum
              | TBool
                deriving (Show,Eq)

data TERMLANG = Num  Int 
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
              | Bind String TERMLANG TERMLANG
              | Id String 
                deriving (Show,Eq)
  

type Env = [(String,TERMLANG)]

type Cont = [(String,TYPELANG)]


-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Adding Booleans
subst :: String -> TERMLANG -> TERMLANG -> TERMLANG
subst i v (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Boolean x) = (Boolean x)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (Id i') = if i==i' then v else (Id i')
subst i v (Bind i' v' b') = if i==i'
                               then (Bind i' (subst i v v') b')
                               else (Bind i' (subst i v v') (subst i v b'))

-- Exercise 1
evalS :: TERMLANG -> (Maybe TERMLANG)
evalS (Num x) = if x<0 then Nothing else Just (Num x)
evalS (Plus l r) = do {(Num l') <- evalS l;
                       (Num r') <- evalS r;
                       return (Num (l'+r') )}
evalS (Minus l r) = do {(Num l') <- evalS l;
                        (Num r') <- evalS r;
                        if (l'-r')<0 then Nothing else return (Num (l'-r') )}
evalS (Mult l r) = do {(Num l') <- evalS l;
                       (Num r') <- evalS r;
                       return (Num (l'*r') )}
evalS (Div l r) = do {(Num l') <- evalS l;
                      (Num r') <- evalS r;
                      if r' == 0 then Nothing else return (Num (l' `div` r') )}
evalS (Boolean x) = Just (Boolean x)
evalS (And l r) = do {(Boolean l') <- evalS l;
                      (Boolean r') <- evalS r;
                      return (Boolean (l'&&r') )}
evalS (Or l r) = do {(Boolean l') <- evalS l;
                     (Boolean r') <- evalS r;
                     return (Boolean (l'||r') )}
evalS (Leq l r) = do {(Num l') <- evalS l;
                      (Num r') <- evalS r;
                      if (l'-r')<=0 then return (Boolean True) else return (Boolean False)}
evalS (IsZero x) = do {(Num x') <- evalS x;
                       if x'==0 then return (Boolean True) else return (Boolean False)} -- It returns Nothing when x<0
evalS (If i t e) = do {(Boolean i') <- evalS i;
                       if i'==True then return t else return e} -- Do I need to check if t and e are the same type?
evalS (Bind i v b) = do {v' <- evalS v;
                         (evalS (subst i v' b))}
evalS (Id i) = Nothing

-- Exercise 2
evalM :: Env -> TERMLANG -> (Maybe TERMLANG)
evalM e (Num x) = return (Num x)
evalM e (Plus l r) = do {(Num l') <- evalM e l;
                         (Num r') <- evalM e r;
                         return (Num (l' + r'))}
evalM e (Minus l r) = do {(Num l') <- evalM e l;
                          (Num r') <- evalM e r;
                          return (Num (l' - r'))}
evalM e (Mult l r) = do {(Num l') <- evalM e l;
                         (Num r') <- evalM e r;
                         return (Num (l' * r'))}
evalM e (Div l r) = do {(Num l') <- evalM e l;
                        (Num r') <- evalM e r;
                        if r' == 0 then Nothing else return (Num (l' `div` r'))}
evalM e (Boolean x) = Just (Boolean x)
evalM e (And l r) = do {(Boolean l') <- evalM e l;
                        (Boolean r') <- evalM e r;
                        return (Boolean (l'&&r') )}
evalM e (Or l r) = do {(Boolean l') <- evalM e l;
                       (Boolean r') <- evalM e r;
                       return (Boolean (l'||r') )}
evalM e (Leq l r) = do {(Num l') <- evalM e l;
                        (Num r') <- evalM e r;
                        if (l'-r')<=0 then return (Boolean True) else return (Boolean False)}
evalM e (IsZero x) = do {(Num x') <- evalM e x;
                         if x'==0 then return (Boolean True) else return (Boolean False)} -- It returns Nothing when x<0
evalM e (If i t e') = do {(Boolean i') <- evalM e i;
                          if i'==True then (evalM e t) else (evalM e e')} -- Do I need to check if t and e are the same type?
evalM e (Bind i v b) = do {v' <- evalM e v;
                           (evalM ((i,v'):e) b)}
evalM e (Id i) = (lookup i e) --this is correct
-- evalM e (Id i) = do {result <- lookup i e;
--                      return result}  --this is unnecessary because our return type for lookup is identical to eval's.

-- Exercise 3
testEvals :: TERMLANG -> Bool
testEvals x = if evalM [] x == evalS x then True else False

-- Part 2: Type Checking

--Exercise 1
typeofM :: Cont -> TERMLANG -> (Maybe TYPELANG)
typeofM _ (Num n) = if n<0 then Nothing else return TNum
typeofM g (Plus l r) = do {TNum <- typeofM g l;
                           TNum <- typeofM g r;
                           return TNum}
typeofM g (Minus l r) = do {TNum <- typeofM g l;
                            TNum <- typeofM g r;
                            return TNum}
typeofM g (Mult l r) = do {TNum <- typeofM g l;
                           TNum <- typeofM g r;
                           return TNum}
typeofM g (Div l r) = do {TNum <- typeofM g l;
                          TNum <- typeofM g r;
                          return TNum}
typeofM _ (Boolean b) = return TBool
typeofM g (And l r) = do {TBool <- typeofM g l;
                          TBool <- typeofM g r;
                          return TBool}
typeofM g (Or l r) = do {TBool <- typeofM g l;
                         TBool <- typeofM g r;
                         return TBool}
typeofM g (Leq l r) = do {TBool <- typeofM g l;
                          TBool <- typeofM g r;
                          return TBool}
typeofM g (IsZero x) = do {TBool <- typeofM g x;
                           return TBool}
typeofM g (If c t e) = do {TBool <- typeofM g c;
                           t' <- typeofM g t;
                           e' <- typeofM g e;
                           if t'==e' then return t' else Nothing}
typeofM g (Bind i v b) = do {tv <- typeofM g v;
                             typeofM ((i,tv):g) b}
typeofM g (Id i) = (lookup i g)

--Exercise 2
evalT :: TERMLANG -> (Maybe TERMLANG)
evalT x = do {typeofM [] x;
             evalM [] x}

