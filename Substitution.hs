module Substitution where

import Test.QuickCheck
import Data.List
import Type
import Pretty
import Variablen

-- Data type to represent substitutions in prolog.
data Subst = Subst [(VarName, Term)]
  deriving Show

-- Returns those variables of a substitution which aren't depicted on themselfes.
domain :: Subst -> [VarName]
domain (Subst xs) = map (\x -> fst x) $ filter isValid xs where
  -- Checks if a given variable is in a term.
  isValid :: (VarName, Term) -> Bool
  isValid (VarName v, Var (VarName v2)) = v /= v2
  isValid (VarName v, Comb c (t:ts))    = isValid (VarName v, t) && isValid (VarName v, Comb c ts)
  isValid _                             = True

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution with one element from a variable and a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a subsitution to a term.
apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst (x:xs)) (Var (VarName v)) 
  | VarName v == fst x = apply (Subst xs) (snd x)
  | otherwise          = apply (Subst xs) (Var (VarName v))
apply subst (Comb c ts) = Comb c (applyList subst ts) where
  -- Applies a subsitution to all terms in a list.
  applyList :: Subst -> [Term] -> [Term] 
  applyList _ [] = []
  applyList s (x:xs) = apply s x : applyList s xs

-- Merges two substitutions.
-- Not correct yet.
compose :: Subst -> Subst -> Subst
compose (Subst list1) (Subst list2) = Subst (compList list1 list2) where 
  compList :: [(VarName, Term)] -> [(VarName, Term)] -> [(VarName, Term)]
  compList []  xs2      = xs2
  compList xs1 []       = xs1
  compList xs1 (x2:xs2) = applySubst x2 xs1 : compList xs2 xs1
  -- Applies a substitution to a term of a one element substitution.
  applySubst :: (VarName, Term) -> [(VarName, Term)] -> (VarName, Term)
  applySubst s [] = s
  applySubst (v, t) xs1 = (v, apply (Subst xs1) t)

{-
s1 = y -> z
s2 = x -> y
apply(s2, x) = y
apply(s1, y) = z
s1 + s2 = x -> z, y -> z
-}

-- Resticts a substitution such that it's defined only on the given variables.
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst list) vs = Subst (filter (\x -> elem (fst x) vs) list)

-- Instance for pretty printing a substitution.
instance Pretty Subst where
  pretty (Subst []) = "{}"
  pretty subst = printSubst (restrictTo subst (domain subst)) where
    -- Pretty prints a substitution.
    printSubst :: Subst -> String
    printSubst (Subst list) = "{" ++ printList list ++ "}"

    -- Prints all elements of a substitution.
    printList :: [(VarName, Term)] -> String
    printList []     = ""
    printList (x:[]) = pretty (Var (fst x)) ++ " -> " ++ pretty (snd x)
    printList (x:xs) = pretty (Var (fst x)) ++ " -> " ++ pretty (snd x) ++ ", " ++ printList xs
    
-- Instance for listing all variables in a substitution.
instance Vars Subst where 
  allVars (Subst [])     = []
  allVars (Subst (x:xs)) = nub ([fst x] ++ allVars (snd x) ++ allVars (Subst xs))

-- Generator for substitutions.
instance Arbitrary Subst where 
  arbitrary = Subst <$> do 
    v <- arbitrary
    t <- arbitrary
    return arbSubst

arbSubst :: Gen ([(VarName, Term)])
arbSubst = do
  v <- arbitrary
  t <- arbitrary
  return [(v, t)]



-- Tests:
d0 = domain (Subst [(VarName "1", Var (VarName "2")), (VarName "1", Var (VarName "2")), (VarName "1", Comb "[]" [])])
d1 = domain (Subst [(VarName "1", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])])
d2 = domain (Subst [(VarName "1", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1")]]])])

s0 = Subst [(VarName "1", Var (VarName "2"))]
s1 = Subst [(VarName "1", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1")]]])]
s2 = Subst [(VarName "Y", Var (VarName "Z"))]
s3 = Subst [(VarName "X", Var (VarName "Y"))]
s4 = Subst [(VarName "X", Var (VarName "Y")), (VarName "1", Var (VarName "2"))]
s5 = Subst [(VarName "1", Comb "." [Var (VarName "2"), Var (VarName "3"), Var (VarName "4")])]

t0 = Var (VarName "1")
t1 = Comb "." [Var (VarName "1"), Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1"), Comb "[]" []]]]

v0 = [VarName "1", VarName "2"]

comp0 = compose s2 s3 

rest0 = restrictTo s4 v0
