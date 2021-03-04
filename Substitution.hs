{-# LANGUAGE TemplateHaskell #-}
module Substitution 
  ( domain 
  , empty
  , single
  , apply
  , restrictTo
  , pretty
  , allVars
  ) where

import Data.List

import Test.QuickCheck

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
  isValid (VarName v, Comb c (t:ts))    = isValid (VarName v, t) 
                                          && isValid (VarName v, Comb c ts)
  isValid _                             = True

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution with one element from a variable and a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a substitution to a term.
apply :: Subst -> Term -> Term
apply s t = apply' (restrictTo s (domain s)) t (restrictTo s (domain s)) where
  apply' :: Subst -> Term -> Subst -> Term
  apply' (Subst []) t _ = t
  apply' (Subst (x:xs)) (Var (VarName v)) s
    | VarName v == fst x = apply' s (snd x) s
    | otherwise          = apply' (Subst xs) (Var (VarName v)) s
  apply' subst (Comb c ts) s = Comb c (applyList subst ts s) where
  -- Applies a substitution to all terms in a list.
  applyList :: Subst -> [Term] -> Subst -> [Term] 
  applyList _ [] _ = []
  applyList s0 (x:xs) s1 = apply' s0 x s1 : applyList s0 xs s1

-- Merges two substitutions.
compose :: Subst -> Subst -> Subst
compose (Subst list1) (Subst list2) 
  = Subst (compList (substToList (restrictTo (Subst list1) (filterDupVars list1 list2))) list2) 
 where
  -- Returns all variables from the first list that aren't in the second list.
  filterDupVars :: [(VarName, Term)] -> [(VarName, Term)] -> [VarName]
  filterDupVars l1 l2 
    = filter (\elemL1 -> notElem elemL1 (map (\x -> fst x) l2)) 
             (map (\x -> fst x) l1)
  -- Applies all substitutions of the first list to all substitutions of the 
  -- second list.
  compList :: [(VarName, Term)] -> [(VarName, Term)] -> [(VarName, Term)]
  compList []  xs2      = xs2
  compList xs1 []       = xs1
  compList xs1 (x2:xs2) = applySubst x2 xs1 : compList xs2 xs1
  
  -- Applies a substitution to a term of a one element substitution.
  applySubst :: (VarName, Term) -> [(VarName, Term)] -> (VarName, Term)
  applySubst s [] = s
  applySubst (v, t) xs1 = (v, apply (Subst xs1) t)
  

-- Returns only the list of a substitution.
substToList :: Subst -> [(VarName, Term)]
substToList (Subst xs) = xs

{-
s1 = y -> z
s2 = x -> y
apply(s2, x) = y
apply(s1, apply(s2, x)) = z
s1 + s2 = x -> z, y -> z
-}

-- Resticts a substitution such that it's defined only on the given variables.
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst list) vs = Subst (filter (\x -> elem (fst x) vs) list)

-- Instance for pretty printing a substitution.
instance Pretty Subst 
 where
  pretty (Subst []) = "{}"
  pretty subst = printSubst (restrictTo subst (domain subst)) 
   where
    -- Pretty prints a substitution.
    printSubst :: Subst -> String
    printSubst (Subst list) = "{" ++ printList list ++ "}"

    -- Prints all elements of a substitution.
    printList :: [(VarName, Term)] -> String
    printList []     = ""
    printList (x:[]) = pretty (Var (fst x)) ++ " -> " ++ pretty (snd x)
    printList (x:xs) = pretty (Var (fst x)) ++ " -> " ++ pretty (snd x) ++ ", " 
                       ++ printList xs
    
-- Instance for listing all variables in a substitution.
instance Vars Subst 
 where 
  allVars (Subst [])     = []
  allVars (Subst (x:xs)) = nub ([fst x] ++ allVars (snd x) ++ allVars (Subst xs))

-- Generator for substitutions.
instance Arbitrary Subst 
 where 
  arbitrary = Subst <$> do 
    xs <- arbitrary
    return $ nubBy (\t0 t1 -> fst t0 == fst t1) 
                   (filter (\x -> check (fst x) (snd x)) xs) 
     where
      -- Checks if a variable is in a term.
      check :: VarName -> Term -> Bool
      check v (Var v2) = v /= v2
      check v (Comb c []) = True
      check v (Comb c (y:ys)) = check v y && check v (Comb c ys)


-- Property: Apply empty doesn't change a term.
prop_apply_empty :: Term -> Bool
prop_apply_empty t = apply empty t == t

-- Property: Apply a single substitution with element v doesn't change a Term 
-- which only consists of v.
prop_apply_single :: Term -> VarName -> Property
prop_apply_single t v = 
  elem v (domain (single v t)) ==> apply (single v t) (Var v) == t

-- Property: Compose two subst is equal to applying the first after the second 
-- subst.
prop_apply_compose :: Term -> Subst -> Subst -> Bool
prop_apply_compose t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t) 

-- Property: A empty substitution is not defined on any variables.
prop_domain_empty :: Bool
prop_domain_empty = domain empty == []

-- Property: A substitution with one element which is depicted on itself is 
-- defined on no variable.
prop_domain_single_var :: VarName -> Bool
prop_domain_single_var v = domain (single v (Var v)) == []

-- Property: A substitution with one element is defined on the variable of this
-- element.
prop_domain_single_term :: VarName -> Term -> Property
prop_domain_single_term v t = 
  not (isVarInTerm v t) ==> domain (single v t) == [v] 

-- Checks if a given variable is in a given term.
isVarInTerm v (Var v2)        = v == v2
isVarInTerm v (Comb _ [])     = False
isVarInTerm v (Comb c (x:xs)) = isVarInTerm v x || isVarInTerm v (Comb c xs)

-- Property: Checks if the domain of two composed substitutions is equal or less
-- to the domain of both substitution combined.
--prop_domain_compose_combined :: Subst -> Subst -> Bool
--prop_domain_compose_combined s1 s2 = domain (compose s1 s2)

-- Check all properties in this module:
return []
testAll = $quickCheckAll








t6 = Var (VarName "B")
s8 = Subst [(VarName "_",Comb "f" []),(VarName "B",Comb "g" [Comb "g" [Comb "f" [],Comb "f" []],Var (VarName "_0")]),(VarName "A",Var (VarName "_"))]
s9 = Subst [(VarName "_",Var (VarName "_0")),(VarName "_0",Comb "f" []),(VarName "A",Comb "g" [])]



--------------------------------------------------------------------------------
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
s6 = Subst [(VarName "_",Comb "f" [Var (VarName "A")]),
  (VarName "A",Comb "g" [Comb "f" [Comb "g" [Comb "g" [Comb "g" []],Comb "g" [Comb "g" []]],Comb "g" [Var (VarName "_0"),Comb "f" []]],Comb "f" []]),
  (VarName "_0",Comb "g" []),
  (VarName "B",Var (VarName "_"))]
s7 = Subst [(VarName "_0",Comb "f" []),(VarName "_",Var (VarName "A"))]


t0 = Var (VarName "1")
t1 = Comb "." [Var (VarName "1"), Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1"), Comb "[]" []]]]

v0 = [VarName "1", VarName "2"]

comp0 = compose s2 s3 

rest0 = restrictTo s4 v0
