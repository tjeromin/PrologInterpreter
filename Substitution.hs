{-# LANGUAGE TemplateHaskell #-}
module Substitution 
  ( Subst
  , domain 
  , empty
  , single
  , apply
  , compose
  , restrictTo
  , pretty
  , allVars
  , testAll
  ) where

import Data.List

import Test.QuickCheck

import Type
import Pretty
import Variables

-- Data type to represent substitutions in prolog.
data Subst = Subst [(VarName, Term)]
  deriving Show

-- Returns those variables of a substitution which aren't depicted on themselfes.
domain :: Subst -> [VarName]
domain (Subst xs) = map (\x -> fst x) $ filter isValid xs where
  -- Checks if a given variable is in a term.
  isValid :: (VarName, Term) -> Bool
  isValid (VarName v, Var (VarName v2)) = v /= v2
  isValid _                             = True

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution with one element from a variable and a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a substitution to a term.
apply :: Subst -> Term -> Term
apply (Subst [])       t = t
apply (Subst (x : xs)) (Var (VarName v))
  | VarName v == fst x = snd x
  | otherwise          = apply (Subst xs) (Var (VarName v))
apply subst (Comb c ts) = Comb c (applyList subst ts)
  where
    -- Applies a substitution to all terms in a list.
    applyList :: Subst -> [Term] -> [Term]
    applyList _ []       = []
    applyList s (x : xs) = apply s x : applyList s xs


-- Merges two substitutions.
compose :: Subst -> Subst -> Subst
compose (Subst list1) (Subst list2) =
  Subst $ 
    union 
      (filter 
        (\x -> notElem x (substToList (restrictTo (Subst list1) (filterDupVars list1 list2)))) 
        ((compList list1 list2) ++ list1)) 
      (compList list1 list2)
  where
    -- Returns all variables from the first list that aren't in the second list.
    filterDupVars :: [(VarName, Term)] -> [(VarName, Term)] -> [VarName]
    filterDupVars l1 l2 =
      filter
        (\elemL1 -> elem elemL1 (map (\x -> fst x) l2))
        (map (\x -> fst x) l1)
    -- Applies all substitutions of the first list to all substitutions of the
    -- second list.
    compList :: [(VarName, Term)] -> [(VarName, Term)] -> [(VarName, Term)]
    compList []  xs2        = xs2
    compList _   []         = []
    compList xs1 (x2 : xs2) = (fst x2, apply (Subst xs1) (snd x2)) : compList xs1 xs2

    -- Returns only the list of a substitution.
    substToList :: Subst -> [(VarName, Term)]
    substToList (Subst xs) = xs

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
    allVars (Subst [(v, Var v2)]) 
      | v == v2   = []
      | otherwise = [v, v2]
    allVars (Subst (x:xs)) = nub ([fst x] ++ allVars (snd x) ++ allVars (Subst xs))

-- Generator for substitutions.
instance Arbitrary Subst 
 where 
  arbitrary = Subst <$> do 
    xs <- arbitrary
    return $ nubBy (\t0 t1 -> fst t0 == fst t1) 
                   (filter (\x -> Var (fst x) /= (snd x)) xs) 


--------------------------------- QUICKCHECK -----------------------------------


-- Property: Apply empty doesn't change a term.
prop_apply_empty :: Term -> Bool
prop_apply_empty t = apply empty t == t

-- Property: Apply a single substitution with element v doesn't change a Term 
-- which only consists of v.
prop_apply_single :: Term -> VarName -> Bool
prop_apply_single t v = apply (single v t) (Var v) == t

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
  t /= Var v ==> domain (single v t) == [v] 

-- Property: Checks if the domain of two composed substitutions is equal or less
-- to the domain of both substitution combined.
prop_domain_compose_combined :: Subst -> Subst -> Bool
prop_domain_compose_combined s1 s2 = (length $ nub $ domain (compose s1 s2)) <=
  (length $ nub (domain s1 ++ domain s2)) -- endlosschleife

-- Property: Checks if composed single substitutions which depicts in a cycle 
-- are only defined on the first variable.
prop_domain_compose_single :: VarName -> VarName -> Property
prop_domain_compose_single v1 v2 = v1 /= v2 
  ==> domain (compose (single v2 (Var v1)) (single v1 (Var v2))) == [v2]

-- Property: An empty substitution contains no variables.
prop_allVars_empty :: Bool
prop_allVars_empty = allVars empty == []

-- Property: A single substitution which variable depicts on itself contains no 
-- variable.
prop_allVars_var :: VarName -> Bool
prop_allVars_var v = (allVars $ single v (Var v)) == []

-- Property: allVarsof a single substitution is the same as allVars of its term
-- combined with its variable if the term is unequal to the variable.
prop_allVars_term :: VarName -> Term -> Property
prop_allVars_term v t = t /= Var v 
  ==> allVars (single v t) == nub ([v] ++ allVars t)

-- Property: allVars of composed substitutions is a subset of allVars of the two 
-- combined substitutions.
prop_allVars_compose_subset :: Subst -> Subst -> Bool
prop_allVars_compose_subset s1 s2 = (length $ nub $ allVars (compose s1 s2)) <=
  (length $ nub (allVars s1 ++ allVars s2))

-- Property: allVars of two composed single substitutions which variables 
-- depicts on themselfes are these two variables.
prop_allVars_compose_single :: VarName -> VarName -> Property
prop_allVars_compose_single v1 v2 = v1 /= v2 
  ==> allVars (compose (single v2 (Var v1)) (single v1 (Var v2))) == [v1, v2]

-- Property: domain is a subset of allVars of the same substitution.
prop_domain_allVars :: Subst -> Bool
prop_domain_allVars s = (length $ nub $ domain s) <= (length $ nub $ allVars s)

-- Property: domain of a restricted empty substitution is {} for every term.
prop_restrictTo_empty :: [VarName] -> Bool
prop_restrictTo_empty xs = (domain $ restrictTo empty xs) == []

-- Property: domain of every restricted substitution is a subset of the given
-- variables for the restriction.
prop_restrictTo_subset :: [VarName] -> Subst -> Bool
prop_restrictTo_subset xs s = (length $ nub $ domain $ restrictTo s xs) 
  <= (length $ nub xs)

-- Check all properties in this module:
return []
testAll :: IO Bool
testAll = $quickCheckAll










{-
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
s8 = Subst [(VarName "_",Comb "f" []),(VarName "B",Comb "g" [Comb "g" [Comb "f" [],Comb "f" []],Var (VarName "_0")]),(VarName "A",Var (VarName "_"))]
s9 = Subst [(VarName "_",Var (VarName "_0")),(VarName "_0",Comb "f" []),(VarName "A",Comb "g" [])]


t0 = Var (VarName "1")
t1 = Comb "." [Var (VarName "1"), Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1"), Comb "[]" []]]]
t6 = Var (VarName "B")


v0 = [VarName "1", VarName "2"]

comp0 = compose s2 s3 

rest0 = restrictTo s4 v0
-}