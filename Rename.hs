{-# LANGUAGE TemplateHaskell #-}
module Rename 
  ( rename
  , testAll
  ) where

import Data.List

import Test.QuickCheck
import Data.List.Unique

import Type
import Variables


rename :: [VarName] -> Rule -> Rule
rename xs (Rule t ts) 
  = Rule 
      (head (check_ (xs ++ (concatMap allVars $ rList xs (allVars $ Rule t ts) (t:ts))) $ rList xs (filter (\x -> x /= VarName "_") (allVars $ Rule t ts)) (t:ts)))
      (tail (check_ (xs ++ (concatMap allVars $ rList xs (allVars $ Rule t ts) (t:ts))) $ rList xs (filter (\x -> x /= VarName "_") (allVars $ Rule t ts)) (t:ts))) 
 where
  -- Checks if the given variable is not in the given list or rule.
  valid :: [VarName] -> VarName -> Bool
  valid fs v = notElem v fs && notElem v (allVars $ Rule t ts)

  -- Renames all variables of the second list in all terms of the given third list.
  rList :: [VarName] -> [VarName] -> [Term] -> [Term]
  rList _  []     terms = terms 
  rList fs (v:vs) terms = rList ((nextValid fs v) : fs) vs (map (rVar fs v) terms)

  -- Renames the given variable in a term.
  rVar :: [VarName] -> VarName -> Term -> Term
  rVar fs v (Var v2)
    | v == v2   = Var $ nextValid fs v
    | otherwise = Var v2
  rVar fs v (Comb c terms) = Comb c (map (rVar fs v) terms)

  -- Checks if a list of terms contains duplicate variables that begin with '_'.
  check_ :: [VarName] -> [Term] -> [Term]
  check_ fs terms 
    | contains_ terms = check_ (nextValid fs (VarName "_") : fs) (rList_ fs (VarName "_") terms)
    | otherwise       = terms

  -- Renames one occurence of the given variable in the given list.
  rList_ :: [VarName] -> VarName -> [Term] -> [Term]
  rList_ _  _ []          = []
  rList_ fs v (t1:ts1) 
    | elem v (allVars t1) = (r_ fs v t1 : ts1)
    | otherwise           = (t1 : rList_ fs v ts1)

  -- Renames one occurence of the given variable in the given term.
  r_ :: [VarName] -> VarName -> Term -> Term
  r_ fs _ (Var v2)       = Var $ nextValid fs v2
  r_ fs v (Comb c terms) = Comb c (rList_ fs v terms)

  -- Renames the given variable such that the new name is valid.
  nextValid :: [VarName] -> VarName -> VarName
  nextValid fs (VarName v) 
    | length v == 1 = if valid fs (VarName (v ++ "0"))
                        then VarName (v ++ "0") 
                        else nextValid fs (VarName (v ++ "0"))
    | otherwise     = if valid fs (VarName ((v !! 0) : (countUp v)))
                        then VarName ((v !! 0) : (countUp v))
                        else nextValid fs (VarName ((v !! 0) : (countUp v)))
  
  -- Increments a variable name by one.
  countUp :: String -> String
  countUp v = show $ (+1) (read [v !! 1] :: Int)

contains_ :: [Term] -> Bool
contains_ terms = elem (VarName "_") $ concatMap allVars terms

-- Returns a list with all duplicate variables that begin with '_' in all terms 
-- of the given list.
getDupAnonym :: [VarName] -> [Term] -> [VarName]
getDupAnonym fs xs 
  = filter 
      (\(VarName x) -> head x == '_' && notElem (VarName x) fs)
      (repeated $ concatMap allVarsWDup xs)

-- Same as allVars but with duplicates.
allVarsWDup :: Term -> [VarName]
allVarsWDup (Var x)           = [x]
allVarsWDup (Comb _ [])       = []
allVarsWDup (Comb _ (y : ys)) = allVarsWDup y ++ concatMap allVarsWDup ys


--------------------------------- QUICKCHECK -----------------------------------


prop_rename_intersect_allVars :: [VarName] -> Rule -> Bool
prop_rename_intersect_allVars xs r 
  = intersect (allVars (rename xs r)) (allVars r) == [] 

prop_rename_intersect_list :: [VarName] -> Rule -> Bool
prop_rename_intersect_list xs r = intersect (allVars (rename xs r)) xs == []

prop_rename_anonymous :: [VarName] -> Rule -> Bool
prop_rename_anonymous xs r = notElem (VarName "_") (allVars $ rename xs r)

prop_rename_eq_allVars :: [VarName] -> Rule -> Property
prop_rename_eq_allVars xs r = notElem (VarName "_") (allVars r) 
  ==> length (allVars $ rename xs r) == length (allVars r)

prop_rename_ge_allVars :: [VarName] -> Rule -> Bool
prop_rename_ge_allVars xs r 
  = length (allVars $ rename xs r) >= length (allVars r)

-- Check all properties in this module:
return []
testAll :: IO Bool
testAll = $quickCheckAll





l0 = [VarName "_1", VarName "A"]
r0 = Rule (Comb "g" [Var (VarName "_"), Var (VarName "_")]) 
          [Var (VarName "_"),Comb "g" [Var (VarName "A"),Var (VarName "B"), Var (VarName "_0")]]
ts0 = [Comb "g" [Var (VarName "_2"),Var (VarName "_2")], 
      Var (VarName "_2"),
      Comb "g" [Var (VarName "A0"),Var (VarName "B0"),Var (VarName "_3")]]

r1 = Rule (Comb "g" [Var (VarName "_"), Comb "g" [Var (VarName "_"), Var (VarName "_")], Var (VarName "_")]) 
          [Var (VarName "_"),Comb "g" [Var (VarName "A"),Var (VarName "B"), Var (VarName "_0"), Var (VarName "_0")]]

-- (Comb "g" [Var (VarName "_"), Comb "g" [Var (VarName "_"), Var (VarName "_")], Var (VarName "_")])

-- (Comb "g" [Var (VarName "_1"), Comb "g" [Var (VarName "_1"), Var (VarName "_1")], Var (VarName "_1")])

