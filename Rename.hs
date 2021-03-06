{-# LANGUAGE TemplateHaskell #-}
module Rename 
  ( rename
  , testAll
  ) where

import Data.List

import Test.QuickCheck

import Type
import Variables


rename :: [VarName] -> Rule -> Rule
rename xs (Rule t ts) 
  = Rule 
      (head (rList xs (allVars $ Rule t ts) (t:ts))) 
      (tail (rList xs (allVars $ Rule t ts) (t:ts))) 
 where
  -- Checks if the given variable is not in the given list or rule.
  valid :: [VarName] -> VarName -> Bool
  valid fs v = notElem v fs && notElem v (allVars $ Rule t ts)

  rList :: [VarName] -> [VarName] -> [Term] -> [Term]
  rList _  []     terms = terms 
  rList fs (v:vs) terms = rList ((nextValid fs v) : fs) vs (map (rTerm fs v) terms)

  -- Renames the given variable in a term.
  rTerm :: [VarName] -> VarName -> Term -> Term
  rTerm fs v (Var v2)
    | v == v2   = Var $ nextValid fs v
    | otherwise = Var v2
  rTerm fs v (Comb c terms) = Comb c (map (rTerm fs v) terms)

  nextValid :: [VarName] -> VarName -> VarName
  nextValid fs (VarName v) 
    | length v == 1 = if valid fs (VarName (v ++ "0") )
                        then VarName (v ++ "0") 
                        else nextValid fs (VarName (v ++ "0"))
    | otherwise     = if valid fs (VarName ((v !! 0) : (countUp v)))
                        then VarName ((v !! 0) : (countUp v))
                        else nextValid fs (VarName ((v !! 0) : (countUp v)))
  
  countUp :: String -> String
  countUp v = show $ (+1) (read [v !! 1] :: Int)

-- _ -> _0 -> _1 -> _2 -> ...




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
