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

  rListAnonym :: [VarName] -> [Term] -> [Term]
  rListAnonym _  []       = []
  rListAnonym fs (t1:ts1) = (snd $ rAnonym fs t1) : (rListAnonym ((fst $ rAnonym fs t1) ++ fs) (rListAnonym fs ts1))

  rAnonym :: [VarName] -> Term -> ([VarName], Term)
  rAnonym fs (Var (VarName v))
    | head v == '_' = ([nextValid fs (VarName v)], Var $ nextValid fs (VarName v))
    | otherwise     = ([], Var $ VarName v)
  rAnonym fs (Comb c terms) = ([], Comb c $ rListAnonym fs terms)

  r :: [VarName] -> [Term] -> [Term]
  r fs terms 
    | fst (f fs terms) == VarName "" = terms
    | otherwise                      = r (fst (f fs terms) : fs) (snd (f fs terms))

  f :: [VarName] -> [Term] -> (VarName, [Term])
  f _  []                        = (VarName "", [])
  f fs (y:ys) 
    | fst (g fs y) == VarName "" = f fs ys 
    | otherwise                  = (fst (g fs y), (snd (g fs y)) : ys)

  g :: [VarName] -> Term -> (VarName, Term)
  g fs (Var (VarName "_")) = (nextValid fs (VarName "_"), Var $ nextValid fs (VarName "_"))
  g fs (Var v)             = (VarName "", Var v)
  g fs (Comb c terms)      = (fst (f fs terms), Comb c (snd (f fs terms)))

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

r1 = Rule (Comb "g" [Var (VarName "_"), Comb "g" [Var (VarName "_"), Var (VarName "_")], Var (VarName "_")]) 
          [Var (VarName "_"),Comb "g" [Var (VarName "A"),Var (VarName "B"), Var (VarName "_0")]]
