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
rename xs r = r



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

