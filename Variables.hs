module Variables 
  ( Vars
  , allVars
  , freshVars
  ) where

import Data.List

import Type

class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var x)           = [x]
  allVars (Comb _ (y : ys)) = nub (allVars y ++ concatMap allVars ys)
  allVars (Comb _ [])       = []

instance Vars Rule where
  allVars (Rule y xs) = nub (allVars y ++ concatMap allVars xs)

instance Vars Prog where
  allVars (Prog xs) = nub $ concatMap allVars xs

instance Vars Goal where
  allVars (Goal xs) = nub $ concatMap allVars xs

freshVars :: [VarName]
freshVars = map (\x -> VarName x) 
            ([[i] | i <- ['A' .. 'Z']] ++ 
            [i : show j | j <- [0 ..], i <- ['A' .. 'Z']])
