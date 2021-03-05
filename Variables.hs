module Variables
  ( Vars,
    allVars,
    freshVars,
  )
where

import           Data.List
import           Type

--Returns list of VarNames of the given data type (Term, Rule, Prog, Goal) without duplicates
class Vars a where
  allVars :: a -> [VarName]

--instance if the argument a in allVars is a term
instance Vars Term where
  allVars (Var x)           = [x]
  allVars (Comb _ (y : ys)) = nub (allVars y ++ concatMap allVars ys)
  allVars (Comb _ [])       = []

--instance if the argument a in allVars is a rule
instance Vars Rule where
  allVars (Rule y xs) = nub (allVars y ++ concatMap allVars xs)

--instance if the argument a in allVars is a prog
instance Vars Prog where
  allVars (Prog xs) = nub $ concatMap allVars xs

--instance if the argument a in allVars is a goal
instance Vars Goal where
  allVars (Goal xs) = nub $ concatMap allVars xs

--creates an "infinite" list of variable name that are permitted in prolog
freshVars :: [VarName]
freshVars =
  map
    (\x -> VarName x)
    ( [[i] | i <- ['A' .. 'Z']]
        ++ [i : show j | j <- [(0 :: Int) ..], i <- ['A' .. 'Z']]
    )
