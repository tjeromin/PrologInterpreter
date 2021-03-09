module SLD 
  ( SLDTree
  , Strategy
  , dfs
  , bfs
  , solveWith
  ) where

import Type
import Substitution
import Unification
import Rename
import Pretty

-- Represents a SLD-Tree in Prolog.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]

-- Represents a search strategy for looking for solutions in a SLD-Tree.
type Strategy = SLDTree -> [Subst]

-- Looks for solutions with depth-first search.
dfs :: Strategy
dfs (SLDTree _ []) = []
dfs (SLDTree _ (x:xs)) = 
  case x of 
    (s, SLDTree (Goal []) []) -> s : concatMap dfs (map snd xs) -- [] or _?
    _                         -> dfs (snd x) ++ concatMap dfs (map snd xs)

-- Looks for solutions with breadth-first search.
bfs :: Strategy
bfs (SLDTree _ slds) = fst (searchLvl slds) : concatMap bfs (snd (searchLvl slds))
 where
  searchLvl :: [(Subst, SLDTree)] -> ([Subst], [SLDTree])
  searchLvl []     = ([], [])
  searchLvl (x:xs) = 
    case x of 
      (s, SLDTree (Goal []) []) -> (s : fst (searchLvl xs), snd (searchLvl xs))
      _                         -> (fst (searchLvl xs), snd x : snd (searchLvl xs))

-- Lists all solutions for a given program and goal.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat 
  = map 
    (\subst -> restrictTo subst (filter (\var -> var /= VarName "_") $ allVars g))
    (strat (sld p g))
