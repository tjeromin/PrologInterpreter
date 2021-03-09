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

sld p g = SLDTree g []

-- Looks for solutions with depth-first search.
dfs :: Strategy
dfs (SLDTree _ []) = []
dfs (SLDTree _ (x:xs)) = 
  case x of 
    (s, SLDTree (Goal []) _) -> s : concatMap dfs (map snd xs) 
    (s, tree)                -> dfs (applyToSLD s tree) ++ concatMap dfs (map snd xs)

-- Looks for solutions with breadth-first search.
bfs :: Strategy
bfs (SLDTree _ slds) = fst (searchLvl slds) ++ concatMap bfs (snd (searchLvl slds))
 where
  searchLvl :: [(Subst, SLDTree)] -> ([Subst], [SLDTree])
  searchLvl []     = ([], [])
  searchLvl (x:xs) = 
    case x of 
      (s, SLDTree (Goal []) _) -> (s : fst (searchLvl xs), snd (searchLvl xs))
      (s, tree)                -> (fst (searchLvl xs), applyToSLD s tree : snd (searchLvl xs))

applyToSLD :: Subst -> SLDTree -> SLDTree
applyToSLD subst (SLDTree g xs) = SLDTree g $ map (\(s, t) -> (compose s subst, t)) xs

-- Lists all solutions for a given program and goal.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat 
  = map 
    (\subst -> restrictTo subst (filter (\var -> var /= VarName "_") $ allVars g))
    (strat (sld p g))
