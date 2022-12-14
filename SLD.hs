module SLD
  ( SLDTree,
    Strategy,
    sld,
    dfs,
    bfs,
    solveWith,
  )
where

import           Data.List
import           Rename
import           Substitution
import           Type
import           Unification

-- Represents a SLD-Tree in Prolog.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

-- Represents a search strategy for looking for solutions in a SLD-Tree.
type Strategy = SLDTree -> [Subst]

--creates a SLD tree recursively. "sld" uses "sld'" in order to forward forbidden variable names for the "rename" function
sld :: Prog -> Goal -> SLDTree
sld prog (Goal gs) = sld' prog (Goal gs) (allVars prog)

--returns recursively the child trees of the given sld node (or root in the beginning case).
sld' :: Prog -> Goal -> [VarName] -> SLDTree
sld' prog goal vs = SLDTree goal (map (\(s, g) -> (s, (sld' prog g (union vs $ allVars s)))) (newGoals prog goal (union vs (allVars goal))))

--returns list of new goals (with Substitution) derived from prog for multible goals.
newGoals :: Prog -> Goal -> [VarName] -> [(Subst, Goal)]
newGoals _    (Goal [])       _  = []
newGoals prog (Goal (t : ts)) vs = map (\(s, Goal newTerms) -> (s, Goal (newTerms ++ map (apply s) ts))) (goalList prog vs t)

--checks if a given rule can be applied on a given term. If so, it returns a Maybe tuple with the substitution computed by unify and the list of "rule-terms".
unifyCheck :: Rule -> Term -> Maybe (Subst, [Term])
unifyCheck (Rule t ts) term =
  case (unify t term) of
    (Nothing) -> Nothing
    (Just s)  -> Just (s, (map (\x -> apply s x) ts))

--returns a list with tuples that carry all the substitution and new goals derived from all rules that are appliable on the given term.
goalList :: Prog -> [VarName] -> Term -> [(Subst, Goal)]
goalList (Prog [])       _  _ = []
goalList (Prog (x : xs)) vs t = 
  case unifyCheck (rename vs x) t of
    (Nothing)    -> goalList (Prog xs) vs t
    Just (s, ys) -> [(s, Goal ys)] ++ goalList (Prog xs) vs t

-- Looks for solutions with depth-first search.
dfs :: Strategy
dfs (SLDTree _ []) = []
dfs tree = dfs' empty tree
 where 
  dfs' :: Subst -> SLDTree -> [Subst]
  dfs' subst (SLDTree (Goal []) [])  = [subst]
  dfs' _     (SLDTree _ [])          = []
  dfs' subst (SLDTree _ ((s, t):ts)) = dfs' (compose s subst) t ++ concatMap (\(s2, t2) -> dfs' (compose s2 subst) t2) ts
  
-- Looks for solutions with breadth-first search.
bfs :: Strategy
bfs tree = bfs' [(empty, tree)]
 where
  bfs' :: [(Subst, SLDTree)] -> [Subst]
  bfs' []                                = []
  bfs' ((s, (SLDTree (Goal []) _)) : ts) = [s] ++ bfs' ts
  bfs' ((s, (SLDTree _ ts2)) : ts)       = bfs' (ts ++ map (\(s2, t2) -> (compose s2 s, t2)) ts2)

-- Lists all solutions for a given program, strategy and goal.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat =
  map
    (\subst -> restrictTo subst (allVars g)) --filter (\var -> var /= VarName "_") $
    (strat (sld p g))

{-
vars1 = allVars p1 ++ allVars goal1

goal1 = Goal [(Comb "vorfahre" [Var (VarName "X"), Var (VarName "Y")])]
rule1 = (Rule (Comb "vorfahre" [Var (VarName "N"), Var (VarName "V")]) [Comb "vorfahre" [Var (VarName "N"), Var (VarName "V2")], Comb "vorfahre" [Var (VarName "V2"), Var (VarName "Gs")]])

p1 = Prog [Rule (Comb "=" [Var (VarName "X"),Var (VarName "X")]) [],Rule (Comb "ehemann" [Comb "christine" [],Comb "heinz" []]) [],Rule (Comb "ehemann" [Comb "maria" [],Comb "fritz" []]) [],Rule (Comb "ehemann" [Comb "monika" [],Comb "herbert" []]) [],Rule (Comb "ehemann" [Comb "angelika" [],Comb "hubert" []]) [],Rule (Comb "mutter" [Comb "herbert" [],Comb "christine"
  []]) [],Rule (Comb "mutter" [Comb "angelika" [],Comb "christine" []]) [],Rule (Comb "mutter" [Comb "hubert" [],Comb "maria" []]) [],Rule (Comb "mutter" [Comb "susanne" [],Comb "monika" []]) [],Rule (Comb "mutter" [Comb "norbert" [],Comb "monika" []]) [],Rule (Comb "mutter" [Comb "andreas" [],Comb "angelika" []]) [],Rule (Comb "vater" [Var (VarName "K"),Var (VarName "V")]) [Comb "ehemann" [Var (VarName "M"),Var (VarName "V")],Comb "mutter" [Var (VarName "K"),Var (VarName "M")]],Rule (Comb "elter" [Var (VarName "K"),Var (VarName "E")]) [Comb "vater" [Var (VarName "K"),Var (VarName "E")]],Rule (Comb "elter" [Var (VarName "K"),Var (VarName "E")]) [Comb "mutter" [Var (VarName "K"),Var (VarName "E")]],Rule (Comb "grossvater" [Var (VarName "E"),Var (VarName "G")]) [Comb "elter" [Var (VarName "E"),Var (VarName "F")],Comb "vater" [Var (VarName "F"),Var (VarName "G")]],Rule (Comb "grossvaeter" [Var (VarName "Gs")]) [Comb "findall" [Comb "." [Var (VarName "E"),Comb "." [Var (VarName "G"),Comb "[]" []]],Comb "grossvater" [Var (VarName "E"),Var (VarName "G")],Var (VarName "Gs")]],Rule (Comb "vorfahre" [Var (VarName "N"),Var (VarName "V")]) [Comb "vorfahre" [Var (VarName "N"),Var (VarName "V2")],Comb "vorfahre" [Var (VarName "V2"),Var (VarName "V")]],Rule (Comb "vorfahre" [Var (VarName "N"),Var (VarName "V")]) [Comb "elter" [Var (VarName "N"),Var (VarName "V")]],Rule (Comb "geschwister" [Var (VarName "S"),Var (VarName "P")]) [Comb "mutter" [Var (VarName
  "S"),Var (VarName "M")],Comb "mutter" [Var (VarName "P"),Var (VarName "M")],Comb "\\+" [Comb "=" [Var (VarName "P"),Var (VarName "S")]]]]

p2 = Prog [Rule (Comb "p" [Var (VarName "X"),Var (VarName "Z")]) [Comb "q" [Var (VarName "X"),Var (VarName "Y")],Comb "p" [Var (VarName "Y"),Var (VarName "Z")]],Rule (Comb "p" [Var (VarName "X"),Var (VarName "X")]) [],Rule (Comb "q" [Comb "a" [],Comb "b" []]) []]
g2 = Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]

-}
