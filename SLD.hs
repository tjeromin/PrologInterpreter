module SLD
  ( SLDTree
  , Strategy
  , sld
  , dfs
  , bfs
  , solveWith
  ) where

import Data.List
import Data.Maybe
import Type
import Substitution
import Unification
import Rename

-- Represents a SLD-Tree in Prolog.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving Show


-- Construct the SLD tree for a given program and goal
sld :: Strategy -> Prog -> Goal -> SLDTree
sld strat p g = build empty g
  where
  -- Found solution
  build _ g'@(Goal []) = SLDTree g' []
  build s g'@(Goal (l:ls)) = SLDTree g'
    [ (mgu, build (compose mgu s) (Goal (map (apply mgu) (b ++ ls))))
    -- 1. Renaming of the program (fresh variants)
    | let Prog rs = p, Rule h b <- map (rename (allVars g' ++ allVars s)) rs
    -- 2. mgu of the rule heads and the first literal
    , mgu <- maybeToList (unify l h)
    ]

-- Alias type for search strategies
type Strategy = SLDTree -> [Subst]


-- Depth-first search in an SLD tree
dfs :: Strategy
dfs t = dfs' empty t
  where dfs' sigma (SLDTree (Goal []) _)  = [sigma]
        dfs' sigma (SLDTree _         ts) =
          [phi | (theta, t') <- ts, phi <- dfs' (compose theta sigma) t']

-- Breadth-first search in an SLD tree
bfs :: Strategy
bfs t = bfs' [(empty, t)]
  where bfs' [] = []
        bfs' ts = [sigma | (sigma, SLDTree (Goal []) _) <- ts] ++
          bfs' [ (compose theta sigma, t')
               | (sigma, SLDTree _ ts') <- ts
               , (theta, t') <- ts'
               ]
-- Solve a given goal with respect to a given program using a given strategy
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = map (`restrictTo` allVars g) (strat (sld strat p g))


{-
-- Represents a search strategy for looking for solutions in a SLD-Tree.
type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld prog (Goal gs) = sld' prog (Goal gs) (allVars prog)
  
sld' :: Prog -> Goal -> [VarName] -> SLDTree
sld' prog goal vs = SLDTree goal (map (\(s, g) -> (s, (sld' prog g (union vs $ allVars s)))) (newGoals prog goal (union vs (allVars goal))))

newGoals :: Prog -> Goal -> [VarName] -> [(Subst, Goal)]
newGoals _    (Goal [])     _ = []
newGoals prog (Goal (t:ts)) vs = map (\(s, Goal newTerms) -> (s, Goal (newTerms ++ map (apply s) ts))) (goalList prog vs t)

unifyCheck :: Rule -> Term -> Maybe (Subst, [Term])
unifyCheck (Rule t ts) term = 
  case (unify t term) of
    (Nothing) -> Nothing
    (Just s)  -> Just (s, (map (\x -> apply s x) ts))

goalList :: Prog -> [VarName] -> Term -> [(Subst, Goal)]
goalList (Prog [])       _  _ = []
goalList (Prog (x : xs)) vs t = 
  case unifyCheck (rename vs x) t of
    (Nothing)    -> goalList (Prog xs) vs t
    Just (s, ys) -> [(s, Goal ys)] ++ goalList (Prog xs) vs t

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
    (\subst -> restrictTo subst (allVars g)) --filter (\var -> var /= VarName "_") $ 
    (strat (sld p g))

-}
vars1 = allVars p1 ++ allVars goal1

goal1 = Goal [(Comb "vorfahre" [Var (VarName "X"), Var (VarName "Y")])]
rule1 = (Rule (Comb "vorfahre" [Var (VarName "N"), Var (VarName "V")]) [Comb "vorfahre" [Var (VarName "N"), Var (VarName "V2")], Comb "vorfahre" [Var (VarName "V2"), Var (VarName "Gs")]])

p1 = Prog [Rule (Comb "=" [Var (VarName "X"),Var (VarName "X")]) [],Rule (Comb "ehemann" [Comb "christine" [],Comb "heinz" []]) [],Rule (Comb "ehemann" [Comb "maria" [],Comb "fritz" []]) [],Rule (Comb "ehemann" [Comb "monika" [],Comb "herbert" []]) [],Rule (Comb "ehemann" [Comb "angelika" [],Comb "hubert" []]) [],Rule (Comb "mutter" [Comb "herbert" [],Comb "christine" 
  []]) [],Rule (Comb "mutter" [Comb "angelika" [],Comb "christine" []]) [],Rule (Comb "mutter" [Comb "hubert" [],Comb "maria" []]) [],Rule (Comb "mutter" [Comb "susanne" [],Comb "monika" []]) [],Rule (Comb "mutter" [Comb "norbert" [],Comb "monika" []]) [],Rule (Comb "mutter" [Comb "andreas" [],Comb "angelika" []]) [],Rule (Comb "vater" [Var (VarName "K"),Var (VarName "V")]) [Comb "ehemann" [Var (VarName "M"),Var (VarName "V")],Comb "mutter" [Var (VarName "K"),Var (VarName "M")]],Rule (Comb "elter" [Var (VarName "K"),Var (VarName "E")]) [Comb "vater" [Var (VarName "K"),Var (VarName "E")]],Rule (Comb "elter" [Var (VarName "K"),Var (VarName "E")]) [Comb "mutter" [Var (VarName "K"),Var (VarName "E")]],Rule (Comb "grossvater" [Var (VarName "E"),Var (VarName "G")]) [Comb "elter" [Var (VarName "E"),Var (VarName "F")],Comb "vater" [Var (VarName "F"),Var (VarName "G")]],Rule (Comb "grossvaeter" [Var (VarName "Gs")]) [Comb "findall" [Comb "." [Var (VarName "E"),Comb "." [Var (VarName "G"),Comb "[]" []]],Comb "grossvater" [Var (VarName "E"),Var (VarName "G")],Var (VarName "Gs")]],Rule (Comb "vorfahre" [Var (VarName "N"),Var (VarName "V")]) [Comb "vorfahre" [Var (VarName "N"),Var (VarName "V2")],Comb "vorfahre" [Var (VarName "V2"),Var (VarName "V")]],Rule (Comb "vorfahre" [Var (VarName "N"),Var (VarName "V")]) [Comb "elter" [Var (VarName "N"),Var (VarName "V")]],Rule (Comb "geschwister" [Var (VarName "S"),Var (VarName "P")]) [Comb "mutter" [Var (VarName 
  "S"),Var (VarName "M")],Comb "mutter" [Var (VarName "P"),Var (VarName "M")],Comb "\\+" [Comb "=" [Var (VarName "P"),Var (VarName "S")]]]]

p2 = Prog [Rule (Comb "p" [Var (VarName "X"),Var (VarName "Z")]) [Comb "q" [Var (VarName "X"),Var (VarName "Y")],Comb "p" [Var (VarName "Y"),Var (VarName "Z")]],Rule (Comb "p" [Var (VarName "X"),Var (VarName "X")]) [],Rule (Comb "q" [Comb "a" [],Comb "b" []]) []]
g2 = Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]


