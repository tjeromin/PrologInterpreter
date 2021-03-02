import Type

-- Data type to represent substitutions in prolog.
data Subst = Subst [(VarName, Term)]
  deriving Show

-- Returns those variables of a substitution which aren't depicted on themselfes.
domain :: Subst -> [VarName]
domain (Subst xs) = map (\x -> fst x) $ filter isValid xs where
  -- Checks if a given variable is in a term.
  isValid :: (VarName, Term) -> Bool
  isValid (VarName v, Var (VarName v2)) = v /= v2
  isValid (VarName v, Comb c [])        = True
  isValid (VarName v, Comb c (t:ts))    = isValid (VarName v, t) && isValid (VarName v, Comb c ts)

-- Tests:
d0 = domain (Subst [(VarName "1", Var (VarName "2")), (VarName "1", Var (VarName "2")), (VarName "1", Comb "[]" [])])
d1 = domain (Subst [(VarName "1", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])])
d2 = domain (Subst [(VarName "1", Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1")]]])])

-- An empty substitution.
empty :: Subst
empty = Subst []

-- Creates a substitution with one element from a variable and a term.
single :: VarName -> Term -> Subst
single v t = Subst [(v, t)]

-- Applies a subsitution to a term.
apply :: Subst -> Term -> Term
apply (Subst []) t = t
apply (Subst (x:xs)) (Var (VarName v)) 
  | VarName v == fst x = apply (Subst xs) (snd x)
  | otherwise          = apply (Subst xs) (Var (VarName v))
apply s (Comb c ts) = Comb c (applyList s ts) where
  -- Applies a subsitution to all terms in a list.
  applyList :: Subst -> [Term] -> [Term] 
  applyList s [] = []
  applyList s (t:ts) = apply s t : applyList s ts

--Tests:
s0 = Subst [(VarName "1", Var (VarName "2"))]
t0 = Var (VarName "1")
t1 = Comb "." [Var (VarName "1"), Comb "." [Comb "2" [], Comb "." [Comb "3" [], Var (VarName "1"), Comb "[]" []]]]

