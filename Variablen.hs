import           Type

class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  allVars []              = []
  allVars (Var x)         = [x]
  allVars (Comb _ y : ys) = nub (allVars y ++ allVars ys)

instance Vars Rule where
  allVars []        = []
  allVars Rule x xs = nub (allVars x ++ allVars xs)

instance Vars Prog where
  allVars []            = []
  allVars (Prog x : xs) = nub (allVars x ++ allVars xs)

instance Vars Goal where
  allVars []            = []
  allVars (Goal x : xs) = nub (allVars x ++ allVars xs)

freshVars :: [VarName]
freshVars = [VarName (i, j) | j <- [1 .. 100], i <- ['A' .. 'Z']]
