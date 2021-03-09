import           Pretty
import           Rename
import           Substitution
import           Type
import           Unification

-- Represents a SLD-Tree in Prolog.
data SLDTree = SLDTree Goal [(Subst, SLDTree)]

sld :: Prog -> Goal -> SLDTree
sld prog (Goal gs) = sld' prog (Goal gs) (allVars prog ++ allVars (Goal gs))
  where
    sld' :: Prog -> Goal -> [VarName] -> SLDTree
    sld' prog (Goal gs) xs = SLDTree (Goal gs) $ map (\(s, g) -> (s, (sld prog g))) (concatMap (goalList prog xs) gs)

unifyCheck :: Rule -> Term -> Maybe (Subst, [Term])
unifyCheck (Rule r rs) t = case (unify r t) of
  (Nothing) -> Nothing
  (Just s)  -> Just (s, (map (\x -> apply s x) rs))

goalList :: Prog -> [VarName] -> Term -> [(Subst, Goal)]
goalList (Prog (x : xs)) vs t = case (unifyCheck (rename vs x) t) of
  (Nothing)    -> []
  Just (s, ys) -> [(s, Goal ys)] ++ goalList (Prog xs) vs t
