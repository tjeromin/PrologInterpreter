{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( ds,
    unify,
    testAllUnify,
  )
where

import           Data.List
import           Data.Maybe
import           Prelude
import           Substitution
import           Test.QuickCheck
import           Type

--returns the disagreement set as maybe type of two terms and "Nothing" if there is none.
ds :: Term -> Term -> Maybe (Term, Term)
--returns nothing in case that at least one term is an anonymous variable
ds (Var (VarName "_")) _ = Nothing
ds _ (Var (VarName "_")) = Nothing
--if t1 = t2 : ds(t1, t2 ) = ∅. t1 and t2 are only variables here.
ds (Var t1) (Var t2) = if (t1 == t2) then Nothing else Just (Var t1, Var t2)
--if t1 or t2 variable and /= t1 t2 : ds(t1, t2 ) = {t1, t2 }
ds (Var t1) (Comb n2 ys) = Just ((Var t1), Comb n2 ys)
ds (Comb n1 xs) (Var t1) = Just (Comb n1 xs, Var t1)
--if t1 = f (t1 , ..., tn ) und t' = g(s1 , ..., sm ) (n, m ≥ 0):
--if  f /= g or m /= n: ds(t, t0 ) = {t, t0 }
ds (Comb n1 xs) (Comb n2 ys) =
  if ((n1 /= n2) || (length xs) /= (length ys))
    then Just (Comb n1 xs, Comb n2 ys)
    else -- if f = g und m = n und ti = si for all i < k und tk /= sk : ds(t, t0 ) = ds(tk , sk )
      dsList xs ys

-- does the comparing operation of the comment in the else statement before.
dsList :: [Term] -> [Term] -> Maybe (Term, Term)
dsList (x : xs) (y : ys) =
  case ds x y of
    Nothing -> dsList xs ys
    dsXY    -> dsXY
dsList _ _ = Nothing

-- unifies two given terms
unify :: Term -> Term -> Maybe Subst
--introduces "unifyFull" in order to accumulate the substitutions recursively. Starting with the empty substitution.
unify term1 term2 = unifyFull term1 term2 empty
  where
    unifyFull :: Term -> Term -> Subst -> Maybe Subst
    unifyFull t1 t2 s
      --if there is no disagreement set the Substitution does not differ from the prior state.
      | ds t1 t2 == Nothing = Just s
      | otherwise = case ds t1 t2 of
        (Just (Var v, term)) ->
          -- e.g {X,f(X)} not allowed
          if (elem v (allVars term))
            then Nothing
            else --applying the composed substitution on both terms.
              unifyFull (apply (compose (single v term) s) t1) (apply (compose (single v term) s) t2) (compose (single v term) s)
        (Just (term, Var v)) ->
          if (elem v (allVars term))
            then Nothing
            else unifyFull (apply (compose (single v term) s) t1) (apply (compose (single v term) s) t2) (compose (single v term) s)
        _ -> Nothing

--------------------------------- QUICKCHECK -----------------------------------

prop_apply_ds_id :: Term -> Bool
prop_apply_ds_id t = isNothing (ds t t)

prop_apply_ds_diff :: Term -> Term -> Property
prop_apply_ds_diff t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

prop_apply_unify_ds :: Term -> Term -> Property
prop_apply_unify_ds t1 t2 = isNothing (ds t1 t2) ==> isJust (unify t1 t2) && (fmap domain (unify t1 t2) == Just [])

prop_apply_unify :: Term -> Term -> Property
prop_apply_unify t1 t2 = let s = unify t1 t2 in isJust s ==> isNothing (ds ((apply (fromJust s) t1)) (apply (fromJust s) t2))

-- Check all properties in this module:
return []

testAllUnify :: IO Bool
testAllUnify = $quickCheckAll

-- f1 :: Term -> Term -> Term
-- f1 t1 t2 = let s = unify t1 t2 in apply (fromJust s) t1
--
-- f2 :: Term -> Term -> Term
-- f2 t1 t2 = let s = unify t1 t2 in apply (fromJust s) t2
--
-- t1 = Comb "ehemann" [Comb "monika" [], Var (VarName "M")]
--
-- t2 = Comb "ehemann" [Var (VarName "F"), Comb "herbert" []]
--
-- t3 = Comb "equ" [Comb "f" [Comb "1" []], Comb "g" [Var (VarName "X")]]
--
-- t4 = Comb "equ" [Var (VarName "Y"), Var (VarName "Y")]
--
-- t5 = Var (VarName "X")
--
-- t6 = Comb "f" [Var (VarName "X")]
--
-- t7 = Comb "f" [Var (VarName "A"), Var (VarName "_0")]
--
-- t8 = Comb "f" [Comb "f" [Comb "f" [], Comb "g" [Comb "g" [Var (VarName "_0"), Var (VarName "B")], Var (VarName "_0")]], Comb "g" []]
--
-- s1 = Subst [(VarName "A", Comb "f" [Comb "f" [], Comb "g" [Comb "g" [Var (VarName "_0"), Var (VarName "B")], Var (VarName "_0")]]), (VarName "_0", Comb "g" [])]
