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

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var (VarName "_")) _ = Nothing
ds _ (Var (VarName "_")) = Nothing
ds (Var t1) (Var t2) = if (t1 == t2) then Nothing else Just (Var t1, Var t2)
ds (Var t1) (Comb n2 ys) = Just ((Var t1), Comb n2 ys)
ds (Comb n1 xs) (Var t1) = Just (Comb n1 xs, Var t1)
ds (Comb n1 xs) (Comb n2 ys) =
  if ((n1 /= n2) || (length xs) /= (length ys))
    then Just (Comb n1 xs, Comb n2 ys)
    else
      if ((xs \\ ys) == [])
        then Nothing
        else ds (head (xs \\ ys)) (head (ys \\ xs))

unify :: Term -> Term -> Maybe Subst
unify term1 term2 = unifyFull term1 term2 empty
  where
    unifyFull :: Term -> Term -> Subst -> Maybe Subst
    unifyFull t1 t2 s
      | ds t1 t2 == Nothing = Just s
      | otherwise = case ds t1 t2 of
        (Just (Var v, w)) ->
          if (elem v (allVars w))
            then Nothing
            else unifyFull (apply (compply s (ds t1 t2)) t1) (apply (compply s (ds t1 t2)) t2) (compply s (ds t1 t2))
        (Just (v, Var w)) ->
          if (elem w (allVars v))
            then Nothing
            else unifyFull (apply (compply s (ds t1 t2)) t1) (apply (compply s (ds t1 t2)) t2) (compply s (ds t1 t2))
        -- (Just (Comb _ _, Comb _ _)) -> Nothing
        _ -> Nothing

compply :: Subst -> Maybe (Term, Term) -> Subst
compply s1 (Just (Var v, w)) = (compose (single v w) s1)
compply s1 (Just (v, Var w)) = (compose (single w v) s1)
compply _ _                  = empty

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
