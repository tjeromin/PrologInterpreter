{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( ds,
    unify,
  )
where

import           Data.List
import           Data.Maybe
import           Prelude
import           Pretty
import           Substitution
import           Test.QuickCheck
import           Type
import           Variables

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
unify t1 t2 = unifyFull t1 t2 (ds t1 t2) empty
  where
    unifyFull :: Term -> Term -> Maybe (Term, Term) -> Subst -> Maybe Subst
    unifyFull t1 t2 (Just (Var v, w)) s = if (elem v w) then Nothing else
    unifyFull t1 t2  s
      | ds t1 t2 == Nothing = Just s
      | otherwise = unifyFull (apply (compply s (ds t1 t2)) t1) (apply (compply s (ds t1 t2)) t2) (compply s (ds t1 t2))

compply :: Subst -> Maybe (Term, Term) -> Subst
compply s1 (Just (Var v, w)) = (compose s1 (single v w))
compply s1 (Just (v, Var w)) = (compose s1 (single w v))

t1 = Comb "ehemann" [Comb "monika" [], Var (VarName "M")]

t2 = Comb "ehemann" [Var (VarName "F"), Comb "herbert" []]
