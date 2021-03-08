module Pretty where 

import Type

-- Prints a data type pretty 
class Pretty a where
  pretty :: a -> String

-- Instance for pretty printing a term
instance Pretty Term where
  pretty (Var (VarName name)) = name
  pretty (Comb "." [e, l]) = "[" ++ prettyList e l ++ "]"
  pretty (Comb name []) = name 
  pretty (Comb name ts) = name ++ "(" ++ prettyComb ts ++ ")"
   where    
    prettyComb :: [Term] -> String
    prettyComb []     = ""
    prettyComb (x:[]) = pretty x 
    prettyComb (x:xs) = pretty x ++ ", " ++ prettyComb xs

prettyList :: Term -> Term -> String
prettyList e  (Comb "[]" [])      = pretty e
prettyList e1 (Comb "." [e2, l2]) = pretty e1 ++ ", " ++ prettyList e2 l2
prettyList e  l                   = pretty e ++ "|" ++ pretty l


-- Tests:

t0 = pretty (Var (VarName "A"))
--"A"
t1 = pretty (Comb "true" [])
-- "true"
t2 = pretty (Comb "[]" [])
-- "[]"
t3 = pretty (Comb "f" [Var (VarName "B"), Var (VarName "_"), Comb "true" []])
-- "f(B, _, true)"
t4 = pretty (Comb "." [Comb "true" [], Comb "[]" []])
-- "[true]"
t5 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "g" [Var (VarName "C")], Comb "[]" []]])
-- "[true, g(C)]"
t6 = pretty (Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "." [Comb "3" [], Comb "[]" []]]])
-- "[1, 2, 3]"
t7 = pretty (Comb "." [Comb "true" [], Var (VarName "D")])
-- "[true|D]"
t8 = pretty (Comb "." [Var (VarName "E"), Comb "h" [Var (VarName "F"), Comb "i" [Var (VarName "G")]]])
-- "[E|h(F, i(G))]"
t9 = pretty (Comb "." [Comb "true" [], Comb "." [Comb "true" [], Comb "true" []]])
-- "[true, true|true]"
t10 = pretty (Comb "." [Comb "[]" [], Comb "[]" []])
-- "[[]]"
t11 = pretty (Comb "." [Comb "." [Comb "true" [], Comb "[]" []], Comb "[]" []])
-- "[[true]]"
t12 = pretty (Comb "." [Var (VarName "H")])
-- ".(H)"
t13 = pretty (Comb "." [Var (VarName "I"), Comb "true" [], Comb "j" [Var (VarName "J")]])
-- ".(I, true, j(J))"
t14 = pretty (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Comb "[]" []]])
-- "[K|.(L, M, N, [])]"
