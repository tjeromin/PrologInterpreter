module Pretty where 

import Type

-- Prints a data type pretty 
class Pretty a where
  pretty :: a -> String

-- Instance for pretty printing a term
instance Pretty Term where
  pretty term = prettyTerm term 0 0 where
    -- Pretty print a term.
    -- Term -> Depth of brackets -> Index of current term -> Pretty String 
    prettyTerm :: Term -> Int -> Int -> String
    prettyTerm (Var (VarName name))            _ _ = name
    prettyTerm (Comb "." [Var (VarName name)]) _ _ = ".(" ++ name ++ ")"
    prettyTerm (Comb "." (Var (VarName name):(Comb "." ts):_)) d i 
      = "[" ++ name ++ "|.(" ++ list ts d (i+1) False ++ ")]"
    prettyTerm (Comb "." (Var (VarName name):(Comb c (t:ts)):_)) d i 
      = "[" ++ name ++ "|" ++ c ++ "(" ++ list (t:ts) d (i+1) False ++ ")]"
    prettyTerm (Comb "." (Var (VarName name):ts)) d i 
      = ".(" ++ name ++ ", " ++ list ts d i False ++ ")"
    prettyTerm (Comb "." [t])               d i = prettyTerm t d i
    prettyTerm (Comb "." (t:[Comb "[]" _])) d i 
      | i == 0    = "[" ++ prettyTerm t d i ++ "]"
      | otherwise = prettyTerm t d i
    prettyTerm (Comb "." ts) d i
      | d == 0    = "[" ++ list ts (d+1) i True ++ "]"
      | otherwise = list ts d i True
    prettyTerm (Comb name []) _ _ = name 
    prettyTerm (Comb name ts) d i = name ++ "(" ++ comb ts d i ++ ")"

    -- Prints a list
    -- List of terms -> Depth of brackets -> Index of current term -> 
    -- Use "|" or ", " -> Pretty list
    list :: [Term] -> Int -> Int -> Bool -> String
    list [] _ _ _ = ""
    list (t:[]) d i _ = prettyTerm t d i
    list (t:(Comb "[]" _):[]) d i _ = prettyTerm t d i 
    list (t:(Comb "." ts):[]) d i s = prettyTerm t d i ++ ", " ++ list ts d (i+1) s 
    list (t:t2:[]) d i s 
      | s == True = prettyTerm t d i ++ "|" ++ prettyTerm t2 d (i+1)
      | otherwise = prettyTerm t d i ++ ", " ++ prettyTerm t2 d (i+1)
    list (t:ts) d i s = prettyTerm t d i ++ ", " ++ list ts d (i+1) s

    -- Prints all combinators except a list
    -- List of terms -> Depth of brackets -> Index of current term -> 
    -- Pretty String
    comb :: [Term] -> Int -> Int -> String
    comb []     _ _ = ""
    comb (t:[]) d i = prettyTerm t d i
    comb (t:ts) d i = prettyTerm t d i ++ ", " ++ list ts d (i+1) False

-- Tests:
{-
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
t14 = pretty (Comb "." [Var (VarName "K"), Comb "." [Var (VarName "L"), Var (VarName "M"), Var (VarName "N"), Var (VarName "O")]])
-- "[K|.(L, M, N, O)]"
-}