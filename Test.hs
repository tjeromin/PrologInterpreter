import Substitution
import Type

-- Renames the given variable such that the new name is valid.
nextValid :: VarName -> VarName
nextValid (VarName v) 
  | length v == 1 = if True
                    then VarName (v ++ "0") 
                    else nextValid (VarName (v ++ "0"))
  | otherwise     = if True
                    then VarName (countVarUp v)
                    else nextValid (VarName (countVarUp v))

countVarUp :: String -> String
countVarUp s = take ((length s) - (length $ getLastNum s)) s ++ countNumUp (getLastNum s)

getLastNum :: String -> String
getLastNum s 
  | elem (last s) ['0' .. '9'] = getLastNum (init s) ++ [last s]
  | otherwise            = ""

-- Increments a variable name by one.
countNumUp :: String -> String
countNumUp ""  = "0"
countNumUp num = show $ (+1) (read num :: Int)