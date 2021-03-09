module Main where

import Type
import Parser
import SLD

main :: IO ()
main = loop dfs (Prog [])
  
loop :: Strategy -> Prog -> IO ()
loop strat prog
  = do putStr "?- "
       input <- getLine
       case input of 
         ":q"     -> putStrLn "Bye!"
         ":h"     -> do
                       putStrLn help
                       loop strat prog
         ":s dfs" -> do
                       putStrLn "Strategy set to depth-first search."
                       loop dfs prog
         ":s bfs" -> do
                       putStrLn "Strategy set to breadth-first search."
                       loop bfs prog
         _        -> processInput input prog

processInput :: String -> Prog -> IO ()
processInput input prog
  | take 3 input == ":l " = do 
                              e <- (parseFile (drop 3 input))
                              f e prog
  | otherwise = loop bfs prog

f :: Either String Prog -> Prog -> IO ()
f (Left err) prog
  = do putStrLn err 
       loop dfs prog
f (Right prog) _
  = do putStrLn "file" 
       loop dfs prog
      

help :: String
help 
  = "Commands available from the prompt:\n" ++
    "  <goal>      Solves/proves the specified goal.\n" ++
    "  :h          Shows this help message.\n" ++
    "  :l <file>   Loads the specified file.\n" ++
    "  :q          Exits the interactive environment.\n" ++
    "  :r          Reloads the last loaded file.\n" ++
    "  :s <strat>  Sets the specified search strategy\n" ++
    "              where <strat> is one of 'dfs', 'bfs'." 
