module Main where

import Type
import Parser
import Substitution
import SLD

main :: IO ()
main = loop dfs (Prog []) ":l \\"
  
loop :: Strategy -> Prog -> FilePath -> IO ()
loop strat prog path
  = do putStr "?- "
       input <- getLine
       case input of 
         ""       -> loop strat prog path
         ":q"     -> putStrLn "Bye!"
         ":h"     -> do
                       putStrLn help
                       loop strat prog path
         ":s dfs" -> do
                       putStrLn "Strategy set to depth-first search."
                       loop dfs prog path
         ":s bfs" -> do
                       putStrLn "Strategy set to breadth-first search."
                       loop bfs prog path
         _        -> processInput input strat prog path

processInput :: String -> Strategy -> Prog -> FilePath -> IO ()
processInput input strat prog path
  | take 3 input == ":l " = do 
                              e <- (parseFile (drop 3 input))
                              case e of
                                (Left err)      -> do putStrLn err 
                                                      loop strat prog path
                                (Right newProg) -> do putStrLn $ "Loaded " ++ (drop 3 input)
                                                      loop strat newProg input
  | input == ":r" = do putStrLn "Try loading last file..."
                       processInput path strat prog path
  | otherwise = do case parse input of
                     (Left err) -> do putStrLn err
                                      loop strat prog path
                     (Right goal) -> do --putStrLn $ show goal
                                        --putStrLn $ show $ sld prog goal
                                        displaySolutions $ solveWith prog goal strat                                          
                                        loop strat prog path

displaySolutions :: [Subst] -> IO ()
displaySolutions [] = putStrLn "No Solutions."
displaySolutions (x:xs) 
  = do putStr (pretty x ++ " ")
       input <- getLine
       case input of
         ";" -> do displaySolutions xs
         _   -> putStrLn ""

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
