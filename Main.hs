import Data.Char()
import SLD
import Type
import Parser
import Data.Either
import Data.List()
import Pretty()
import Substitution
import Util
import System.IO

header, string, stratS1, stratS2, stratSF, help :: String

-- Welcome message.
header = unlines ["Welcome!" ,
                 ("Type " ++ ('"' : ":h") ++ ('"' : " for help."))]
string = "?- "
stratS1 = "Strategy set to "
stratS2 = "-first search."
stratSF = "Strategy set failed! set to old strategy!"

-- Help message.
help = unlines
    ["Commands available from the prompt:",
    " <goal>      Solves/proves the specified goal.",
    " :h          Shows this help message.",
    " :l <file>   Loads the specified file.",
    " :q          Exits the interactive environment.",
    " :s <strat>  Sets the specified search strategy",
    "             where <strat> is one of 'dfs', 'bfs', or 'iddfs'."]

-- The state of the interactive Prolog environment.
-- Stores a program and a SLD-tree evaluation strategy.
data REPLState = REPLState Prog Strategy

-- Main function. Starts the interactive Prolog environment.
main :: IO()
main = do
  hSetBuffering stdin LineBuffering  
  putStr header
  query (REPLState (Prog []) dfs)

-- User input loop.
query :: REPLState -> IO()
query rst = do  putStr string
                x <- getLine
                process x rst

-- Process user input.
process :: String -> REPLState -> IO()
process x rst@(REPLState p st) = 
  do case filter (/=' ') x of
                    ""              -> query rst
                    ":q"            -> return()
                    ":h"            -> do putStr help
                                          query rst
                    (':':'l':_)     -> do y <- (parseFile (tail (tail x)))
                                                  :: IO (Either String Prog)
                                          if(isRight y) then
                                            putStrLn "Loaded."
                                            else putStrLn ("Failed Loading.")
                                          query (REPLState (fromRight' p y) st)
                    (':':'s':strat) -> setStrat strat rst
                    _               -> do let y =(parse x) :: Either String Goal
                                          if(isRight y) then do
                                            let substs = solve st p (
                                                    fromRight' (Goal []) y)
                                            checkEmpty substs
                                            else putStrLn "Failed loading Goal!"
                                          query rst


checkEmpty,output :: [Subst] -> IO()

-- Checks if a list of solutions is empty. If it is, terminate with 'false'.
-- If not, evaluate the solutions.
checkEmpty substs = case substs of
                         [] ->  putStrLn "false"
                         _  ->  output substs

-- Evaluate the contents of a solution list. Print 'true' for every empty
-- solution. Otherwise print the solution.
output []             = do putStrLn "No more solutions."
output (subst:substs) = do case subst of 
                             Subst [] -> putStr "true"
                             _        -> putStr (pretty subst)
                           y <- getChar
                           putStrLn ""
                           if (y == ';') then
                             output substs
                             else return()

-- Tries changing the strategy of a state and queries for new
-- user-input.
setStrat :: String -> REPLState -> IO()
setStrat strat (REPLState prog st) = case strat of
                      "bfs" -> do putStrLn (stratS1 ++ "breadth" ++ stratS2)
                                  query (REPLState prog bfs)
                      "dfs" -> do putStrLn (stratS1 ++ "depth" ++ stratS2)
                                  query (REPLState prog dfs)
                      _     -> do putStrLn (stratSF)
                                  query (REPLState prog st)
