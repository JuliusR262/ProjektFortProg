import Data.Char()
import SLD
import Type
import Parser
import Data.Either
import Data.List()
import Pretty()
import Substitution

header,string,stratS1,stratS2,stratSF,help :: String
header = unlines ["Welcome!" ,
                 ("Type " ++ ('"' : ":h") ++ ('"' : " for help."))]
string = "?- "
stratS1 = "Strategy set to "
stratS2 = "-first search."
stratSF = "Strategy set failed! set to old strategy!"



help = unlines
    ["Commands available from the prompt:",
    " <goal>      Solves/proves the specified goal.",
    " :h          Shows this help message.",
    " :l <file>   Loads the specified file.",
    " :q          Exits the interactive environment.",
    " :s <strat>  Sets the specified search strategy",
    "             where <strat> is one of 'dfs', 'bfs', or 'iddfs'."]

data REPLState = REPLState Prog Strategy

main :: IO()
main = do   putStr header
            query (REPLState (Prog []) dfs)

query :: REPLState -> IO()
query rst = do  putStr string
                x <- getLine
                process x rst

process :: String -> REPLState -> IO()
process x rst = do case filter (/=' ') x of
                    ""              -> query rst
                    ":q"            -> return()
                    ":h"            -> do putStr help
                    (':':'l':_)     -> do y <- (parseFile (tail (tail x)))
                                                  :: IO (Either String Prog)
                                          let (REPLState p st) = rst
                                          if(isRight y) then
                                            putStrLn "Loaded."
                                            else putStrLn ("Failed Loading.")
                                          query (REPLState (fromRight' p y) st)
                    (':':'s':strat) -> setStrat strat rst
                    _               -> do let y =(parse x) :: Either String Goal
                                          if(isRight y) then do
                                            let (REPLState prog st) = rst
                                            let substs = solve st prog (
                                                    fromRight' (Goal []) y)
                                            checkEmpty substs
                                            else putStrLn "Failed loading Goal!"
                                          query rst

fromRight' :: b -> Either a b -> b
fromRight' _ (Right b)  = b
fromRight' b  _         = b

checkEmpty,output :: [Subst] -> IO()
checkEmpty substs = case substs of
                         [] ->  putStrLn "false"
                         _  ->  output substs


output []             = do  putStrLn "No more solutions."
output (subst:substs) = do  putStrLn "IN OUTPUT"
                            let x = pretty subst
                            if(x == "{}") then
                              putStr "true"
                              else putStr x
                            y <- getChar
                            putStrLn ""
                            if (y == ';') then
                              output substs
                              else return()

setStrat :: String -> REPLState -> IO()
setStrat strat (REPLState prog st) = case strat of
                      "bfs" -> do putStrLn (stratS1 ++ "breadth" ++ stratS2)
                                  query (REPLState prog bfs)
                      "dfs" -> do putStrLn (stratS1 ++ "depth" ++ stratS2)
                                  query (REPLState prog dfs)
                      _     -> do putStrLn (stratSF)
                                  query (REPLState prog st)
