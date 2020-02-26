import Data.Char
import SLD


header :: String
header = unlines ["Welcome!" , 
                 ("Type " ++ ('"' : ":h") ++ ('"' : " for help."))]
                 
string = "?- "

stratS1 = "Strategy set to "
stratS2 = "-first search."
stratSF = "Strategy set failed! set to old strategy!"

help   = unlines ["Commands available from the prompt:",
                  " <goal>      Solves/proves the specified goal.",
                  " :h          Shows this help message.",
                  " :l <file>   Loads the specified file.",
                  " :q          Exits the interactive environment.",
                  " :s <strat>  Sets the specified search strategy",
                  "             where <strat> is one of 'dfs', 'bfs', or 'iddfs'."]

main :: IO()
main = do   putStr header
            query dfs


query ::Strategy -> IO()
query st = do   putStr string
                x <- getLine
                process x st
                
                
process x st = do case filter (/=' ') x of
                    ":q"            -> putStr ""
                    ":h"            -> do putStr help
                                          query st
                    (':':'s':strat) -> setStrat strat st         
                    _               -> query st
                
setStrat strat st = case strat of
                          "bfs" -> do putStrLn (stratS1 ++ "breadth" ++ stratS2)
                                      query bfs
                          "dfs" -> do putStrLn (stratS1 ++ "depth" ++ stratS2)
                                      query dfs
                          _     -> do putStrLn (stratSF)
                                      query st