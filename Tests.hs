import Substitution
import Unifikation
import Vars
import Type
import Umbennung
import Parser
import Data.Either
import SLD
import Data.List (intercalate)
import Util

subst11 = (Subst [("A", Comb "f" [Var "B", Var "_", Comb "true" []]),("D", Comb "false" [])])
subst12 = (Subst [("B", Comb "false" [])])
term1 =  (Comb "." [Var "K",Comb "." [Var "L",Var "A",Var "B", Var "B"]])

pretty1 = pretty (apply subst1 term1)

vars    = ["A","B","C"]
substs  = Subst [("A",Var "B"),("X",Var "B"),("C",Var "B")]

subst1 = (single "B" (Comb "g" [Var "A", Var "C"]))
subst2 = (single "B" (Comb "f" [Var "C"]))
composit = subst2 `compose` subst1


subst3 = Subst [("A", Comb "f" [Comb "x" []]),("B", Comb "true" []),("C", Comb "c" [Var "E"]), ("D", Var "E"), ("E", Comb "f" [Comb "f" [Comb "x" []]]), ("G", Var "H")]
subst4 = Subst [("A", Comb "g" [Comb "x" []]),("B", Comb "g" [Comb "x" []]),("C", Comb "g" [Comb "x" []]),("D", Comb "g" [Comb "x" []]),("H", Comb "false" [])]
composit2 = subst4 `compose` subst3

ds1 = Comb "." [Var "A", Comb "g" [Var "X", Var "_"] ,Var "h(A,B)",Var "D"]
ds2 = Comb "." [Var "A", Comb "g" [Var "X", Var "W"], Var "Z", Var "D"]

rule1 = (Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "_", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])])



goaltest  = Goal [(Comb "p" [Var "S", Comb "b" []])]
goaltest2 = Goal [(Comb "p" [Var "_", Comb "b" []])]
goaltest3  = Goal [(Comb "p" [Var "_", Var "S"])]
goaltest4  = Goal [(Comb "p" [Var "_0", Comb "b" []])]
goaltest5 = Goal [(Comb "p" [Var "_", Var "_"])]

progtest = Prog [ (Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "X", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])] ),
                  (Rule (Comb "p" [Var "X", Var "X"]) []),
                  (Rule (Comb "q" [Comb "a" [], Comb "b" []]) []) ]

dfstest  = intercalate ", " (map (pretty) (solve dfs progtest goaltest))
dfstest2 = intercalate ", " (map (pretty) (solve dfs progtest goaltest2))
dfstest3 = intercalate ", " (map (pretty) (solve dfs progtest goaltest3))
dfstest4 = intercalate ", " (map (pretty) (solve dfs progtest goaltest4))
dfstest5 = intercalate ", " (map (pretty) (solve dfs progtest goaltest5))



bfstest  = intercalate ", " (map (pretty) (solve bfs progtest goaltest))
bfstest2 = intercalate ", " (map (pretty) (solve bfs progtest goaltest2))


progtestsss   = (parseFile "append.pl") :: IO (Either String Prog)
goaltestsss2   = (parse "append(_,_,[1,2]).") :: Either String Goal
goaltestsss1   = (parse "append(X,Y,[1,2]).") :: Either String Goal





appendtest = do x <- (parseFile "append.pl") :: IO (Either String Prog)
                let y = goaltestsss1
                let a = goaltestsss2
                let z = (intercalate ", " (map (pretty) (solve bfs (fromRight' (Prog []) x) (fromRight (Goal []) y))))
                let b = (intercalate ", " (map (pretty) (solve bfs (fromRight' (Prog []) x) (fromRight (Goal []) a))))
                let q = pretty (sld (fromRight (Prog []) x) (fromRight (Goal []) y))
                let p = pretty (sld (fromRight (Prog []) x) (fromRight (Goal []) a))
                putStrLn q
                putStrLn p


appendtest2 = do  x <- (parseFile "append.pl") :: IO (Either String Prog)
                  let y = goaltestsss1
                  let a = goaltestsss2
                  let z = (intercalate ", " (map (pretty) (solve bfs (fromRight (Prog []) x) (fromRight (Goal []) y))))
                  let b = (intercalate ", " (map (pretty) (solve bfs (fromRight (Prog []) x) (fromRight (Goal []) a))))
                  putStrLn z
                  putStrLn b








reverseprog = (parse "examples/list.pl") :: IO (Either String Prog)

goalreverse = (parse "reverse([1,2,3],X)") :: Either String Goal


reversetest = do x <- reverseprog 
                 let y <- fromRight' x
                 let a = (intercalate ", " (map (pretty) (solve bfs (from







--appendprog = fromRight (Prog []) ((parseFile "append.pl") :: IO (Either String Prog))
