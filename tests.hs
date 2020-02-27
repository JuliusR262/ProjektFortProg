




--subst1 = (Subst [("A", Comb "f" [Var "B", Var "_", Comb "true" []]),("D", Comb "false" [])])
--subst2 = (Subst [("B", Comb "false" [])])
--term1 =  (Comb "." [Var "K",Comb "." [Var "L",Var "A",Var "B", Var "B"]])

--pretty1 = pretty (apply subst1 term1)



--vars    = ["A","B","C"]
--substs  = Subst [("A",Var "B"),("X",Var "B"),("C",Var "B")]

--subst1 = (single "B" (Comb "g" [Var "A", Var "C"]))
--subst2 = (single "B" (Comb "f" [Var "C"]))
--composit = subst2 `compose` subst1






{--

appendrule1 = Rule (Comb "append" [Comb "[]" [], Var "L", Var "L"]) []
appendrule2 = Rule (Comb "append" [Comb "." [Var "E", Var "R"],Var "L",Comb "." [Var "E", Var "RL"]]) [Comb "append" [Var "R", Var "L", Var "RL"]]

appendrule1term = (Comb "append" [Comb "[]" [], Var "L", Var "L"])
appendrule2term = (Comb "append" [Var "R", Var "L", Var "RL"])

test123 = apply (fromJust (unify (head appendgoal) appendrule1term)) (head appendgoal)
test1232 = apply (fromJust (unify (head appendgoal) appendrule2term)) (head appendgoal)

appendgoal = [(Comb "append" [Var "_", Var "Y", Comb "." [Comb "1" [], Comb "2" []]])]



-- append([],L,L).
-- append([E|R],L,[E|RL]) :- append(R,L,RL).


appendgoal   = (parse "append(_,Y,[1,2]).") :: Either String Goal

appendprog = Prog (appendrule1 : appendrule2 : [])

goaltest  = Goal [(Comb "p" [Var "S", Comb "b" []])]
goaltest2 = Goal [(Comb "p" [Var "_", Comb "b" []])]
goaltest3  = Goal [(Comb "p" [Var "_", Var "S"])]
goaltest4  = Goal [(Comb "p" [Var "_0", Comb "b" []])]
goaltest5 = Goal [(Comb "p" [Var "_", Var "_"])]

progtest = Prog [ (Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "X", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])] ),
                  (Rule (Comb "p" [Var "X", Var "X"]) []),
                  (Rule (Comb "q" [Comb "a" [], Comb "b" []]) []) ]
--}


--appendprog = do x <- (parseFile "append.pl") :: IO (Either String Prog)
--                let y = (fromRight (Prog []) x)
--                let z =
