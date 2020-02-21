




--subst1 = (Subst [("A", Comb "f" [Var "B", Var "_", Comb "true" []]),("D", Comb "false" [])])
--subst2 = (Subst [("B", Comb "false" [])])
--term1 =  (Comb "." [Var "K",Comb "." [Var "L",Var "A",Var "B", Var "B"]])

--pretty1 = pretty (apply subst1 term1)



--vars    = ["A","B","C"]
--substs  = Subst [("A",Var "B"),("X",Var "B"),("C",Var "B")]

--subst1 = (single "B" (Comb "g" [Var "A", Var "C"]))
--subst2 = (single "B" (Comb "f" [Var "C"]))
--composit = subst2 `compose` subst1


