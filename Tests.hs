import Substitution
import Unifikation
import Vars
import Type

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
