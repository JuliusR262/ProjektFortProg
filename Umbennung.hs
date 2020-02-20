import Substitution
import Type
import Vars
import Pretty

rename :: Goal -> Rule -> Rule
rename g (Rule t ts) = rename' (allVars g) (freshVars) (t:ts) 
 where
    rename' :: [VarName] -> [VarName] -> [Term] -> Rule
    rename'  []     _  (t:ts)  = Rule t ts
    rename' (v:vs) (u:us) ts   = rename' vs us (map(apply(single v (Var u)))ts)
    
    




a = (Goal [Var "A",Var "B",Var"B",Var "C"])
b = (Rule (Var "A") [Var "B", Var "Q",Comb "."[Var "A" , Comb "[]" []]])