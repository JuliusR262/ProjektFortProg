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


-- cName1 /= cName2 || (length cTerm1) /= (length cTerm2)



a = (Goal [Var "A",Var "B",Var"B",Var "C"])
b = (Rule (Var "_") [Var "B", Var "Q",Comb "."[Var "A" , Comb "[]" []]])


--Goal [t]                    t                  ts
--f(X,Y,Z)                -> f(X,Y,Z)     :- .....
--Goal [t1,t2]                    t1      :-      t1s     ,  t2 :- t2s
--f(X,Y,Z) , g(X,K,L)