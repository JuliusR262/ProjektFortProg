import Substitution
import Type
import Vars
import Pretty
import Control.Monad.State
 
 
type MyState = [(VarName, VarName)]

getUnusedV :: MyState -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars, not(elem fvvName (snd(unzip s)))] )
 
expandState :: MyState -> VarName -> VarName -> MyState
expandState state newVar assignedVar = (newVar, assignedVar) : state
 
getVar :: VarName -> MyState -> Maybe VarName
getVar v s  = lookup v s 

type MyStateMonad = State MyState


showState :: MyStateMonad -> String
showState (State [])   = show []
showState (State (v:vs)) = show v ++ showState (State vs) 


getNext :: VarName -> MyStateMonad VarName
getNext newVar = state (\st -> let 
                                    oldVal = getUnusedV st
                                    st' = expandState st newVar oldVal 
                               in (getUnusedV(st'), st') )

 
mystate :: MyStateMonad VarName
mystate = do  x <- getNext "D"
              z <- getNext "E"    
              y <- getNext "F"                  
              return x

mystate2 :: MyStateMonad VarName
mystate2 = getNext "A"             
              
main = do
  print(evalState mystate [("A","_0"),("B","_1"),("C","_2")])
  print (getUnusedV [("A","_0"),("C","_1"),("","_2"),("","_3")] )
  print(evalState mystate2 [("A","_0"),("B","_1"),("C","_2"),("E","_3")])
  print(getVar [("A","_0"),("B","_1"),("C","_2"),("E","_3")] "Q")


{-
valFromState :: MyState -> VarName
valFromState v = let usedVars = snd (unzip v)
                  in vhelp' usedVars freshVars
vhelp' :: [VarName] -> [VarName] -> VarName
vhelp' usedVars (x:xs) = if elem x usedVars then vhelp' usedVars xs else x
-}




{-
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
-}