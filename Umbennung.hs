module Umbennung (rename) where
import Substitution
import Type
import Vars
import Pretty
--import Control.Monad.State
 
 
type MyState = [(VarName, VarName)]

type Forbidden = [VarName]

getUnusedV :: Forbidden -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName s)] )

getUnusedVF :: MyState -> Forbidden -> VarName
getUnusedVF s fbs = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName (snd(unzip s))), not (elem fvvName fbs)] )                              
 
expandState :: MyState -> VarName -> VarName -> MyState
expandState state newVar assignedVar = (newVar, assignedVar) : state
 
getVar :: VarName -> MyState -> Maybe VarName
getVar v s  = lookup v s 

emptyMS :: MyState
emptyMS = []

mStoSubst :: MyState -> Subst
mStoSubst ms =let (vs,fvs) = unzip ms in 
              Subst (zip vs (map (\x -> Var x) fvs))

rename :: Rule -> Forbidden -> Rule
rename (Rule r rs) vs = let   (t:ts) = renameWild (r:rs) []
                              (Rule x xs) = (Rule t ts)
                              substi = mStoSubst (buildSubst (allVars(Rule x xs)) vs emptyMS) in
                        Rule (apply substi x) (map (apply substi) xs)

buildSubst :: [VarName] -> Forbidden -> MyState -> MyState
buildSubst [] _ st        = st
buildSubst (v:vs) fbs st  = if((getVar v st) == Nothing) then
                              buildSubst vs fbs (expandState st v (getUnusedVF st fbs))
                            else buildSubst vs fbs st



renameWild :: [Term] -> Forbidden -> [Term]
renameWild ts vs =  if(not (elem "_" (allVars (Goal ts)))) then
                      ts
                    else let  v = getUnusedV vs
                              (tts,_) = renameSWild ts v False in
                      renameWild tts (v:vs)
                      
renameSWild :: [Term] -> VarName -> Bool -> ([Term],Bool)
renameSWild [(Var "_")] v False     = ([Var v],True)
renameSWild [(Var x)]   _ False     =  ([Var x],False)
renameSWild [Comb cname ts] v False = let (tts,bs) = renameSWild ts v False in 
                                      ([Comb cname tts],bs)
renameSWild (t:ts) v False          = let (tt,b)  = renameSWild [t] v False
                                          (tts,bs)= renameSWild ts  v b in
                                      ((tt++tts),bs)
renameSWild ts _ b                  = (ts,b) 


-- renameWild [(Var "_"),(Var "_")] []
-- renameWild [Comb "." [Var "K", Comb "." [Var "L", Var "M", Var "N", Var "O"]]] []

{-
 where  renameWild' [Term] -> Forbidden -> Varname -> [Term]
        renameWild' [Var "_"]   xs x  = [Var x]
        renameWild' [Comb c ys] xs x  = [Comb c (renameWild ys (tail xs))]
-}


-- applyFst (single "_" (Var "_0")) ((Var "_"),false)


{-
renameWild :: [Term] -> Forbidden -> ([Term],Forbidden)
renameWild []      vs = ([], vs)
renameWild (t1:ts) vs = let (t2,f) = (renameWild' t1 vs) in
                        ((t2 : (fst(renameWild ts (f ++ vs))),(f ++ vs))
 where  renameWild' :: Term -> Forbidden -> (Term,Forbidden) 
        renameWild' tt vss = case tt of
                            (Var "_")       ->  let x = (getUnusedV vss) in
                                                ((Var x),[x])
                            (Comb cname xs) ->  let (ttt,ff) = (renameWild xs vss) in
                                                ((Comb cname ttt),(ff++vss))
                            _               -> (tt,vss)
-}                            



-- rename (Rule (Var "X") []) []
-- rename (Rule (Var "X") [(Var "Y")]) [("_0")]



{-
getUnusedV :: MyState -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName (snd(unzip s)))] )

-}


{-
rename :: Rule -> Forbidden -> Rule
rename (Rule r rs) fbs = rename' (allVars (Rule r rs)) fbs emptyMS (Rule r rs) 
 where rename' :: [VarName] -> Forbidden -> MyState -> Rule -> Rule       
       rename' [] _ _ (Rule r rs) = (Rule r rs)
       rename' (v1:vs) fvs st (Rule r rs)   
        | (elem (getUnusedV st) fvs) = let v2 = getUnusedV st in 
                      rename' (v1:vs) fvs (expandState v2 v2) (Rule r rs)
        | otherwise = case getVar v1 of
                            Nothing -> let v2 = getUnusedV st in 
                                          rename vs  fvs (expandState st v1 v2) 
                                          (Rule (apply (single v1 (Var (v2))) r) (map (apply (single v1 (Var v2))) rs))
                            _       -> let Just x = (getVar v1 st) in 
                                          rename' vs  fvs st (Rule (apply (single v1 (Var x)) r) 
                                          (map (apply (single v1 (Var x))) rs))
-}                                                        


{-
rename :: Goal -> Rule -> Rule
rename g (Rule t ts) = rename' (allVars g) (freshVars) (t:ts) 
 where
    rename' :: [VarName] -> [VarName] -> [Term] -> Rule
    rename'  []     _  (t:ts)  = Rule t ts
    rename' (v:vs) (u:us) ts   = rename' vs us (map(apply(single v (Var u)))ts)
    
-}




{-

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