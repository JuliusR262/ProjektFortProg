module Umbennung (rename,renameWild,Forbidden,renameWildRule) where

import Pretty()
import Substitution
import Type
import Vars

type MyState = [(VarName, VarName)]

type Forbidden = [VarName]

getUnusedV :: Forbidden -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName s)
                              , not(elem ('_':fvvName) s)] )

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

rename :: Forbidden -> Rule -> Rule
rename vs (Rule r rs) = let (Rule x xs) = (Rule r rs)
                            rls = allVars (Rule x xs)
                            vs' = rls ++ vs
                            substi = mStoSubst (buildSubst (filter (/= "_") rls) vs' emptyMS) 
                            (Rule p ps) = Rule (apply substi x) (map (apply substi) xs) 
                            (t:ts) = renameWild (p:ps) [] in
                            (Rule t ts)
                              

buildSubst :: [VarName] -> Forbidden -> MyState -> MyState
buildSubst [] _ st        = st
buildSubst (v:vs) fbs st  = if((getVar v st) == Nothing) then
                              buildSubst vs fbs (expandState st v (getUnusedVF st fbs))
                            else buildSubst vs fbs st

renameWildRule :: Rule -> Forbidden -> Rule
renameWildRule (Rule t ts) vs = let tts = renameWild (t:ts) vs
                                in Rule (head tts) (tail tts)

renameWild :: [Term] -> Forbidden -> [Term]
renameWild ts vs =  if(not (elem "_" (allVars (Goal ts)))) then
                      ts
                    else let  v = getUnusedV vs
                              (tts,_) = renameSWild ts v False in
                      renameWild tts (v:vs)

renameSWild :: [Term] -> VarName -> Bool -> ([Term],Bool)
renameSWild [(Var "_")] v False     = ([Var ('_':v)],True)
renameSWild [(Var x)]   _ False     =  ([Var x],False)
renameSWild [Comb cname ts] v False = let (tts,bs) = renameSWild ts v False in
                                      ([Comb cname tts],bs)
renameSWild (t:ts) v False          = let (tt,b)  = renameSWild [t] v False
                                          (tts,bs)= renameSWild ts  v b in
                                      ((tt++tts),bs)
renameSWild ts _ b                  = (ts,b)

