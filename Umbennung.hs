module Umbennung (rename,renameWild,Forbidden,renameWildRule) where

import Pretty()
import Substitution
import Type
import Vars

type VarState = [(VarName, VarName)]

type Forbidden = [VarName]

-- Returns a variable name out of 'freshVars' that 
-- is not contained in a given variable list.
getUnusedV :: Forbidden -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName s)
                              , not(elem ('_':fvvName) s)] )

-- Returns a variable name out of 'freshVars' that
-- is neither contained in a list of forbidden variables,
-- nor in the right side of the state's tuples.
getUnusedVF :: VarState -> Forbidden -> VarName
getUnusedVF s fbs = head( [fvvName | fvvName <- freshVars
                                   , not(elem fvvName (snd(unzip s)))
                                   , not (elem fvvName fbs)] )

-- Includes a new tuple of variable substitutions into a state.
expandState :: VarState -> VarName -> VarName -> VarState
expandState state newVar assignedVar = (newVar, assignedVar) : state

-- Alias for lookup.
getVar :: VarName -> VarState -> Maybe VarName
getVar v s  = lookup v s

-- Returns an empty 'VarState'.
emptyVS :: VarState
emptyVS = []

-- Converts a 'VarState' into a substitution.
mStoSubst :: VarState -> Subst
mStoSubst ms = let (vs,fvs) = unzip ms 
               in Subst (zip vs (map (\x -> Var x) fvs))

-- Renames a rule by not using any variable names from
-- a given variable list. This also includes anonymous variables.
rename :: Forbidden -> Rule -> Rule
rename vs (Rule r rs) = let   rls = allVars (Rule r rs)
                              vs' = rls ++ vs
                              substi = mStoSubst (buildSubst (filter (/= "_") rls) vs' emptyVS) 
                              (Rule p ps) = Rule (apply substi r) (map (apply substi) rs) 
                              (t:ts) = renameWild (p:ps) [] in
                              (Rule t ts)
                              
-- Expands a state using a list of forbidden variables and
-- a list of new variables by finding new 'freshVars' for
-- every variable that is not forbidden.
buildSubst :: [VarName] -> Forbidden -> VarState -> VarState
buildSubst []     _   st  = st
buildSubst (v:vs) fbs st  = if((getVar v st) == Nothing) then
                              buildSubst vs fbs (expandState st v (getUnusedVF st fbs))
                            else buildSubst vs fbs st

-- Renames anonymous variables inside a rule.
renameWildRule :: Rule -> Forbidden -> Rule
renameWildRule (Rule t ts) vs = let tts = renameWild (t:ts) vs
                                in Rule (head tts) (tail tts)

-- Renames anonymous variables.
renameWild :: [Term] -> Forbidden -> [Term]
renameWild ts vs =  if(not (elem "_" (allVars (Goal ts)))) then
                      ts
                    else let  v = getUnusedV vs
                              (tts,_) = renameSWild ts v False 
                         in renameWild tts (v:vs)


renameSWild :: [Term] -> VarName -> Bool -> ([Term],Bool)
renameSWild [(Var "_")] v False     = ([Var ('_':v)],True)
renameSWild [(Var x)]   _ False     =  ([Var x],False)
renameSWild [Comb cname ts] v False = let (tts,bs) = renameSWild ts v False in
                                      ([Comb cname tts],bs)
renameSWild (t:ts) v False          = let (tt,b)  = renameSWild [t] v False
                                          (tts,bs)= renameSWild ts  v b in
                                      ((tt++tts),bs)
renameSWild ts _ b                  = (ts,b)

