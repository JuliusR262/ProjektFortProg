module SLD () where

import Type
import Substitution
import Vars
import Umbennung
import Data.Maybe
import Unifikation

data SLDTree = SLDT Goal [(Subst, SLDTree)]
  deriving Show
 
 
 
findRules :: Prog -> Term -> [Rule]
findRules (Prog []) _ = []
findRules (Prog ((Rule (Comb cName1 cTerm1) rTerms ):xs)) (Comb cName2 cTerm2)
  | cName1 == cName2 = (Rule (Comb cName1 cTerm1) rTerms) : (findRules (Prog xs) (Comb cName2 cTerm2))
  | otherwise = findRules (Prog xs) (Comb cName2 cTerm2)
findRules _ _ = error "Invalid Term"
 
sld :: Prog -> Goal -> SLDTree
sld p (Goal []) = SLDT (Goal []) []
sld (Prog rs) (Goal ts) = 
  SLDT (Goal ts) [ (subst, ((sld (Prog rs) newGoal))) | t <- ts,
                                          (Rule renamedRT renamedRTS) <- map ((flip rename) (allVars (Goal ts))) (findRules (Prog rs) (head ts)),
                                          let maybeSubst = unify renamedRT (head ts),
                                          isJust maybeSubst,
                                          let subst = fromJust maybeSubst,
                                          let newGoal =  Goal (map (apply subst) (renamedRTS ++ (tail ts)))]



goaltest = Goal [(Comb "p" [Var "S", Comb "b" []])]
                                          
progtest = Prog [ (Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "X", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])] ), 
                 (Rule (Comb "p" [Var "X", Var "X"]) []),
                 (Rule (Comb "q" [Comb "a" [], Comb "b" []]) []) ]



--  let (Rule renamedRT renamedRTS) = rename (Rule rt rts) (allVars (head ts)), 
--  let maybeSubst = (unify (head ts) (rename r (allVars (head ts)))), 
--  let newGoal = apply subst rts ++ ]


{--


SLDT (Goal [Comb "p" [Var "S",Comb "b" []]]) [(Subst [("_1",Comb "b" []),("_0",Var "S")],SLDT (Goal [Comb "q" [Var "S",Var "_2"],Comb "p" [Var "_2",Comb "b" []]]) [(Subst [("_2",Comb "b" []),("S",Comb "a" [])],SLDT (Goal [Comb "p" [Comb "b" [],Comb "b" []]]) [(Subst [("_1",Comb "b" []),("_0",Comb "b" [])],SLDT (Goal [Comb "q" [Comb "b" [],Var "_2"],Comb "p" [Var "_2",Comb "b" []]]) []),(Subst [("_0",Comb "b" [])],SLDT (Goal []) [])]),(Subst [("_2",Comb "b" []),("S",Comb "a" [])],SLDT (Goal [Comb "p" [Comb "b" [],Comb "b" []]]) [(Subst [("_1",Comb "b" []),("_0",Comb "b" [])],SLDT (Goal [Comb "q" [Comb "b" [],Var "_2"],Comb "p" [Var "_2",Comb "b" []]]) []),(Subst [("_0",Comb "b" [])],SLDT (Goal []) [])])]),(Subst [("S",Comb "b" []),("_0",Comb "b" [])],SLDT (Goal []) [])]


--}
