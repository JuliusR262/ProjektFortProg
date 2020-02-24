module SLD () where

import Type
import Substitution
import Vars

data SLDTree = SLDT Goal [(Subst, SLDTree)]
  deriving Show
 
type Forbidden = [VarName] 
 

rename :: Rule -> Forbidden -> Rule
rename r _ = r
 
 
findRules :: Prog -> String -> [Rule]
findRules (Prog []) _ = []
findRules (Prog ((Rule (Comb cName cTerm) rTerms ):xs)) rName
  | rName == cName = (Rule (Comb cName cTerm) rTerms) : (findRules (Prog xs) rName)
  | otherwise = findRules (Prog xs) rName


 
--sld :: Prog -> Goal -> SLDTree
--sld p g = SLDT 
