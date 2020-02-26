module SLD (Strategy, dfs, bfs, solve) where

import Data.List  (intercalate)
import Data.Maybe (isJust, fromJust)

import Substitution
import Type
import Umbennung
import Unifikation
import Vars

data SLDTree = SLDT Goal [(Subst, SLDTree)]
  deriving Show

type Strategy = SLDTree -> [Subst]

instance Pretty (SLDTree) where
  pretty (SLDT (Goal ts) []) = (pretty (Goal ts))
  pretty (SLDT (Goal ts) xs) = "( Goal " ++ (pretty (Goal ts)) ++ ", "
                               ++ intercalate ", " (map pretty' xs) ++ ")"
   where
    pretty' :: (Subst, SLDTree) -> String
    pretty' (subst, sldt) = " ( Subst und SLDT " ++ pretty subst
                                  ++ ", " ++ pretty sldt ++ ")"

findRules :: Prog -> Term -> [Rule]
findRules (Prog []) _ = []
findRules (Prog ((Rule (Comb cName1 cTerm1) rTerms ):xs)) (Comb cName2 cTerm2)
  | cName1 == cName2 = (Rule (Comb cName1 cTerm1) rTerms) : (findRules (Prog xs) (Comb cName2 cTerm2))
  | otherwise = findRules (Prog xs) (Comb cName2 cTerm2)
findRules _ _ = error "Invalid Term"

sld :: Prog -> Goal -> SLDTree
sld prog goal = sld' prog goal (allVars goal)


sld' :: Prog -> Goal -> Forbidden -> SLDTree
sld' _ (Goal []) _ = SLDT (Goal []) []
sld' (Prog rs) (Goal ts) fb =
  SLDT (Goal ts) [ (subst, ((sld' (Prog rs) newGoal fb'))) |
                                          (Rule renamedRT renamedRTS) <- map ((flip rename) fb) (findRules (Prog rs) (head ts)),
                                          let maybeSubst = unify renamedRT (head ts),
                                          isJust maybeSubst,
                                          let subst = fromJust maybeSubst,
                                          let newGoal =  Goal (map (apply subst) (renamedRTS ++ (tail ts))),
                                          let fb' = fb ++ (allVars subst)]

solve :: Strategy -> Prog -> Goal -> [Subst]
solve stgy prog goal = map (restrictTo (allVars goal) )  (stgy (sld prog goal))


dfs :: Strategy
dfs (SLDT (Goal []) _)          = [Subst []]
dfs (SLDT (Goal _) [])         = []
dfs (SLDT g ((subst, sldt):xs)) = (map (compose subst) (dfs sldt)) ++ (dfs (SLDT g xs))


bfs :: Strategy
bfs sldt = bfs' [(Subst [],sldt)]

bfs' :: [(Subst, SLDTree)] -> [Subst]
bfs' [] = []
bfs' (q:qs) = case q of
  (qsubst, SLDT (Goal []) [] ) -> qsubst : (bfs' qs)
  (_, SLDT (Goal _) [] ) -> bfs' qs
  (qsubst, SLDT (Goal _) ts ) -> bfs' (qs ++ map (\ (sbst,sldts) -> (compose qsubst sbst, sldts)) ts)
