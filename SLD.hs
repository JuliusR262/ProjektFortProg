module SLD (Strategy, dfs, bfs, sld, solve) where

import Data.List  (intercalate)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Either()
import Parser()
import Substitution
import Type
import Umbennung
import Unifikation
import Vars
import Debug.Trace


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
  where 
    sld' (Prog _ )  g@(Goal [])     _  = SLDT g []
    sld' (Prog [])  g               _  = SLDT g []
    sld' p@(Prog rs) g@(Goal (t:ts)) vs = 
      let rs' = map (rename (vs ++ (allVars g))) rs
          ts' = map (\(Rule l r) -> case unify t l of 
                                      Just s  -> Just $ (s, sld' prog (Goal $ map (apply s) (r++ts)) (vs ++ allVars s ++ allVars g))
                                      Nothing -> Nothing) rs'
      in SLDT g (catMaybes ts')

solve :: Strategy -> Prog -> Goal -> [Subst]
solve stgy (Prog rs) (Goal ts) =  map (restrictTo (allVars (Goal ts))) (stgy (sld (Prog rs) (Goal ts)))


dfs :: Strategy
dfs (SLDT (Goal []) _)          = pure empty
dfs (SLDT (Goal _) [])          = []
dfs (SLDT g ((subst, sldt):xs)) = (compose <$> (dfs sldt) <*> pure subst) ++ (dfs (SLDT g xs))


bfs :: Strategy
bfs sldt = bfs' [(Subst [],sldt)]

bfs' :: [(Subst, SLDTree)] -> [Subst]
bfs' [] = []
bfs' (q:qs) = case q of
  (qsubst, SLDT (Goal []) [] ) -> qsubst : (bfs' qs)
  (_, SLDT (Goal _) [] ) -> bfs' qs
  (qsubst, SLDT (Goal _) ts ) -> bfs' (qs ++ map (\ (sbst,sldts) -> (compose sbst qsubst, sldts)) ts)
