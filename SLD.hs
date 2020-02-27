module SLD (Strategy, dfs, bfs, sld, solve) where

import Data.List  (intercalate)
import Data.Maybe (catMaybes)
import Data.Either()
import Parser()
import Substitution
import Type
import Umbennung
import Unifikation
import Vars

-- Data structure for SLD trees. Contains a root
-- and a list of edges containing a substitution
-- and a sub-tree.
data SLDTree = SLDT Goal [(Subst, SLDTree)]
  deriving Show

-- Type-signature for SLD tree evaluation strategies.
type Strategy = SLDTree -> [Subst]

-- Makes SLD trees a bit more readable.
instance Pretty (SLDTree) where
  pretty (SLDT (Goal ts) []) = (pretty (Goal ts))
  pretty (SLDT (Goal ts) xs) = "( Goal " ++ (pretty (Goal ts)) ++ ", "
                               ++ intercalate ", " (map pretty' xs) ++ ")"
   where
    pretty' :: (Subst, SLDTree) -> String
    pretty' (subst, sldt) = " ( Subst und SLDT " ++ pretty subst
                            ++ ", " ++ pretty sldt ++ ")"

-- Computes an SLD tree.
sld :: Prog -> Goal -> SLDTree
sld prog goal = sld' prog goal (allVars goal)
  where 
    sld' :: Prog -> Goal -> Forbidden -> SLDTree
    sld' (Prog _ )  g@(Goal [])     _  = SLDT g []
    sld' (Prog [])  g               _  = SLDT g []
    sld' (Prog rs)  g@(Goal (t:ts)) vs = 
      let rs' = map (rename (vs ++ (allVars g))) rs
          ts' = map (\(Rule l r) -> case unify t l of 
                                      Just s  -> Just $ (s, sld' prog (Goal $ map (apply s) (r++ts)) (vs ++ allVars s ++ allVars g))
                                      Nothing -> Nothing) rs'
      in SLDT g (catMaybes ts')

-- Evaluates an SLD tree with a given strategy and returns the solutions.
solve :: Strategy -> Prog -> Goal -> [Subst]
solve stgy (Prog rs) (Goal ts) =  map (restrictTo (allVars (Goal ts))) (stgy (sld (Prog rs) (Goal ts)))


-- Evaluates an SLD tree using depth-first search.
dfs :: Strategy
dfs (SLDT (Goal []) _)          = pure empty
dfs (SLDT (Goal _) [])          = []
dfs (SLDT g ((subst, sldt):xs)) = (compose <$> (dfs sldt) <*> pure subst) ++ (dfs (SLDT g xs))

-- Evaluates an SLD tree using breadth-first seach.
bfs :: Strategy
bfs sldt = bfs' [(Subst [],sldt)]
  where
    bfs' :: [(Subst, SLDTree)] -> [Subst]
    bfs' [] = []
    bfs' (q:qs) = 
      case q of
        (qsubst, SLDT (Goal []) [] ) -> qsubst : (bfs' qs)
        (_, SLDT (Goal _) [] ) -> bfs' qs
        (qsubst, SLDT (Goal _) ts ) -> bfs' (qs ++ map (\ (sbst,sldts) -> (compose sbst qsubst, sldts)) ts)
