<<<<<<< HEAD
module SLD (Strategy, dfs, bfs, solve, sld) where
=======
module SLD (Strategy, dfs, bfs, solve,sld) where
>>>>>>> 36012de8ff9cd2831bb14ae2b6c029a421ae812b

import Data.List  (intercalate)
import Data.Maybe (isJust, fromJust)

import Data.Either

import Substitution
import Type
import Umbennung
import Unifikation
import Vars

import Parser

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
sld prog (Goal ts) = sld' prog (Goal (renameWild ts (allVars (Goal ts)))) []--(allVars goal)


sld' :: Prog -> Goal -> Forbidden -> SLDTree
sld' _ (Goal []) _ = SLDT (Goal []) []
sld' (Prog rs) (Goal ts) fb =
  SLDT (Goal ts) [ (subst, ((sld' (Prog rs) newGoal fb'))) |
                                          (Rule renamedRT renamedRTS) <- map ((flip rename) fb) (findRules (Prog rs) (head ts)),
                                          let maybeSubst = unify (head ts) renamedRT,
                                          isJust maybeSubst,
                                          let subst = fromJust maybeSubst,
                                          let newGoal =  Goal (map (apply subst) (renamedRTS ++ (tail ts))),
                                          let fb' = fb ++ (allVars subst)]


appendrule1 = Rule (Comb "append" [Comb "[]" [], Var "L", Var "L"]) []
appendrule2 = Rule (Comb "append" [Comb "." [Var "E", Var "R"],Var "L",Comb "." [Var "E", Var "RL"]]) [Comb "append" [Var "R", Var "L", Var "RL"]]

appendrule1term = (Comb "append" [Comb "[]" [], Var "L", Var "L"])
appendrule2term = (Comb "append" [Var "R", Var "L", Var "RL"])

test123 = apply (fromJust (unify (head appendgoal) appendrule1term)) (head appendgoal)
test1232 = apply (fromJust (unify (head appendgoal) appendrule2term)) (head appendgoal)

appendgoal = [(Comb "append" [Var "_", Var "Y", Comb "." [Comb "1" [], Comb "2" []]])]

{--

-- append([],L,L).
-- append([E|R],L,[E|RL]) :- append(R,L,RL).


appendgoal   = (parse "append(_,Y,[1,2]).") :: Either String Goal

appendprog = Prog (appendrule1 : appendrule2 : [])

goaltest  = Goal [(Comb "p" [Var "S", Comb "b" []])]
goaltest2 = Goal [(Comb "p" [Var "_", Comb "b" []])]
goaltest3  = Goal [(Comb "p" [Var "_", Var "S"])]
goaltest4  = Goal [(Comb "p" [Var "_0", Comb "b" []])]
goaltest5 = Goal [(Comb "p" [Var "_", Var "_"])]

progtest = Prog [ (Rule (Comb "p" [Var "X", Var "Z"]) [(Comb "q" [Var "X", Var "Y"]), (Comb "p" [Var "Y", Var "Z"])] ),
                  (Rule (Comb "p" [Var "X", Var "X"]) []),
                  (Rule (Comb "q" [Comb "a" [], Comb "b" []]) []) ]
--}


--appendprog = do x <- (parseFile "append.pl") :: IO (Either String Prog)
--                let y = (fromRight (Prog []) x)
--                let z =


solve :: Strategy -> Prog -> Goal -> [Subst]
solve stgy prog goal =  map (restrictTo (allVars goal) ) (stgy (sld prog goal))


dfs :: Strategy
dfs (SLDT (Goal []) _)          = [Subst []]
dfs (SLDT (Goal _) [])          = []
dfs (SLDT g ((subst, sldt):xs)) = (compose <$> (dfs sldt) <*> [subst]) ++ (dfs (SLDT g xs))


bfs :: Strategy
bfs sldt = bfs' [(Subst [],sldt)]

bfs' :: [(Subst, SLDTree)] -> [Subst]
bfs' [] = []
bfs' (q:qs) = case q of
  (qsubst, SLDT (Goal []) [] ) -> qsubst : (bfs' qs)
  (_, SLDT (Goal _) [] ) -> bfs' qs
  (qsubst, SLDT (Goal _) ts ) -> bfs' (qs ++ map (\ (sbst,sldts) -> (compose sbst qsubst, sldts)) ts)
