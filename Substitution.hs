module Substitution (Subst(Subst),Pretty,pretty,empty,
                                single,apply,compose,
                                restrictTo) where
import Pretty
import Vars
import Type
import Data.List
import TypeExtension

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving Show

-- Pretty-inztance for Substitutions
instance Pretty Subst where
    pretty subst = "{" ++ (intercalate ", " (prettyHelp subst))  ++ "}"
        where prettyHelp :: Subst -> [String]
              prettyHelp (Subst []) = []
              prettyHelp (Subst ((svName, sTerm):xs)) = (svName ++ " -> " ++ (pretty sTerm)) : (prettyHelp (Subst xs))

-- Vars-Instanz für Substitutionen
instance Vars Subst where
    allVars (Subst []) = []
    allVars (Subst ((svName, sTerm):xs)) = svName : (allVars sTerm) ++ allVars (Subst xs)

-- The empty substitution
empty :: Subst
empty = Subst []

-- One single substitution
single :: VarName -> Term -> Subst
single vName term = Subst [(vName, term)]

-- concatinate two Subst to one Subst by concatinate two Terms
substConcat :: Subst -> Subst -> Subst
substConcat (Subst xs) (Subst ys) = Subst (xs ++ ys)


-- Applys a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst ((svName, sTerm):xs)) (Var vName)
    | svName == vName = sTerm
    | otherwise = (apply (Subst xs) (Var vName))
apply subst (Comb cName cTerm) = (Comb cName (map (apply subst) cTerm))

-- find the single Substitution Rules by a given Variable.
-- and returns a Maybe of Nothing or Tuple of Varname and Term
findSubst :: VarName -> Subst -> Maybe (VarName, Term)
-- by empty Substitution terminate with 'Nothing'
findSubst _ (Subst []) = Nothing
findSubst vName (Subst ((s3vName, s3Term):s3))
                                -- terminate with Substitutionsrule if the right were found.
                                | vName == s3vName = Just (s3vName, s3Term)
                                -- otherwise searching continue in the restlist of substitutionrules.
                                | otherwise = findSubst vName (Subst s3)


-- compose two Substitutions to one single.
compose :: Subst -> Subst -> Subst
-- Die leere Substitution komponiert mit einer anderen Substitution
-- liefert die andere Substitution unverändert zurück.
compose (Subst []) (Subst s1) = (Subst s1)
compose (Subst s2) (Subst []) = (Subst s2)
compose (Subst (x2:s2)) (Subst (x1:s1)) = let
                                          (s2vName, s2Term) = x2
                                          (s1vName, s1Term) = x1
                                          -- Wendet eine Substitution auf alle Terme einer anderne Substitution an.
                                          applyToAll :: Subst -> [(VarName,Term)] -> [(VarName,Term)]
                                          applyToAll substToApply [] = []
                                          -- Falls s2 eine Variable hat die s1 nicht hat,
                                          -- wende die einzelne Substitution mit der Variable
                                          -- auf alle Terme in s1 an und packe die einzelne Substitution
                                          -- anschließend in s1.
                                          applyToAll substToApply ((s3vName,s3Term):s3) = (s3vName, (apply substToApply s3Term)) : (applyToAll substToApply s3)
                                          in if (findSubst s2vName (Subst (x1:s1))) == Nothing then compose (Subst s2) (Subst (x2:(applyToAll (Subst [x2]) (x1:s1))))
                                                                                  else compose (Subst s2) (Subst (x1:s1))

-- schränkt eine Substitution ein für eine Menge von Variablen die in der Substitution vorkommen
restrictTo :: [VarName] -> Subst -> Subst
restrictTo [] (Subst _) = Subst []  -- Fall das [VarName leer ist] oder Ende der Rekursion
restrictTo (v1:vs) (Subst s) = let x = (findSubst v1 (Subst s)) -- sucht in findSubst nach Subst für eine gegebene Variable
                        in  case x of
                                Just y  -> substConcat (Subst [y]) (restrictTo vs (Subst s))  -- Fall eine Substituition gefunden :
                                                                                              -- wird mit substConcat an die Liste der eingeschränkten SUbstitutionen angefügt
                                _       -> restrictTo vs (Subst s)  -- Fall x = Nothing, wird verworfen und nächste Variable betrachtet


