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

-- Pretty-Instanz für Substitutionen
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

-- konkateniert zwei Subst zu einer indem die Terme konkateniert werden
substConcat :: Subst -> Subst -> Subst
substConcat (Subst xs) (Subst ys) = Subst (xs ++ ys)


-- Applys a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst ((svName, sTerm):xs)) (Var vName)
    | svName == vName = sTerm
    | otherwise = (apply (Subst xs) (Var vName))
apply subst (Comb cName cTerm) = (Comb cName (map (apply subst) cTerm))

-- Findet eine einzelne Substitutionsregel mit der gegebenen Variable.
findSubst :: VarName -> Subst -> Maybe (VarName, Term)
-- Bei der leeren Substitution terminieren wir mit 'Nothing'
findSubst _ (Subst []) = Nothing
findSubst vName (Subst ((s3vName, s3Term):s3))
                                -- Terminiere mit der Substitutionsregel falls die richtige gefunden wird.
                                | vName == s3vName = Just (s3vName, s3Term)
                                -- Ansonsten suchen wir weiter in der Restliste der Substitutionsregeln.
                                | otherwise = findSubst vName (Subst s3)


-- Komponiert zwei Substitutionen zu einer.
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


