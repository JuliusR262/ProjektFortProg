module Substitution (Subst(Subst),Pretty,pretty,empty,
                                single,apply,compose,
                                restrictTo) where
import Pretty
import Vars
import Type
import Data.List
import Data.Maybe

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


-- Applys a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst ((svName, sTerm):xs)) (Var vName)
    | svName == vName = sTerm
    | otherwise = (apply (Subst xs) (Var vName))
apply subst (Comb cName cTerm) = (Comb cName (map (apply subst) cTerm))

-- Composes two substitutions into one.
compose :: Subst -> Subst -> Subst
compose (Subst s2) (Subst s1) = 
  let s3 = [ (n2, t2) | (n2, t2) <- s2, isNothing (lookup n2 s1)]
  in Subst (s3 ++ [(v1, (apply (Subst s3) t1)) | (v1, t1) <- s1])  
  
restrictTo :: [VarName] -> Subst -> Subst
restrictTo xs (Subst ts1) = Subst [(v2Name,t) | v1Name <- xs,(v2Name,t) <- ts1,v1Name==v2Name]

