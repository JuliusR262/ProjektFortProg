import Type

import Pretty

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving Show

-- The empty substitution
empty :: Subst
empty = Subst []

-- One single substitution
single :: VarName -> Term -> Subst
single vName term = Subst [(vName, term)]


-- Applys a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst []) term     = term
apply (Subst (x:xs)) term = apply (Subst xs) (applySingle x term)
    where applySingle :: (VarName, Term) -> Term -> Term
          applySingle (svName, sTerm) (Var destvName)
              | svName == destvName = sTerm
              | otherwise = (Var destvName)
          applySingle (svName, sTerm) (Comb destcName destTerm) = (Comb destcName (map (applySingle (svName, sTerm)) destTerm))
              
