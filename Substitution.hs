import Type

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
apply (Subst []) term          = term
apply (Subst (x:xs)) term = apply xs (applySingle x term)
    where applySingle :: (VarName, Term) -> Term -> Term
          applySingle (svName, sTerm) (Var destvName)
              -- If the var name from the substitution was found, replace it with the substitution term
              | svName == destvName = sTerm
              -- Otherwise leave the term unchanged
              | otherwise = (Var destvName)
          -- Search for vars in every comb's sub-term
          applySingle subst (Comb cName (x:xs)) = applySingle subst x
