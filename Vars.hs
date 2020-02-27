module Vars (Vars, allVars, freshVars) where

import Type

-- | Returns a list of all variables inside a data structure.
class Vars a where
    allVars :: a -> [VarName]

-- | Infinite list containing internally used prolog variables.
freshVars :: [VarName]
freshVars =  (freshVars' 0)
  where
    freshVars' :: Int -> [VarName]
    freshVars' x = ('_' : (show x)) : (freshVars' (x+1))

-- | Instance of Vars for Term.
instance Vars (Term) where
  allVars (Var vName)         = [vName]
  allVars (Comb _ [])         = []
  allVars (Comb _ (x:xs))     = (allVars x) ++ (allVars (Comb "" xs))

-- | Instance of Vars for Rule.
instance Vars (Rule) where
  allVars (Rule x xs)         = (allVars x) ++ (allVars (Comb "" xs))
  
-- | Instance of Vars for Prog.
instance Vars (Prog) where
  allVars (Prog [])           = []
  allVars (Prog (x:xs))       = (allVars x) ++ (allVars (Prog xs))

-- | Instance of Vars for Goal.
instance Vars (Goal) where
  allVars (Goal xs)           = allVars (Comb "" xs)
