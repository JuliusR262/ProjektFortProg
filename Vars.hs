module Vars (Vars, allVars, freshVars) where

import Type

-- | Returns a list of all variables inside a data structure.
class Vars a where
    allVars :: a -> [VarName]

-- | Infinite list containing internally used prolog variables.
freshVars :: [VarName]
freshVars =  (freshVars' 0)

--
freshVars' :: Int -> [VarName]
freshVars' x = ('_' : (show x)) : (freshVars' (x+1))

-- instances for Term, Rule, Prog, Goal
--   to implement allVars
--  which all returns a list of names of the variables inside
instance Vars (Term) where
  -- concat recursivly all names of variables to one list together
  allVars (Var vName)         = [vName]
  allVars (Comb _ [])         = []
  allVars (Comb _ (x:xs))     = (allVars x) ++ (allVars (Comb "" xs))

instance Vars (Rule) where
  -- build on allVars of Term because Rule Term [Term]
  allVars (Rule x xs)         = (allVars x) ++ (allVars (Comb "" xs))

instance Vars (Prog) where
  -- build on allVars of Rule because Prog [Rule]
  allVars (Prog [])           = []
  allVars (Prog (x:xs))       = (allVars x) ++ (allVars (Prog xs))

instance Vars (Goal) where
  -- build on allVars of Term because Goal [Term]
  allVars (Goal xs)           = allVars (Comb "" xs)
