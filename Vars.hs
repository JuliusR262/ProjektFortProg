module Vars (Vars, allVars, freshVars) where

import Type

class Vars a where
    allVars :: a -> [VarName]

-- infinit list of prolog valid names of variables
--    with helper function
freshVars :: [VarName]
freshVars =  (freshVars' 0)

-- begins with ["_0","_1","_2" ...]
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