module Vars (Vars, allVars, freshVars) where

import Type

class Vars a where
    allVars :: a -> [VarName]

-- unendliche Liste Prolog gültiger Variablennamen
--    mit Hilfsfunktion freshVars'
freshVars :: [VarName]
freshVars =  (freshVars' 0)

-- beginnt bei ["_0","_1","_2" ...]
freshVars' :: Int -> [VarName]
freshVars' x = ('_' : (show x)) : (freshVars' (x+1))

-- Instanzen für Term, Rule, Prog, Goal
--  zum implementieren von allVars
--  welche von allen enthaltenen Variablen den Namen wiedergibt     
instance Vars (Term) where
  -- konkateniert rekursiv alle Variablennamen in einer Liste aneinander
  allVars (Var vName)         = [vName]
  allVars (Comb _ [])         = []
  allVars (Comb _ (x:xs))     = (allVars x) ++ (allVars (Comb "" xs))
  
instance Vars (Rule) where
  -- baut auf allVars von Term auf aufgrund Rule Term [Term]
  allVars (Rule x xs)         = (allVars x) ++ (allVars (Comb "" xs))
  
instance Vars (Prog) where
  -- baut auf allVars von Rule auf aufgrund Prog [Rule]
  allVars (Prog [])           = []
  allVars (Prog (x:xs))       = (allVars x) ++ (allVars (Prog xs))
  
instance Vars (Goal) where
  -- baut auf allVars Term auf aufgrund Goal [Term]
  allVars (Goal xs)           = allVars (Comb "" xs)