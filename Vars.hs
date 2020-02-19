module Vars (Vars, allVars, freshVars) where

import Type

class Vars a where
    allVars :: a -> [VarName]

freshVars :: [VarName]
freshVars =  (freshVars' 0)

freshVars' :: Int -> [VarName]
freshVars' x = ('_' : (show x)) : (freshVars' (x+1))
        
instance Vars (Term) where
    
    allVars (Var vName)         = [vName]
    allVars (Comb _ [])         = []
    allVars (Comb _ (x:xs))     = (allVars x) ++ (allVars (Comb "" xs))
    
    
instance Vars (Rule) where

    allVars (Rule x xs)         = (allVars x) ++ (allVars (Comb "" xs))
    
instance Vars (Prog) where
    
    allVars (Prog [])           = []
    allVars (Prog (x:xs))       = (allVars x) ++ (allVars (Prog xs))
    
instance Vars (Goal) where

    allVars (Goal xs)           = allVars (Comb "" xs)
    
