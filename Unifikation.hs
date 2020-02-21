module Unifikation (ds, unify) where

import Type
import Vars
import Substitution
--import TypeExtension

{--
ds, ds' :: Term -> Term -> Maybe (Term, Term)
ds term1 term2 =
  if term1 == term2 then Nothing
                    else ds' term1 term2
ds' (Var vName1) term2 = Just ((Var vName1), term2)
ds' term1 (Var vName2) = Just ((Var vName2), term1)
ds' (Comb cName1 cTerm1) (Comb cName2 cTerm2)
  | cName1 /= cName2 || (length cTerm1) /= (length cTerm2) =
    Just ((Comb cName1 cTerm1), (Comb cName2 cTerm2))
  | otherwise = dsAll (zip cTerm1 cTerm2)
   where dsAll :: [(Term, Term)] -> Maybe (Term, Term)
         dsAll [] = Nothing
         dsAll ((cterm1, cterm2):xs) =
           let
             result = ds cterm1 cterm2
           in if result == Nothing then dsAll xs
              else result
              --}
 
ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 = 
  case (t1, t2) of
    (Var "_", term) -> Nothing
    (term, Var "_") -> Nothing
    (Var v1, Var v2) -> if v1 == v2 then Nothing 
                                    else Just (Var v1, Var v2)
    
    (Comb c1 cs1, Comb c2 cs2) -> if c1 /= c2 || length cs1 /= length cs2
                                  then Just (Comb c1 cs1, Comb c2 cs2)
                                  else dsAll (zip cs1 cs2)
    where dsAll :: [(Term, Term)] -> Maybe (Term, Term)
          dsAll [] = Nothing
          dsAll ((t1, t2):ts) = case (ds t1 t2) of
            Nothing     -> dsAll ts
            Just result -> Just result
 

-- Liefert den allgemeinsten Unifikator für zwei Terme, falls dieser existiert.
unify :: Term -> Term -> Maybe Subst
unify term1 term2 = unify' term1 term2 empty

unify' :: Term -> Term -> Subst -> Maybe Subst
unify' t1 t2 sigma = case (ds (apply sigma t1) (apply sigma t2) ) of
  Nothing -> Just sigma
  Just ((Comb _ _), _) -> Nothing
  Just ((Var v), t) -> unify' t1 t2 ((single v t) `compose` sigma)
    

