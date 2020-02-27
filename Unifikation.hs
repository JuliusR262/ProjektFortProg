module Unifikation (ds, unify) where

import Type
import Vars
import Substitution

ds :: Term -> Term -> Maybe (Term, Term)
ds t1 t2 =
  case (t1, t2) of
    (Var "_", _) -> Nothing
    (_, Var "_") -> Nothing
    (Var vname, Comb cname ts) -> Just (Var vname, Comb cname ts)
    (Comb cname ts, Var vname) -> Just (Var vname, Comb cname ts)
    (Var v1, Var v2) -> if v1 == v2 then Nothing
                                    else Just (Var v1, Var v2)

    (Comb c1 cs1, Comb c2 cs2) -> if c1 /= c2 || length cs1 /= length cs2
                                  then Just (Comb c1 cs1, Comb c2 cs2)
                                  else dsAll (zip cs1 cs2)
    where dsAll :: [(Term, Term)] -> Maybe (Term, Term)
          dsAll [] = Nothing
          dsAll ((t3, t4):ts) = case (ds t3 t4) of
            Nothing     -> dsAll ts
            Just result -> Just result


-- Liefert den allgemeinsten Unifikator für zwei Terme, falls dieser existiert.
--unify :: Term -> Term -> Maybe Subst
--unify t1 t2 = case ds t1 t2 of 
--  Nothing -> Just 
 

unify term1 term2 = unify' term1 term2 empty

unify' :: Term -> Term -> Subst -> Maybe Subst
unify' t1 t2 sigma = case (ds (apply sigma t1) (apply sigma t2) ) of
  Nothing -> Just sigma
  Just ((Comb _ _), _) -> Nothing
  Just ((Var v), t)    -> if elem v (allVars t) then Nothing
                                                else unify' t1 t2 ((single v t) `compose` sigma)
