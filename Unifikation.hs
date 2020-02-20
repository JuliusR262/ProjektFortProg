module Unifikation (ds, unify) where

import Type
import Vars
import Substitution
import TypeExtension

-- Liefert die Unstimmigkeitsmenge von zwei Termen.
ds, ds' :: Term -> Term -> Maybe (Term, Term)
-- Fall 1: Die beiden Terme sind gleich, also gibt es keine Unstimmigkeit,
-- liefer 'Nothing'
ds term1 term2 =
  if term1 == term2 then Nothing
                    -- Prüfe die anderen Fälle, falls Fall 1 nicht auftritt.
                    else ds' term1 term2
-- Fall 2: Einer der beiden Terme ist eine Variable,
-- terminiere mit der Variable und dem Term als Unstimmigkeit.
ds' (Var vName1) term2 = Just ((Var vName1), term2)
ds' term1 (Var vName2) = Just ((Var vName2), term1)

-- Fall 3: Beide Terme sind Literale.
ds' (Comb cName1 cTerm1) (Comb cName2 cTerm2)
  -- Fall 3.1: Haben die Literale andere Namen oder Listenlängen,
  -- terminiere mit den beiden Literalen als Unstimmigkeit.
  | cName1 /= cName2 || (length cTerm1) /= (length cTerm2) =
    Just ((Comb cName1 cTerm1), (Comb cName2 cTerm2))
  -- Fall 3.2:
  | otherwise = dsAll (zip cTerm1 cTerm2)
   -- Sucht rekursiv nach einer Unstimmigkeit innerhalb der Termliste eines
   -- Literals.
   where dsAll :: [(Term, Term)] -> Maybe (Term, Term)
         dsAll [] = Nothing
         dsAll ((cterm1, cterm2):xs) =
           let
             -- Finde die Unstimmigkeitsmenge der
             -- beiden Terme am Anfang der Liste.
             result = ds cterm1 cterm2
            -- Falls es keine Unstimmigkeit gibt, suche weiter in der
            -- Restliste.
           in if result == Nothing then dsAll xs
               -- Falls doch, terminiere mit der gefundenen Unstimmigkeit.
              else result
-- Liefert den allgemeinsten Unifikator für zwei Terme, falls dieser existiert.
unify :: Term -> Term -> Maybe Subst
-- Ruf die Hilfsfunktion auf mit einem leeren Sigma (leerer Substitution) zum
-- Start.
unify term1 term2 = unify' term1 term2 empty
-- Hilfsfunktion: Konstruiert rekursiv den allgemeinsten Unifikator.
unify' :: Term -> Term -> Subst -> Maybe Subst
-- Fall 1: Die beiden Terme sind nach der Substitution von Sigma gleich,
-- also ist Sigma = mgu und wir terminieren mit Sigma.
unify' term1 term2 sigma =
  if (apply sigma term1) == (apply sigma term2) then Just sigma else

    case (ds (apply sigma term1) (apply sigma term2) ) of
      -- Kann dieser Fall auftreten?
      -- Falls es nach apply sigma keine Unstimmigkeit gibt,
      -- die Terme trotzdem aber nicht gleich sind, dann gibt
      -- es keinen mgu. Terminiere mit 'Nothing'.
      Nothing -> Nothing
      -- Fail: Falls an der ersten Stelle ein Literal ist, ist
      -- beides automatisch ein Literal und somit nicht unifizierbar.
      -- Terminiere mit 'Nothing'.
      Just ((Comb _ _), _) -> Nothing
      -- Fall 2: Falls wir eine Variable haben, erstellen wir eine
      -- Substitution,
      -- die diese Variable zu dem rechten Term substituiert und komponieren
      -- sie an das momentane Sigma. Danach rufen wir unify' rekursiv mit dem
      -- neuen Sigma auf.
      Just ((Var vName), term) ->
        unify' term1 term2 ((single vName term) `compose` sigma)


ds1 = Comb "." [Var "A", Comb "g" [Var "X", Var "Y"] ,Var "C",Var "D"]
ds2 = Comb "." [Var "A", Comb "g" [Var "X", Var "W"],Var "Z",Var "D"]
