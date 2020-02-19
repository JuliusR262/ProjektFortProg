import Type
import Vars
import Substitution 
import TypeExtension

ds, ds' :: Term -> Term -> Maybe (Term, Term)
ds term1 term2 = if term1 == term2 then Nothing else ds' term1 term2
ds' (Var vName1) term2 = Just ((Var vName1), term2)
ds' term1 (Var vName2) = Just ((Var vName2), term1)
ds' (Comb cName1 cTerm1) (Comb cName2 cTerm2)
    | cName1 /= cName2 || (length cTerm1) /= (length cTerm2) = Just ((Comb cName1 cTerm1), (Comb cName2 cTerm2))
    | otherwise = dsAll (zip cTerm1 cTerm2) 
    where dsAll :: [(Term, Term)] -> Maybe (Term, Term)
          dsAll [] = Nothing
          dsAll ((cterm1, cterm2):xs) = let 
                                            result = ds cterm1 cterm2 
                                        in if result == Nothing then dsAll xs 
                                                             else result

unify :: Term -> Term -> Maybe Subst
unify term1 term2 = unify' term1 term2 empty
unify' :: Term -> Term -> Subst -> Maybe Subst
unify' term1 term2 sigma = if (apply sigma term1) == (apply sigma term2) then Just sigma
                                                                          else 
                            case (ds (apply sigma term1) (apply sigma term2) ) of
                                Nothing -> Nothing
                                Just ((Comb _ _), _) -> Nothing
                                Just ((Var vName), term) -> unify' term1 term2 ((single vName term) `compose` sigma)
                                
                                                             
ds1 = Comb "." [Var "A", Comb "g" [Var "X", Var "Y"] ,Var "C",Var "D"]
ds2 = Comb "." [Var "A", Comb "g" [Var "X", Var "W"],Var "Z",Var "D"]
