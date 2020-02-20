
-- 

--apply (Subst []) term     = term
--apply (Subst (x:xs)) term = apply (Subst xs) (applySingle x term)
--    where applySingle :: (VarName, Term) -> Term -> Term
--          applySingle (svName, sTerm) (Var destvName)
--              | svName == destvName = sTerm
--              | otherwise = (Var destvName)
--          applySingle (svName, sTerm) (Comb destcName destTerm) = (Comb destcName (map (applySingle (svName, sTerm)) destTerm))






--case x of
                                   --  (vName,sTerm) -> sTerm
                                   --  _ -> (apply (Subst xs) (Var vName))