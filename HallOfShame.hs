----------- Pretty --------------------------

--pretty' :: Term -> String
--    pretty' (Comb "." [t3,t4])   = 
--      case t4 of
--          Comb "[]" []        -> pretty t3
--          Comb "." [t5,t6]    -> pretty t3 ++ ", " ++ pretty'(Comb "." [t5,t6] )
--          _                   -> pretty t3 ++ "|"  ++ pretty t4 
--    pretty' _ = ""
--    
--  pretty (Comb cName t2)      = cName ++ "(" 
--                                      ++ intercalate ", " (map pretty (t2))
--                                      ++ ")"



--pretty (Comb "." [t1,t2])   = case t2 of
--                                  
--                                  Comb "[]" []  ->  "[" ++ pretty t1 ++ "]"
--                                  Comb "." [t3,t4]   -> "[" ++ pretty t1 ++ ", " 
--                                                            ++ pretty(Comb "." [t3,t4] ) ++ "]"
--                                  _             ->  "[" ++ pretty t1 ++ "|" 
--                                                        ++ pretty t2 ++ "]"



-- if t2 is Comb and empty
-- if t2 is Comb is and continue the Lis
                                  --    like structure .(E,L)
                                  -- init and tail to delete the
                                  --    additional brackets
-- if the second term is a variable or
                                  -- the list doesnt continue list by structure
                                  
-- if list in Comb is longer because structure list in Prolog

-------------------------------------------------
-----------Substitution -------------------------
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