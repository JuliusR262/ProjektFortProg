import Type
import Pretty
import Data.List

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving Show
  
  
instance Pretty Subst where
    pretty subst = "{" ++ (intercalate ", " (prettyHelp subst))  ++ "}"
        where prettyHelp :: Subst -> [String]
              prettyHelp (Subst []) = []
              prettyHelp (Subst ((svName, sTerm):xs)) = (svName ++ " -> " ++ (pretty sTerm)) : (prettyHelp (Subst xs))

-- The empty substitution
empty :: Subst
empty = Subst []

-- One single substitution
single :: VarName -> Term -> Subst
single vName term = Subst [(vName, term)]




-- Applys a substitution to a term
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst ((svName, sTerm):xs)) (Var vName)
    | svName == vName = sTerm
    | otherwise = (apply (Subst xs) (Var vName))
apply subst (Comb cName cTerm) = (Comb cName (map (apply subst) cTerm))
--apply (Subst []) term     = term
--apply (Subst (x:xs)) term = apply (Subst xs) (applySingle x term)
--    where applySingle :: (VarName, Term) -> Term -> Term
--          applySingle (svName, sTerm) (Var destvName)
--              | svName == destvName = sTerm
--              | otherwise = (Var destvName)
--          applySingle (svName, sTerm) (Comb destcName destTerm) = (Comb destcName (map (applySingle (svName, sTerm)) destTerm))

instance Eq Term where
    (Var vName1) == (Var vName2) = vName1 == vName2
    (Var _) == (Comb _ _) = False
    (Comb _ _) == (Var _) = False
    (Comb cName1 cList1) == (Comb cName2 cList2) = cName1 == cName2 && cList1 == cList2
    
    t1 /= t2 = not (t1 == t2)
          
findSubst :: VarName -> Subst -> Maybe (VarName, Term)
findSubst _ (Subst []) = Nothing
findSubst vName (Subst ((s3vName, s3Term):s3))  
                                | vName == s3vName = Just (s3vName, s3Term)
                                | otherwise = findSubst vName (Subst s3)

compose :: Subst -> Subst -> Subst
compose (Subst []) (Subst s1) = (Subst s1)
compose (Subst s2) (Subst []) = (Subst s2)
compose (Subst (x2:s2)) (Subst (x1:s1)) = let 
                    (s2vName, s2Term) = x2
                    (s1vName, s1Term) = x1
                    applyToAll :: Subst -> [(VarName,Term)] -> [(VarName,Term)]
                    applyToAll substToApply [] = []
                    applyToAll substToApply ((s3vName,s3Term):s3) = (s3vName, (apply substToApply s3Term)) : (applyToAll substToApply s3)
                                          
                                          -- If s2 has a rule that s1 doesnt , apply it to everything in s1 and put it on top. Look for more rules that only s2 has in the next iteration.
                                          in if (findSubst s2vName (Subst (x1:s1))) == Nothing 
                                                then compose (Subst s2) (Subst (x2:(applyToAll (Subst [x2]) (x1:s1))))
                                             else compose (Subst s2) (Subst (x1:s1))
    
restrictTo :: [VarName] -> Subst -> Subst
restrictTo [] (Subst _) = Subst []
restrictTo (v1:vs) (Subst s) = let x = (findSubst v1 (Subst s))
                        in  case x of
                                Just y  -> substConcat (Subst [y]) (restrictTo vs (Subst s))
                                _       -> restrictTo vs (Subst s)

substConcat :: Subst -> Subst -> Subst
substConcat (Subst xs) (Subst ys) = Subst (xs ++ ys)

--case x of
                                   --  (vName,sTerm) -> sTerm
                                   --  _ -> (apply (Subst xs) (Var vName))

subst1 = (Subst [("A", Comb "f" [Var "B", Var "_", Comb "true" []]),("D", Comb "false" [])])
subst2 = (Subst [("B", Comb "false" [])])
term1 =  (Comb "." [Var "K",Comb "." [Var "L",Var "A",Var "B", Var "B"]])

--pretty1 = pretty (apply subst1 term1)


