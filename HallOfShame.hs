
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




-- restrictTo :: [VarName] -> Subst -> Subst
-- restrictTo [] (Subst _) = Subst [] 
-- restrictTo (v1:vs) (Subst s) = let x = (findSubst v1 (Subst s))
--                         in  case x of
--                                Just y  -> substConcat (Subst [y]) (restrictTo vs (Subst s))  
--                                _       -> restrictTo vs (Subst s)




 --   (Subst ( [ (svName1, sTerm3) | (svName2, sTerm2) <- s2, (svName1, sTerm1) <- s1, s1Terms <- snd (unzip s1), sTerm3 <- (map (apply (Subst s2)) s1Term ) ] ++
 -- [ (svName2, sTerm2) | (svName2, sTerm2) <- s2, (svName1, sTerm1) <- s1 , (lookup svName2 s1) == Nothing]))

-- schränkt eine Substitution ein für eine Menge von Variablen die in der Substitution vorkommen

-- concatinate two Subst to one Subst by concatinate two Terms
--substConcat :: Subst -> Subst -> Subst
--substConcat (Subst xs) (Subst ys) = Subst (xs ++ ys)


-- find the single Substitution Rules by a given Variable.
-- and returns a Maybe of Nothing or Tuple of Varname and Term
--findSubst :: VarName -> Subst -> Maybe (VarName, Term)
-- by empty Substitution terminate with 'Nothing'
--findSubst _ (Subst []) = Nothing
--findSubst vName (Subst ((s3vName, s3Term):s3))
--                                -- terminate with Substitutionsrule if the right were found.
--                                | vName == s3vName = Just (s3vName, s3Term)
--                                -- otherwise searching continue in the restlist of substitutionrules.
--                                | otherwise = findSubst vName (Subst s3)


--case x of
                                   --  (vName,sTerm) -> sTerm
                                   --  _ -> (apply (Subst xs) (Var vName))
                                   
                                   
{-
compose'' :: Subst -> Subst -> Subst
compose'' (Subst s2) (Subst s1) = 
     Subst ([ (v1,  t)       |  let (s1Vars, s1Terms) = unzip s1,
                                let s3 =  [ (n2, t2) | (n2, t2) <- s2, (elem n2 s1Vars) == False],
                               (v1,_) <- s1, 
                                t     <- map (apply (Subst s3)) (s1Terms)])                                   
                                   
                                   
 -}                                  




{-

-- Composes two substitutions into one.
compose :: Subst -> Subst -> Subst
compose (Subst s2) (Subst s1) = 
  let (s1Vars, s1Terms) = unzip s1
      s3Terms = map (apply (Subst s2)) s1Terms
      s3 = (zip s1Vars s3Terms)
  in Subst (s3 ++ [ (n2, t2) | (n2, t2) <- s2, (elem n2 s1Vars) == False])



-}


























--------Removed TypeExtension ---------
{-module TypeExtension () where

import Type
import Vars
import Pretty

-- Definiert die Eq-Instanz für Term.
instance Eq Term where
    -- Sind es zwei Variablen mit dem gleichen Inhalt, dann ist der Term gleich.
    (Var vName1) == (Var vName2) = vName1 == vName2
    -- Handelt es sich um zwei Terme die jeweils eine Variable und ein Literal sind,
    -- so können sie nicht gleich sein. Terminiere mit 'False'.
    (Var _) == (Comb _ _) = False
    (Comb _ _) == (Var _) = False
    -- Sind es zwei Literale mit dem gleichen Namen und der gleichen Termliste,
    -- dann sind sie gleich.
    (Comb cName1 cList1) == (Comb cName2 cList2) = cName1 == cName2 && cList1 == cList2

    t1 /= t2 = not (t1 == t2)
-}

