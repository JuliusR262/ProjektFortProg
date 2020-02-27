

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



----------Umbennung ----------
-- renameWild [(Var "_"),(Var "_")] []
-- rename (Rule (Comb "." [(Var "_"),(Var "M")]) [Comb "." [Var "_", Comb "." [Var "X", Var "M", Var "_", Var "X"]]]) []

{-
 where  renameWild' [Term] -> Forbidden -> Varname -> [Term]
        renameWild' [Var "_"]   xs x  = [Var x]
        renameWild' [Comb c ys] xs x  = [Comb c (renameWild ys (tail xs))]
-}


-- applyFst (single "_" (Var "_0")) ((Var "_"),false)


{-
renameWild :: [Term] -> Forbidden -> ([Term],Forbidden)
renameWild []      vs = ([], vs)
renameWild (t1:ts) vs = let (t2,f) = (renameWild' t1 vs) in
                        ((t2 : (fst(renameWild ts (f ++ vs))),(f ++ vs))
 where  renameWild' :: Term -> Forbidden -> (Term,Forbidden)
        renameWild' tt vss = case tt of
                            (Var "_")       ->  let x = (getUnusedV vss) in
                                                ((Var x),[x])
                            (Comb cname xs) ->  let (ttt,ff) = (renameWild xs vss) in
                                                ((Comb cname ttt),(ff++vss))
                            _               -> (tt,vss)
-}



-- rename (Rule (Var "X") []) []
-- rename (Rule (Var "X") [(Var "Y")]) [("_0")]



{-
getUnusedV :: MyState -> VarName
getUnusedV s = head( [fvvName | fvvName <- freshVars
                              , not(elem fvvName (snd(unzip s)))] )

-}


{-
rename :: Rule -> Forbidden -> Rule
rename (Rule r rs) fbs = rename' (allVars (Rule r rs)) fbs emptyMS (Rule r rs)
 where rename' :: [VarName] -> Forbidden -> MyState -> Rule -> Rule
       rename' [] _ _ (Rule r rs) = (Rule r rs)
       rename' (v1:vs) fvs st (Rule r rs)
        | (elem (getUnusedV st) fvs) = let v2 = getUnusedV st in
                      rename' (v1:vs) fvs (expandState v2 v2) (Rule r rs)
        | otherwise = case getVar v1 of
                            Nothing -> let v2 = getUnusedV st in
                                          rename vs  fvs (expandState st v1 v2)
                                          (Rule (apply (single v1 (Var (v2))) r) (map (apply (single v1 (Var v2))) rs))
                            _       -> let Just x = (getVar v1 st) in
                                          rename' vs  fvs st (Rule (apply (single v1 (Var x)) r)
                                          (map (apply (single v1 (Var x))) rs))
-}


{-
rename :: Goal -> Rule -> Rule
rename g (Rule t ts) = rename' (allVars g) (freshVars) (t:ts)
 where
    rename' :: [VarName] -> [VarName] -> [Term] -> Rule
    rename'  []     _  (t:ts)  = Rule t ts
    rename' (v:vs) (u:us) ts   = rename' vs us (map(apply(single v (Var u)))ts)

-}




{-

type MyStateMonad = State MyState

showState :: MyStateMonad -> String
showState (State [])   = show []
showState (State (v:vs)) = show v ++ showState (State vs)


getNext :: VarName -> MyStateMonad VarName
getNext newVar = state (\st -> let
                                    oldVal = getUnusedV st
                                    st' = expandState st newVar oldVal
                               in (getUnusedV(st'), st') )


mystate :: MyStateMonad VarName
mystate = do  x <- getNext "D"
              z <- getNext "E"
              y <- getNext "F"
              return x

mystate2 :: MyStateMonad VarName
mystate2 = getNext "A"

main = do
  print(evalState mystate [("A","_0"),("B","_1"),("C","_2")])
  print (getUnusedV [("A","_0"),("C","_1"),("","_2"),("","_3")] )
  print(evalState mystate2 [("A","_0"),("B","_1"),("C","_2"),("E","_3")])
  print(getVar [("A","_0"),("B","_1"),("C","_2"),("E","_3")] "Q")



valFromState :: MyState -> VarName
valFromState v = let usedVars = snd (unzip v)
                  in vhelp' usedVars freshVars
vhelp' :: [VarName] -> [VarName] -> VarName
vhelp' usedVars (x:xs) = if elem x usedVars then vhelp' usedVars xs else x
-}




{-
rename :: Goal -> Rule -> Rule
rename g (Rule t ts) = rename' (allVars g) (freshVars) (t:ts)
 where
    rename' :: [VarName] -> [VarName] -> [Term] -> Rule
    rename'  []     _  (t:ts)  = Rule t ts
    rename' (v:vs) (u:us) ts   = rename' vs us (map(apply(single v (Var u)))ts)


-- cName1 /= cName2 || (length cTerm1) /= (length cTerm2)



a = (Goal [Var "A",Var "B",Var"B",Var "C"])
b = (Rule (Var "_") [Var "B", Var "Q",Comb "."[Var "A" , Comb "[]" []]])


--Goal [t]                    t                  ts
--f(X,Y,Z)                -> f(X,Y,Z)     :- .....
--Goal [t1,t2]                    t1      :-      t1s     ,  t2 :- t2s
--f(X,Y,Z) , g(X,K,L)
-}


{-

rename :: Rule -> Forbidden -> Rule
rename (Rule r rs) vs = let   (t:ts) = renameWild (r:rs) []
                              (Rule x xs) = (Rule t ts)
                              substi = mStoSubst (buildSubst (allVars(Rule x xs)) vs emptyMS) in
                        Rule (apply substi x) (map (apply substi) xs)

buildSubst :: [VarName] -> Forbidden -> MyState -> MyState
buildSubst [] _ st        = st
buildSubst (v:vs) fbs st  = if((getVar v st) == Nothing) then
                              buildSubst vs fbs (expandState st v (getUnusedVF st fbs))
                            else buildSubst vs fbs st

-}




-------------------------






--  let (Rule renamedRT renamedRTS) = rename (Rule rt rts) (allVars (head ts)),
--  let maybeSubst = (unify (head ts) (rename r (allVars (head ts)))),
--  let newGoal = apply subst rts ++ ]




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

---------------- Unifikation -----------------------

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
