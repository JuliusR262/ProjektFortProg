module Pretty (Pretty, pretty) where

import Type
import Data.List

class Pretty a where
  pretty :: a -> String

-- Instanz pretty Maybe
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = ""
  pretty (Just x)  = pretty x

-- Instanz pretty Term
instance Pretty (Term) where
  pretty (Var vName)          = vName
  pretty (Comb cName [])      = cName
  -- structure .(E,L)
  pretty (Comb "." [t1,t2])   = case t2 of
                                  -- if t2 is Comb and empty
                                  Comb "[]" []  ->  "[" ++ pretty t1 ++ "]"
                                  -- if t2 is Comb ist and continue the List
                                  --    like structure .(E,L)
                                  -- init and tail to delete the
                                  --    additional brackets
                                  Comb "." [t3,t4]   ->  "[" ++ pretty t1 
                                                             ++ ", " 
                                                             ++ init ( tail (
                                                              pretty( 
                                                              Comb "." [t3,t4]
                                                                 )))
                                                        ++ "]"
                                  -- if the second term is a variable or
                                  -- the list doesnt continue list by structure
                                  _             ->  "[" ++ pretty t1 ++ "|" 
                                                        ++  pretty t2 ++ "]"
  
  -- if list in Comb is longer because structure list in Prolog
  pretty (Comb cName t2)      = cName ++ "(" 
                                      ++ intercalate ", " (map pretty (t2))
                                      ++ ")"
