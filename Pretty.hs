module Pretty (Pretty, pretty) where

import Type
import Data.List

class Pretty a where
  pretty :: a -> String

-- instance pretty Maybe
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = ""
  pretty (Just x)  = pretty x

-- instance pretty Term
-- makes a Term more readable
instance Pretty (Term) where
  pretty (Var vName)          = vName
  pretty (Comb cName [])      = cName
  pretty (Comb "." [t1,t2])   = "[" ++ pretty'(t1,t2) ++ "]"
   where
    pretty' :: (Term,Term) -> String
    pretty' (t3,t4)   = 
      case t4 of
          Comb "[]" []        -> pretty t3
          Comb "." [t5,t6]    -> pretty t3 ++ ", " ++ pretty'(t5,t6) 
          _                   -> pretty t3 ++ "|"  ++ pretty t4 
    
  pretty (Comb cName t2)      = cName ++ "(" 
                                      ++ intercalate ", " (map pretty (t2))
                                      ++ ")"