module Pretty (Pretty, pretty) where

import Data.List (intercalate)

import Type

class Pretty a where
  pretty :: a -> String

-- Instance of pretty for Maybe.
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = ""
  pretty (Just x)  = pretty x

-- Turns a Goal into a pretty String.
instance Pretty (Goal) where
  pretty (Goal ts) = intercalate ", " (map pretty ts)

-- Turns a Rule into a pretty String.
instance Pretty (Rule) where
  pretty (Rule t []) = pretty t ++ "."
  pretty (Rule t ts) = pretty t ++ " :- " ++ intercalate ", " (map pretty ts)

-- Turns a Prog into a pretty String.
instance Pretty (Prog) where
  pretty (Prog rs) = intercalate "\n " (map pretty rs)

-- Turns a Term into a pretty String.
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
