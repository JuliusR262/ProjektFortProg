module Pretty (Pretty, pretty) where 

import Type
import Data.List

class Pretty a where
    pretty :: a -> String

instance Pretty (Term) where
   
    pretty (Var vName)          = vName
    pretty (Comb cName [])      = cName
    pretty (Comb "." [t1,t2])   = case t2 of 
                                    Comb "[]" []  -> "[" ++ pretty t1 ++ "]"
                                    Comb "." ts   -> "[" ++ pretty t1 ++ "," 
                                                         ++ init (tail (pretty (Comb "." ts))) 
                                                         ++ "]"
                                    _             -> "[" ++ pretty t1 ++ "|" ++  pretty t2 ++ "]"
    pretty (Comb cName t2)      = cName ++ "(" ++ intercalate ", " (map pretty (t2)) ++ ")"
   
     
