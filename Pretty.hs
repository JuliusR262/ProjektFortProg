module Pretty (Pretty, pretty) where

import Type
import Data.List

class Pretty a where
  pretty :: a -> String

-- Instanz pretty für Maybe
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = ""
  pretty (Just x)  = pretty x

-- Instanz pretty für Term
instance Pretty (Term) where
  pretty (Var vName)          = vName
  pretty (Comb cName [])      = cName
  -- Struktur .(E,L)
  pretty (Comb "." [t1,t2])   = case t2 of
                                  -- Falls t2 Comb ist und leer ist
                                  Comb "[]" []  ->  "[" ++ pretty t1 ++ "]"
                                  -- Falls t2 Comb ist und die Liste fortsetzt
                                  --    nach Struktur .(E,L)
                                  -- init und tail um zusätzliche klammern
                                  --    wieder zu entfernen
                                  Comb "." [t3,t4]   ->  "[" ++ pretty t1 
                                                             ++ ", " 
                                                             ++ init ( tail (
                                                              pretty( 
                                                              Comb "." [t3,t4]
                                                                 )))
                                                        ++ "]"
                                  -- Wenn der zweite Term Variable ist 
                                  --  oder die Liste nicht nach Struktur 
                                  --    fortsetzt
                                  _             ->  "[" ++ pretty t1 ++ "|" 
                                                        ++  pretty t2 ++ "]"
  
  -- falls Liste in Comb länger ist aufgrund Struktur Liste in Prolog
  pretty (Comb cName t2)      = cName ++ "(" 
                                      ++ intercalate ", " (map pretty (t2))
                                      ++ ")"
