module TypeExtension () where

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
