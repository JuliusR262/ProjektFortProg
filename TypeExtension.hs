module TypeExtension () where

import Type
import Vars

instance Eq Term where
    (Var vName1) == (Var vName2) = vName1 == vName2
    (Var _) == (Comb _ _) = False
    (Comb _ _) == (Var _) = False
    (Comb cName1 cList1) == (Comb cName2 cList2) = cName1 == cName2 && cList1 == cList2
    
    t1 /= t2 = not (t1 == t2)

