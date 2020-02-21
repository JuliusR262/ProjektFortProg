 import Control.Monad.State
 import Vars
 import Type
 
 type MyState = [(VarName, VarName)]
 
 valFromState :: MyState -> VarName
 valFromState v = let usedVars = snd (unzip v)
                  in vhelp' usedVars freshVars
 vhelp' :: [VarName] -> [VarName] -> VarName
 vhelp' usedVars (x:xs) = if elem x usedVars then vhelp' usedVars xs else x
 
 
 
 nextState :: MyState -> VarName -> VarName -> MyState
 nextState state newVar assignedVar = (newVar, assignedVar) : state
 
 type MyStateMonad = State MyState
 
 getNext :: VarName -> MyStateMonad VarName
 getNext newVar = state (\st -> let 
                                    oldVal = valFromState st
                                    st' = nextState st newVar oldVal 
                                in (valFromState(st'), st') )
 

 
 mystate :: MyStateMonad VarName
 mystate = do x <- getNext "A"    [("A", "_1")]
              z <- getNext "B"    
              y <- getNext "C"
              newD <- getNext "_" ("_", "__1")
                  
              return z
