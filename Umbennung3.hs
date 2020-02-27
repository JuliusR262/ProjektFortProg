module Umbennung where

import           Control.Monad.State
import           Type
import           Vars


data RState = RState { taken  :: [VarName]
                     , mapped :: [(VarName, VarName)]
                     }

type RS a = State RState a -- implement map for same variables etc.

class Rename a where
  rename :: [VarName] -> a -> a

  renameS :: a -> RS a

instance Rename Term where
  rename vs t = evalState (renameS t) (RState vs [])

  renameS (Var v) = do
    m <- gets mapped
    vs <- gets taken
    let x = head $ getFresh vs -- head is safe since freshVars is infinite.
    case lookup v m of
      Just v' -> return $ Var v'
      Nothing -> if v == "_"
                 then do modify $ \s -> s {taken = (x:vs), mapped = (x, "_"):m}
                         return $ Var x
                 else do modify $ \s -> s {taken = (x:vs), mapped = (v, x):m}
                         return $ Var x
  renameS (Comb s ts) = Comb s <$> mapM renameS ts

getFresh :: [VarName] -> [VarName]
getFresh used = [ x | x <- freshVars, not $ elem x used]

instance Rename Rule where
  rename vs r = evalState (renameS r) (RState vs [])

  renameS (Rule t ts) = Rule <$> renameS t <*> mapM renameS ts
