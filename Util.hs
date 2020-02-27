module Util (fromRight') where

-- Implements 'fromRight' for 'Either' from Haskell's 
-- base package version 4.10.0.0.
fromRight' :: b -> Either a b -> b
fromRight' _ (Right b)  = b
fromRight' b  _         = b
  
