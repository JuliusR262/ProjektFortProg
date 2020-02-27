module Util (fromRight') where

fromRight' :: b -> Either a b -> b
fromRight' _ (Right b)  = b
fromRight' b  _         = b
