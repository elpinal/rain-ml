module Language.RainML.Parser
  ( parse
  ) where

import Data.Char

parse :: String -> Maybe Int
parse []      = Nothing
parse (x : _) = return $ digitToInt x
