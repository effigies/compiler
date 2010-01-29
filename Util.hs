{- Util.hs
 -
 - Some general purpose utilities
 -}
module Util (join, contains)
	where

join :: String -> [String] -> String
join _ [] = ""
join _ [s] = s
join c (s:ss) = s ++ c ++ join c ss

contains :: Eq a => [a] -> a -> Bool
contains = flip elem

