{- Util.hs
 -
 - Some general purpose utilities
 -}
module Util (join, joinWith, contains, tail')
	where

join :: String -> [String] -> String
join _ [] = ""
join _ [s] = s
join sep (s:ss) = s ++ sep ++ join sep ss

joinWith _ _ [] = ""
joinWith _ _ [s] = s
joinWith _ fin [s1,s2] = s1 ++ fin ++ s2
joinWith sep fin (s:ss) = s ++ sep ++ joinWith sep fin ss

contains :: Eq a => [a] -> a -> Bool
contains = flip elem

tail' [] = []
tail' ts = tail ts
