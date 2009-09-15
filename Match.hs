{- Match.hs
 -}

module Match ( match, matchErr, matchSym, matchID, matchInt, matchEOF, testID ) where

{- We need to know what about Symbols -}
import Defs
import Test
import Data.List ( nub )

{- match and matchSym can be thought of as templates, which, when given an
 - argument, become productions which match a single terminal
 -}
match :: (Symbol -> Bool) -> State -> State
match b (State (Tape _ (Token NoLine EOF) _) _) = error "Not enough tokens..."
match b (t:ts) = if b (sym t) then ts else error ("Failed match " ++ show (t:ts))

matchErr :: String -> Symbol -> Line -> State
matchErr e EOF _ = error ("Expected " ++ (show e) ++ "; received EOF.")
matchErr e g l = error ("Expected " ++ (show e) ++ "; received " ++ (show g) ++ ".\n" ++ (show l))

{- matchSym - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
matchSym :: Symbol -> State -> State
matchSym s [] = matchErr (show s) EOF NoLine
matchSym s (t:ts) | sym t == s	= ts
		  | otherwise	= matchErr (show s) (sym t) (line t)

testID :: Symbol -> Bool
testID (REF _) = True
testID (ID _) = True
testID _ = False

matchID :: [Token] -> [Token]
matchID [] = matchErr "identifier" EOF NoLine
matchID ((Token _ (REF _)):ts) = ts
matchID ((Token _ (ID _)):ts) = ts -- Only useful if we don't tabulate
matchID (t:ts) = matchErr "identifier" (sym t) (line t)

matchInt [] = matchErr "integer" EOF NoLine
matchInt ((Token _ (INT _)):ts) = ts
matchInt (t:ts) = matchErr "integer" (sym t) (line t)

matchEOF :: [Token] -> [Token]
matchEOF [] = []
matchEOF ts = error ("Expected EOF, received: " ++ remainder ts)
	where
		remainder :: [Token] -> String
		remainder ts = unlines (map show (nub (map line ts)))