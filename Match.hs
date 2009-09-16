{- Match.hs
 -}

module Match ( match, matchErr, matchSym, matchID, matchInt, matchEOF, testID ) where

{- We need to know what about Symbols -}
import Defs
import Tape
import Test
import Data.List ( nub )

isEOF :: State -> Bool
isEOF = (EOF ==) . extract
--isEOF (State (Tape _ (Token NoLine EOF) _) _)	= True
--isEOF _						= False

extract :: State -> Symbol
extract (State (Tape _ (Token _ s) _) _)	= s

{- match and matchSym can be thought of as templates, which, when given an
 - argument, become productions which match a single terminal
 -}
match :: (Symbol -> Bool) -> State -> State
match b s | isEOF s = error "Not enough tokens..."
match b s | (b . extract) s	= s { tape = mover (tape s) }
	  | otherwise		= error ("Failed match " ++ show ((focus . tape) s : (right . tape) s))

matchErr :: String -> Symbol -> Line -> State
matchErr e EOF _ = error ("Expected " ++ (show e) ++ "; received EOF.")
matchErr e g l = error ("Expected " ++ (show e) ++ "; received " ++ (show g) ++ ".\n" ++ (show l))

{- matchSym - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
matchSym :: Symbol -> State -> State
--matchSym s st | isEOF st = matchErr (show s) EOF NoLine
matchSym s st | extract st == s	= st { tape = mover (tape st) }	      
	      | otherwise	= matchErr (show s) (extract st) ((line . focus . tape) st)

matchEOF st | extract st == EOF = st
	    | otherwise		= matchErr (show EOF) (extract st) ((line . focus . tape) st)

testID :: Symbol -> Bool
testID (REF _) = True
testID (ID _) = True
testID _ = False

matchID :: State -> State
matchID s | testID (extract s)	= s { tape = mover (tape s) }
	  | otherwise		= matchErr "identifier" (extract s) ((line . focus . tape) s)

matchInt :: State -> State
matchInt s | isINT (extract s)	= s { tape = mover (tape s) }
	   | otherwise		= matchErr "integer" (extract s) ((line . focus . tape) s)
--matchInt [] = matchErr "integer" EOF NoLine
--matchInt ((Token _ (INT _)):ts) = ts
--matchInt (t:ts) = matchErr "integer" (sym t) (line t)

{-
matchEOF :: State -> State
matchEOF [] = []
matchEOF ts = error ("Expected EOF, received: " ++ remainder ts)
	where
		remainder :: State -> String
		remainder ts = unlines (map show (nub (map line ts)))
-}