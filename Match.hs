{- Match.hs
 -}

module Match ( syntaxErr, matchSym, matchID, matchInt, matchEOF, testID, extract ) where

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

matchErr :: String -> Symbol -> Line -> State
matchErr e EOF _ = error ("Expected " ++ show e ++ "; received EOF.")
matchErr e g l = error ("Expected " ++ show e ++ "; received " ++ show g ++ ".\n" ++ show l)

syntaxErr :: [Symbol] -> State -> State
syntaxErr valid (State (Tape l f r) s) = State (mover (Tape l (SYNTAXERR valid [f]) r)) s

{- matchSym - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
matchSym :: Symbol -> State -> State
matchSym s st | extract st == s	= st { tape = mover (tape st) }
	      | otherwise	= syntaxErr [s] st

matchEOF st | extract st == EOF = st
	    | otherwise		= syntaxErr [EOF] st

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