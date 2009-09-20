{- Match.hs
 -}

module Match ( syntaxErr, match, matchEOF, extract ) where

{- We need to know what about Symbols -}
import Defs
import Tape
import Test
import Data.List ( nub )

extract :: State -> Symbol
extract (State (Tape _ (Token _ s) _) _)	= s

syntaxErr :: [Symbol] -> State -> State
syntaxErr valid (State (Tape l f r) s) = State (mover (Tape l (SYNTAXERR valid [f]) r)) s

{- match - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
match :: Symbol -> State -> State
match s st | extract st == s	= st { tape = mover (tape st) }
	   | otherwise	= syntaxErr [s] st

matchEOF st | extract st == EOF = st
	    | otherwise		= syntaxErr [EOF] st