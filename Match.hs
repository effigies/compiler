{- Match.hs
 -}

module Match ( match, matchSynch, matchEOF ) where

{- We need to know about Symbols -}
import Defs
import Tape
import Test
import Error

{- match - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
match :: Symbol -> State -> State
match s st | extract st == s	= st { tape = mover (tape st) }
	   | otherwise		= syntaxErr [s] st

-- matchSync
-- If we're matching something we'd like to call a synchronizing token, then
-- resolve any errors right away.
matchSynch :: Symbol -> State -> State
matchSynch s st | extract st == s	= st { tape = mover (tape st) }
		| otherwise		= resolveErr [s] (syntaxErr [s] st)

matchEOF :: State -> State
matchEOF st | extract st == EOF = st
	    | otherwise		= syntaxErr [EOF] st