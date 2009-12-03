{- Match.hs
 -}

module Match ( match, matchSynch ) where

{- We need to know about Symbols -}
import Symbol ( Symbol ( SYNTAXERR ) )
import Defs ( Token, sym )
import Production ( Production )
import Error ( syntaxErr, resolveErr )

{- match - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
match :: Symbol -> Production
match s (t:ts)	| sym t == s			= return ts
		| otherwise			= syntaxErr [s] (t:ts)


-- matchSync
-- If we're matching something we'd like to call a synchronizing token, then
-- resolve any errors right away.
matchSynch :: Symbol -> Production
matchSynch s ts = do
			ts'@(t:_) <- match s ts
			case sym t of
				SYNTAXERR _ _	-> resolveErr [s] ts'
				_		-> return ts'
