{- Match.hs
 -}

module Match ( match, matchSynch,
		matchProgName, matchLowerBound, matchUpperBound,
		matchIdent, matchName,
	) where

{- We need to know about Symbols -}
import Symbol ( Symbol ( SYNTAXERR, VAR, ID, INT ) )
import Defs ( Token (Token), sym )
import Compute
import Production
import Space
import Error ( syntaxErr, resolveErr )
import Type

{- Here's our most general matcher.
 - On a match, it executes a function on the matched token, and returns
 - the remainder of the list.
 - On a failure, it registers a syntax error.
 -}
matchWithEffects :: (Token -> Compute a) -> Production -> Symbol -> Production
matchWithEffects m f s (t:ts)	| sym t == s	= m t >> return ts
				| otherwise	= syntaxErr [s] (t:ts) >>= f

{- Basic match. We eschew side effects, here. -}
match :: Symbol -> Production
match = matchWithEffects return return

matchVar :: (String -> Compute a) -> Production
matchVar f = matchWithEffects f' return VAR
	where
		f' (Token _ (ID n)) = f n

{- When we match the program name, we want to replace the
 - label in the top scope with the name in the token.
 -}
matchProgName :: Production
matchProgName = matchVar $ \n -> modifyDisplay (\(s,t) -> (s { label = n }, t))

matchIdent :: Production
matchIdent = matchVar $ flip insertVariable NULL_t

matchName :: Production
matchName = matchVar putName

matchLowerBound :: Production
matchLowerBound = matchWithEffects f return (INT "_")
	where
		f (Token _ (INT n)) = do
					t <- popType
					pushType $ t { lBound = read n }
					return ()

matchUpperBound :: Production
matchUpperBound = matchWithEffects f return (INT "_")
	where
		f (Token _ (INT n)) = do
					t <- popType
					pushType $ t { uBound = read n }
					return ()

-- matchSync
-- If we're matching something we'd like to call a synchronizing token, then
-- resolve any errors right away.
matchSynch s = matchWithEffects return f s
	where
		f ts@(Token _ (SYNTAXERR _ _):_) = resolveErr [s] ts
		f ts				 = return ts
