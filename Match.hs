{- Match.hs
 -}

module Match ( match, matchSynch,
		matchProgName, matchLowerBound, matchUpperBound,
		matchIdent, matchName, matchScopedVar
	) where

{- We need to know about Symbols -}
import Symbol ( Symbol (VAR, ID, LEXERR, INT, SYNTAXERR), LexErrType (LONGID) )
import Defs ( Token (Token), sym )
import Compute ( Compute, modifyDisplay )
import Production ( Production, insertVariable, pushName, popType, pushType )
import Space ( label )
import Error ( syntaxErr, resolveErr, checkLexErr, checkScopeErr )
import Type ( Type (NULL_t), lBound, uBound )

{- Here's our most general matcher.
 - On a match, it checks for a lexical error (which are usually ignorable,
 - so won't cause a mismatch), executes a function on the matched token, and
 - returns the remainder of the list.
 - On a failure, it registers a syntax error, and performs a post-operation.
 -}
matchWithEffects :: (Token -> Compute a) -> Production -> Symbol -> Production
matchWithEffects m f s (t:ts)	| sym t == s = checkLexErr t
					    >> m t
					    >> return ts
				| otherwise  = syntaxErr [s] (t:ts)
					   >>= f


{- Basic match. We eschew side effects, here. -}
match :: Symbol -> Production
match = matchWithEffects return return

matchVar :: (String -> Compute a) -> Production
matchVar f = matchWithEffects f' return VAR
	where
		f' (Token _ (ID n)) = f n
		f' (Token _ (LEXERR LONGID (ID n))) = f n

matchScopedVar :: Production
matchScopedVar = matchWithEffects checkScopeErr return VAR

{-matchSet :: [Symbol] -> Production
matchSet = matchWithEffects return return
-}

{- When we match the program name, we want to replace the
 - label in the top scope with the name in the token.
 -}
matchProgName :: Production
matchProgName = matchVar $ \n -> modifyDisplay (\(s,t) -> (s { label = n }, t))

matchIdent :: Production
matchIdent = matchVar $ flip insertVariable NULL_t

matchName :: Production
matchName = matchVar pushName

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
