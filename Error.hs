{- Error.hs
 -
 - This stuff is sort of related to match, but it's messy enough to warrant its
 - own file.
 -}

module Error ( syntaxErr, resolveErr, checkLexErr, checkScopeErr ) where

{- We need to know about Symbols -}
import Symbol ( Symbol ( LEXERR, SYNTAXERR, EOF, ID ), LexErrType ( LONGID ),
		isSyntaxErr, isLexErr )
import Defs ( Token ( Token), Line (Line, NoLine), sym )
import Production ( Production, epsilon, reportErr, pushType )
import Compute ( Compute, getDisplay, tellLeft )
import Space (lookupInScope)
import Type (Type (NULL_t))

import Control.Monad (liftM)


-- syntaxErr
-- Simply note that the current symbol is a syntax error, store a list of valid
-- symbols (for reporting). We'll continue on until we reach a nice collection
-- point, at which time we will calmly panic.
syntaxErr :: [Symbol] -> Production
syntaxErr _ ts@(Token _ (SYNTAXERR _ _):_) = return ts
syntaxErr val (t@(Token l s): ts) = return $ Token l (SYNTAXERR val s) : t : ts

-- resolveErr
-- Check for a syntax error, report it, and panic
resolveErr :: [Symbol] -> Production
resolveErr _ [] = epsilon []
resolveErr valid (t:ts) | isSyntaxErr $ sym t = reportErr t
					     >> resolveErr' valid ts
			| otherwise	      = epsilon (t:ts)

-- resolveErr'
-- Panic, noting lexical errors as we go, until we find a synchronizing
-- token.
resolveErr' :: [Symbol] -> Production
resolveErr' valid (t:ts) | inSynch (sym t) valid = checkLexErr t
-- 						>> tellLeft NoLine ("Resolved on: " ++ show t)
						>> return ts
			 | otherwise		 = checkLexErr t
			 			>> resolveErr' valid ts

-- checkLexErr
-- Report a lexical error, if one is found
checkLexErr :: Token -> Compute ()
checkLexErr t	| isLexErr (sym t)	= reportErr t
		| otherwise 		= return ()

checkScopeErr :: Token -> Compute ()
checkScopeErr t@(Token _ (ID n))		 = checkScopeErr' t n
checkScopeErr t@(Token _ (LEXERR LONGID (ID n))) = checkScopeErr' t n

-- checkScopeErr'
-- We're cheating a little bit here, but since this is only ever called in
-- the context of validating a variable's scope, we'll discover its type
-- and push it onto the stack, if we can. (NULL if not)
checkScopeErr' :: Token -> String -> Compute ()
checkScopeErr' (Token l@(Line num line) sy) n = do
			var <- lookupInScope n `liftM` getDisplay
			case var of
				Just t -> pushType t >> return ()
				Nothing -> pushType NULL_t >> tellLeft l
					("Line " ++ show num ++ ": Variable not in scope: " ++ show sy)

-- inSynch
-- Check to see if we're in the synch set
-- synch(P) = follow(P) ++ [EOF]
inSynch :: Symbol -> [Symbol] -> Bool
inSynch EOF _ = True
inSynch r follow = r `elem` follow
