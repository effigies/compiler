{- Error.hs
 -
 - This stuff is sort of related to match, but it's messy enough to warrant its
 - own file.
 -}

module Error ( syntaxErr, resolveErr ) where

{- We need to know about Symbols -}
import Symbol ( Symbol ( LEXERR, SYNTAXERR, EOF ), isSyntaxErr )
import Defs ( Token ( Token, sym ) )
import Production ( Production, epsilon, reportErr )
import Compute

-- syntaxErr
-- Simply note that the current symbol is a syntax error, store a list of valid
-- symbols (for reporting). We'll continue on until we reach a nice collection
-- point, at which time we will calmly panic.
syntaxErr :: [Symbol] -> Production
syntaxErr _ ts@(Token _ (SYNTAXERR _ _):_) = return ts
syntaxErr val (Token l s : ts) = return $ Token l (SYNTAXERR val s) : ts

-- resolveErr
-- Discover and resolve syntax errors, panicking until we find a synchronizing
-- token
resolveErr :: [Symbol] -> Production
resolveErr _ [] = epsilon []
resolveErr valid (t:ts) | isSyntaxErr $ sym t = reportErr t
				      >> resolveErr' valid ts
			| otherwise	= epsilon (t:ts)

resolveErr' :: [Symbol] -> Production
resolveErr' valid (t:ts) | inSynch (sym t) valid	= return ts
			 | otherwise			= resolveErr' valid ts

-- inSynch
-- Check to see if we're in the synch set
-- synch(P) = follow(P) ++ [EOF]
inSynch :: Symbol -> [Symbol] -> Bool
inSynch EOF _ = True
inSynch r follow = r `elem` follow
