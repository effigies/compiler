{- Parser.hs
 -
 - Parse an LL(1) grammar
 -
 - Rather than using IO to take the output of the lexical analyzer,
 - we'll simply subsume its behavior.
 -}

import Symbol ( Symbol (EOF) )
import Type ( Type (NULL_t) )
import Defs ( Token (Token), Line (NoLine) )
import Lex ( scan )
import Grammar ( program )
import Compute
import Test

import Space (Space (Space), Cxt (Top))
import Data.Map (empty)

import Control.Monad.Writer (runWriter)
import Control.Monad.State ( StateT (StateT) )

end = Token NoLine EOF

main = do
	input <- getContents
	(errors, sizes) <- return . snd . parse $ lines input
	mapM putStrLn errors

parse :: [String] -> (([Token],Context),([String],[String]))
parse input = let StateT parser = program $ scan input ++ [end]
		  init = Context (Space "GLOBAL" NULL_t empty empty, Top) [] []
		in runWriter $ parser init