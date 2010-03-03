{- Parser.hs
 -
 - Parse an LL(1) grammar
 -
 - Rather than using IO to take the output of the lexical analyzer,
 - we'll simply subsume its behavior.
 -}

import Symbol ( Symbol (EOF) )
import Type ( Type (NULL_t) )
import Defs ( Token (Token), Line (NoLine), line )
import Lex ( scan )
import Grammar ( program )
import Compute ( Context (Context) )

import Space (Space (Space), Cxt (Top))
import Data.Map (empty)

import Data.List (nub, sort)

import Control.Monad (liftM)

import Control.Monad.Writer (runWriter)
import Control.Monad.State ( StateT (StateT) )

import IO (openFile, Handle, hPutStrLn, hClose, IOMode (WriteMode)) 

end = Token NoLine EOF

main = do
	input <- lines `liftM` getContents
	listingFile	<- openFile "listing" WriteMode
	let tokens = scan input ++ [end]
	let StateT parser = program tokens
	let init = Context (Space "GLOBAL" NULL_t empty empty, Top) [] []
	(errors, sizes) <- return . snd . runWriter $ parser init
	writeList listingFile NoLine tokens errors
	hClose listingFile
-- 	mapM_ (putStrLn . show) errors
	mapM_ putStrLn sizes
{-
parse :: [String] -> (([Token],Context),([String],[String]))
parse input = let StateT parser = program $ scan input ++ [end]
		  init = Context (Space "GLOBAL" NULL_t empty empty, Top) [] []
		in runWriter $ parser init
-}
writeList :: Handle -> Line -> [Token] -> [(Line,String)] -> IO ()
writeList h l = writeList' h l . nub . map line

writeList' :: Handle -> Line -> [Line] -> [(Line,String)] -> IO ()
writeList' file _ [] es = mapM_ (hPutStrLn file . ("ERROR: " ++) . show) es
writeList' file _ ls [] = mapM_ (hPutStrLn file . ("\n" ++) . show) ls
writeList' file c (l:ls) ((l',e):es) =
	if l' == c
		then do
			hPutStrLn file $ "ERROR: " ++ e
			writeList' file c (l:ls) es
		else do
			hPutStrLn file $ '\n' : show l
			writeList' file l ls ((l',e):es)
