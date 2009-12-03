{- Lex.hs
 - Lexical analyzer - control and interpretation component
 -
 -}

import Tape (tapify')
import Defs (Token (Token), sym, Line (Line, NoLine) )
import Token (tokenize)
import PreProcess (fixup)

import Symbol (	Symbol (REF), isLexErr, isID )

import IO (openFile, Handle, hPutStrLn, hClose, IOMode (WriteMode)) 
import Monad (mapM, when)

main :: IO ()
main = do
	input <- getContents
	(table, tokens) <- return . scan . lines $ input
	listingFile	<- openFile "listing" WriteMode
	tokenFile	<- openFile "tokens"  WriteMode
	writeList listingFile NoLine tokens
	mapM_ (hPutStrLn tokenFile . show) tokens
	hClose listingFile
	hClose tokenFile

writeList :: Handle -> Line -> [Token] -> IO ()
writeList _ _ [] = return ()
writeList file l (Token l'@(Line n text) s:ts) = do
	when (l /= l') $ hPutStrLn file (show n ++ ":\t" ++ text)
	when (isLexErr s) $ hPutStrLn file (show s)
	writeList file l' ts

scan :: [String] -> ([Symbol],[Token])
scan input = tabulate (zipWith Line [1..] input >>= scan' >>= fixup)

scan' :: Line -> [Token]
scan' l@(Line _ text) = map (Token l) (tokenize (tapify' '\0' text))

-- tabulate - construct symbol table, and replace IDs with REFs
tabulate :: [Token] -> ([Symbol],[Token])
tabulate = tabulate' []

-- tabulate' - do the real work of tabulate
-- 
-- The first line gets us the "current" symbol table, and token
-- If the token is an ID, it updates the table, and gives us a REF
-- Otherwise, the table and token remain unchanged
-- The second line recursively tabulates
-- The final line prepends our token, and passes the final symbol table up
tabulate' :: [Symbol] -> [Token] -> ([Symbol],[Token])
tabulate' tab []     = (tab,[])
tabulate' tab (t:ts) = let (tab',r) = if isID . sym $ t then insert tab t
						else (tab,t)
			   (tab'',rs) = tabulate' tab' ts
			in (tab'',r:rs)

-- insert - Given a symbol table and a token,
--          insert the token into the symbol table
insert :: [Symbol] -> Token -> ([Symbol],Token)
insert = insert' 1

-- insert' - Insert a token into a symbol table, with base index n
insert' :: Int -> [Symbol] -> Token -> ([Symbol], Token)
insert' n [] tok	= ([sym tok], tok {sym = REF n})
insert' n (t:tab) tok	| t == sym tok	= (t:tab, tok {sym = REF n})
			| otherwise	= let (tab', r) = insert' (n+1) tab tok
					in (t:tab', r)
