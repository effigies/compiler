{- Lex.hs
 - Lexical analyzer - control and interpretation component
 -
 -}

module Lex (symbolTable, insert) where

import IO
import Defs
import PreProcess
import Token
import Tape

process :: Int -> [String] -> [Symbol] -> Handle -> Handle -> IO ( )
process n rs table list tok = let next = (\t -> process (n + 1) rs t list tok)
		in do
			line			<- getLine
			tape			<- return (tapify (line ++ "\0"))
			tokens  		<- return (tokenize tape) 
			tokens'			<- return (preprocess rs tokens)
			(tokens'', table')	<- return (symbolTable table tokens')
			hPutStrLn list ((shows n . ('\t':)) line)
			if length line > 72 then hPutStrLn list "Line too long" else return ()
			writeList list tokens'' table'
			writeTokens tok n tokens'' table'
--			hPutStrLn tok (show tokens'')
--			putStrLn (show table')
			catch (next table') (\_ -> (writeTokens tok (n+1) [EOF] []))
			return ( )

writeList :: Handle -> [Symbol] -> [Symbol] -> IO ( )
writeList file [] table = return ()
writeList file (t:ts) table = do
	case t of
		LEXERR _ _ -> hPutStrLn file (show t)
		_	   -> return ()
	writeList file ts table

writeTokens :: Handle -> Int -> [Symbol] -> [Symbol] -> IO ( )
writeTokens file n [] table = return ()
writeTokens file n (t:ts) table = do
	hPutStrLn file ((shows n . ('\t':)) (show t))
	writeTokens file n ts table

insert :: Int -> Symbol -> [Symbol] -> (Symbol,[Symbol])
insert n id []		= (REF n, [id])
insert n id (e:es)	| id == e	= (REF n, e:es)
			| otherwise	= let (ref,es') = insert (n + 1) id es
					in (ref, e:es')

-- Takes a symbol table and a sequence of symbols, and returns (a,b) where
--    a is the sequence of symbols, with identifiers replaced with table refs
--    b is the updated symbol table
symbolTable :: [Symbol] -> [Symbol] -> ([Symbol],[Symbol])
symbolTable table []	 = ([],table)
symbolTable table (t:ts) = let (ref, table') = if t == ID "_" then insert 1 t table
							 else (t,table)
			       (refs, table'') = symbolTable table' ts
				  in (ref:refs,table'')

--symbolTable table tokens = symbolTable' table (filter isID tokens)
--symbolTable' table []		= ([],table)
--symbolTable' table (id:ids)	= let (ref, table') = insert 1 id table
--				      (refs, table'') = symbolTable' table' ids
--				in (ref:refs,table'')

main :: IO ( )
main = do
	-- Read in reserved words (per project parameters)
	reservedFile	<- openFile "reserved" ReadMode
	r		<- hGetContents reservedFile
	reserved	<- return (lines r)

	-- Listing file - track lexical errors
	listingFile	<- openFile "listing" WriteMode

	-- Token file - complete listing of tokens and errors, with reference
	-- to the symbol table where appropriate
	tokenFile	<- openFile "tokens"  WriteMode

	process 1 reserved [] listingFile tokenFile

	-- Clean up
	hClose listingFile
	hClose tokenFile
	hClose reservedFile
