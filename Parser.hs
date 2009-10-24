{- Parser.hs
 -
 - Parse an LL(1) grammar
 -
 - Rather than using IO to take the output of the lexical analyzer,
 - we'll simply subsume its behavior.
 -}

import Tape
import Defs
import Token
import Grammar
import Test

type LexOut = ([Symbol],[Token])

end = Token NoLine EOF

main = do
	input <- getContents
	results <- return . parse . lines $ input
	mapM (putStrLn) (report results)

parse input = let (tab,toks) = scan input in
	program (State (tapify' end toks) tab)

report :: State -> [String]
report st = (detape . tape $ st) >>= report' (table st)

report' :: [Symbol] -> Token -> [String]
report' table t = case t of
			SYNTAXERR _ _		-> handleSynErr table t
			Token _ (LEXERR _ _)	-> handleLexErr t
			Token _ (REF n)		-> checkRef t { sym = table !! (n - 1) }
			_			-> []

checkRef :: Token -> [String]
checkRef t@(Token _ (LEXERR _ _)) = handleLexErr t
checkRef _ = []

handleLexErr :: Token -> [String]
handleLexErr (Token (Line n _) (LEXERR t s)) = ["Lexical Error: Received Line "
						++ show n ++ ": " ++ s ++ " ("
						++ show t ++ ")"]

handleSynErr :: [Symbol] -> Token -> [String]
handleSynErr table (SYNTAXERR v cs@(Token (Line n _) (LEXERR _ c):_)) =
	("Syntax Error:  Received Line " ++ show n ++ ": " ++ show c ++ "; Expected " ++ showValid v)
	: (cs >>= report' table)
handleSynErr table (SYNTAXERR v (c:cs)) =
	("Syntax Error:  Received " ++ show c ++ "; Expected " ++ showValid v)
	: (cs >>= report' table) -- Catch any LEXERRs

showValid :: [Symbol] -> String
showValid [t] = show t
showValid ts = showValid' ts

showValid' :: [Symbol] -> String
showValid' [t] = "or " ++ show t
showValid' (t:ts) = show t ++ ", " ++ showValid' ts

showt :: [Symbol] -> Token -> String
showt table (Token (Line l _) (REF n)) = show l ++ ": " ++ show (table !! n)
showt _ t = show t

-- Revision of Lex. More modular.

scan :: [String] -> LexOut
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

-- This stuff used to be PreProcess.

-- Constraints
maxIDLen, maxIntLen, maxWholeLen, maxFracLen, maxExpLen	:: Int
maxIDLen	= 10
maxIntLen	= 10
maxWholeLen	= 5
maxFracLen	= 5
maxExpLen	= 2

fixup :: Token -> [Token]
fixup t = case (sym t) of
	WHITESPACE	-> []
	NAME n		-> [t { sym = nameCheck	n }]
	INT i		-> [t { sym = intCheck	i }]
	REAL r		-> [t { sym = realCheck	r }]
	BIGREAL b	-> [t { sym = bigRealCheck	b }]
	_		-> [t]

nameCheck :: String -> Symbol
nameCheck name	| length name > maxIDLen	= LEXERR LONGID name
		| isReserved name		= RES name
		| otherwise			= ID name

intCheck :: String -> Symbol
intCheck int   | length int <= maxIntLen	= INT int
	       | otherwise			= LEXERR LONGINT int

realCheck :: String -> Symbol
realCheck real = let (whole, dot:frac) = span (/= '.') real in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE real
		else if length frac > maxFracLen then
			LEXERR LONGFRAC real
		else REAL real

bigRealCheck :: String -> Symbol
bigRealCheck bigReal = let (real, e:exp)     = break (`elem` "eE") bigReal
			   (whole, frac) = span (/= '.') real
			   exp' = if head exp `elem` "+-"
				  then tail exp
				  else exp
		in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE bigReal
		else if length frac > (maxFracLen + 1) then
			LEXERR LONGFRAC bigReal
		else if length exp' > maxExpLen then
			LEXERR LONGEXP bigReal
		else BIGREAL bigReal