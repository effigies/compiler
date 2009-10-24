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
import PreProcess
import Lex
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