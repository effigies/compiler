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
-- import Lex
import Grammar
import Test

type LexOut = ([Symbol],[Token])

end = Token NoLine EOF

{-
scan :: [String] -> LexOut
scan input = scan' =<< (zipWith Line [1..] input)
-}

scan :: [String] -> LexOut
scan input = tabulate (zipWith Line [1..] input >>= scan' >>= fixup)

scan' :: Line -> [Token]
scan' l@(Line _ text) = map (Token l) (tokenize (tapify' text))

fixup :: Token -> [Token]
fixup t = case (sym t) of
	WHITESPACE	-> []
	NAME n		-> [t { sym = if isReserved n then RES n else ID n }]
	_		-> [t]

ntabulate :: [nToken] -> [nToken]
ntabulate = ntabulate' []

ntabulate :: [Symbol] -> [nToken] -> [nToken]
ntabulate _ [] = []
ntabulate table (t:ts)	| isID (sym t ) = let (tab',r) = insert tab t
					in r { tab = tab' } : ntabulate tab' ts
			| otherwise	= t { tab = table } : ntabulate table ts

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
tabulate' tab (t:ts) = let (tab',r) = if isID (sym t) then insert tab t
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