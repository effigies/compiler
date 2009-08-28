{- Token.hs
 - A permissive tokenizer
 -
 - Defines permissible symbols, and tokenizes a stream, checking for only the
 - most basic lexical errors.
 -
 - Different types of tokens are treated slightly differently. Whitespace,
 - words, and numbers are treated greedily. Addops, mulops, and delimeters are
 - treated as entire lexemes. Relops are clustered in groups of two where
 - possible. (So "<>><>" splits into "<>", "><" and ">", two of which will
 - produce a RELOP, and one a LEXERR.)
 -
 - Through greed, we find the end of the intended token without worrying
 - whether the token is valid, leaving that to another layer of analysis.
 -
 - Thus, the only LEXERRs to arise from tokenize are improper RELOPs and
 - unexpected characters. We leave the more sophisticated lexical errors to
 - another module.
 -}

module Token ( tokenize, reserved, isReserved ) where

import Defs ( Symbol (..), LexErrType (UNREC) )

import Data.List ( nub )
import Tape      ( Tape(Tape), mover, cutl, left )
import Char      ( isDigit, isAlpha, isAlphaNum, isSpace )

contains :: Eq a => [a] -> a -> Bool
contains = flip elem

-- Shorthand
type Stream = Tape Char

-- Full description of unambiguous operators and delimeters
mulops		= "*/"
addops		= "+-"
relops		= [ "=", "<>", "<", "<=", ">", ">=" ]
delims		= [ "[", "]", "(", ")", ",", ";" ]

-- Op words
mulopWords	= [ "div", "mod", "and" ]
addopWords	= [ "or" ]

-- Reserved words
reserved = [	"program", "var", "array", "of", "integer", "real", "function",
		"procedure", "begin", "end", "if", "then", "else", "while",
		"do", "not" ]

-- Ambiguous characters
ambiguous = ":."

-- Useful predicates (We've already imported some from Char)
isAddOp		= contains addops
isMulOp		= contains mulops
isRelOp		= contains (nub (concat relops))
isDelim		= contains (concat delims)
isAmbig		= contains ambiguous
isReserved	= contains reserved

-- The tokenizer is a massively mutually-recursive beast. It is essentially the
-- head of our NFA (we simulate backtracking through pattern-matching), and
-- decides which matcher to pass the string onto. Each of these matchers calls
-- tokenizer when done.
tokenize :: Stream -> [Symbol]
tokenize (Tape "" h rs)	| rs == []	= tokenize' (Tape "" h "\0")
			| otherwise	= tokenize' (Tape "" h rs)
tokenize t = error "Tokenizer should receive the head of a tape."

tokenize' :: Stream -> [Symbol]
tokenize' t@(Tape "" h _) | isSpace h	= matchWS (mover t)
			  | isAlpha h	= matchWord (mover t)
			  | isAmbig h	= handleAmbiguous (mover t)
			  | isAddOp h	= ADDOP [h] : continuer t
			  | isMulOp h	= MULOP [h] : continuer t
			  | isRelOp h	= matchRelOp (mover t)
			  | isDigit h	= matchInt (mover t)
			  | isDelim h	= DELIM [h] : continuer t
			  | h == '\0'	= []
			  | otherwise	= LEXERR UNREC [h] : continuer t

-- These patterns appear frequently
continue = tokenize . cutl
continuer = continue . mover


-- Ambiguous initial characters

-- Minus can either be an operator or modify a numeric literal
--handleAmbiguous t@(Tape "-" h _) | isDigit h = matchInt (mover t)
--				 | h == '.'  = matchFrac (mover t)
--				 | otherwise = ADDOP "-" : continue t

-- Colon may be a lexeme in itself, or the first part of the assignment op
handleAmbiguous t@(Tape ":" h _) | h == '='  = ASSIGNOP  : continuer t
				 | otherwise = DELIM ":" : continue t

-- Dot may indicate the end of the program, or start off a real
handleAmbiguous t@(Tape "." h _) | isDigit h = matchFrac (mover t)
				 | h == '.'  = DELIM ".." : continuer t
				 | otherwise = DOT : continue t

-- Match whitespace
matchWS :: Stream -> [Symbol]
matchWS t@(Tape _ h []) | isSpace h = [ WHITESPACE ]
			| otherwise = WHITESPACE : continue t

matchWS t@(Tape _ h  _)	| isSpace h = matchWS (mover t)
			| otherwise = WHITESPACE : continue t

-- Match words
matchWord :: Stream -> [Symbol]
matchWord t@(Tape _ h _) | isAlphaNum h = matchWord (mover t)
			 | otherwise	= wrapWord (left t) : continue t

wrapWord :: String -> Symbol
wrapWord word	| word `elem` addopWords = ADDOP word
		| word `elem` mulopWords = MULOP word
		| otherwise		 = NAME    word
--		| word `elem` reserved   = RES   word
--		| otherwise		 = ID    word

-- Match numbers
matchInt t@(Tape _ h _) | isDigit h = matchInt (mover t)
			| h == '.'  = matchFrac (mover t)
			| h == 'e' || h == 'E' = matchExp (mover t)
			| otherwise = INT (left t) : continue t

matchFrac t@(Tape _ h _) | isDigit h = matchFrac (mover t)
			 | h == 'e' || h == 'E' = matchExp (mover t)
			 | otherwise = REAL (left t) : continue t

matchExp t@(Tape _ h _)	| h `elem` "+-" = matchExp' (mover t)
			| otherwise	= matchExp' t

matchExp' t@(Tape _ h _) | isDigit h = matchExp' (mover t)
			 | otherwise = BIGREAL (left t) : continue t

-- Match relational operators
matchRelOp t@(Tape _ h _) | isRelOp h		= matchRelOp' (mover t)
			  | otherwise		= RELOP (left t) : continue t

matchRelOp' t = let tok = left t in
		if tok `elem` relops
		then RELOP tok : continue t
		else LEXERR UNREC tok : continue t
