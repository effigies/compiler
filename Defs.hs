{- Defs.hs
 - Compiler-specific type definitions
 -}

module Defs ( Line( Line, NoLine ),
	      Token( Token, SYNTAXERR ), line, sym,
	      State( State ), tape, table,
	      Symbol( WHITESPACE, ASSIGNOP, DELIM, RELOP, MULOP, ADDOP, NAME,
			ID, REF, RES, BIGREAL, REAL, INT, LEXERR, DOT, EOF),
	      LexErrType( UNREC, LONGINT, LONGWHOLE, LONGFRAC, LONGEXP,
			LONGID),
	      isWS, isID, isINT, isREAL, isNum, isRELOP, isADDOP, isMULOP, isSIGN)
	where

import Tape

-- Line is a string with an associated number
data Line	= Line Int String
		| NoLine
	deriving (Show, Eq)

-- Token is a symbol and its containing line
data Token	= Token {line :: Line, sym :: Symbol}
		| SYNTAXERR [String] [Token]
-- Considering adding the symbol table to the token
-- data Token = Token {line :: Line, sym :: Symbol, tab :: [Symbol]}

instance Show Token where
	show (Token l s) = show l ++ " (" ++ show s ++ ")"

data State = State {tape :: (Tape Token), table :: [Symbol] }

instance Show State where
	show (State (Tape l f r) syms) = show (reverse l ++ (f:r)) ++ "\n\n" ++ show syms

-- Symbol is a (TOKEN, LEXEME) pair
-- The lexical analyzer will take a source string
-- and parse it into a list of these symbols

--   SYMBOL	= (TOKEN	,	LEXEME) -- Will match:
data Symbol	=  WHITESPACE			-- Spaces, tabs, newlines
		|  ASSIGNOP			-- :=
		|  DELIM		String	-- [ ] ( ) ,
		|  RELOP		String	-- = <> < > <= >=
		|  MULOP		String	-- * / div mod and
		|  ADDOP		String	-- + - or
		|  NAME			String	-- IDs or reserved words
		|  ID			String  -- ID
		|  REF			Int	-- Symbol table reference
		|  RES			String  -- Reserved word
		|  BIGREAL		String	-- "Big" reals (find out)
		|  REAL			String	-- Reals
		|  INT			String	-- Integers
		|  DOT				-- .
		|  EOF

		-- Lexical errors require we retain both the token and
		|  LEXERR		LexErrType String
	deriving Show

-- In case we want to be able to compare
-- (For example `mapM putStrLn (filter (/= WHITESPACE) match input)`)
instance Eq Symbol where
	WHITESPACE	== WHITESPACE	= True
	ASSIGNOP	== ASSIGNOP	= True
	EOF		== EOF		= True
	DOT		== DOT		= True
	RELOP a		== RELOP b	= a == b
	MULOP a		== MULOP b	= a == b
	ADDOP a		== ADDOP b	= a == b
	NAME a		== NAME b	= a == b
	ID a		== ID b		= a == b
	RES a		== RES b	= a == b
	REF a		== REF b	= a == b
	BIGREAL a	== BIGREAL b	= a == b
	REAL a		== REAL b	= a == b
	INT a		== INT b	= a == b
	DELIM a		== DELIM b	= a == b
	_		== _		= False

isWS :: Symbol -> Bool
isWS WHITESPACE	= True
isWS _		= False

isID :: Symbol -> Bool
isID (ID _)	= True
isID _		= False

isINT :: Symbol -> Bool
isINT (INT _)	= True
isINT _		= False

isREAL :: Symbol -> Bool
isREAL (REAL _)    = True
isREAL (BIGREAL _) = True
isREAL _           = False

isNum :: Symbol -> Bool
isNum s = isINT s || isREAL s

isRELOP :: Symbol -> Bool
isRELOP (RELOP _) = True
isRELOP _         = False

isADDOP :: Symbol -> Bool
isADDOP (ADDOP _) = True
isADDOP _         = False

isMULOP :: Symbol -> Bool
isMULOP (MULOP _) = True
isMULOP _         = False

isSIGN :: Symbol -> Bool
isSIGN (ADDOP "+") = True
isSIGN (ADDOP "-") = True
isSIGN _           = False

data LexErrType	= UNREC
		| LONGINT
		| LONGWHOLE
		| LONGFRAC
		| LONGEXP
		| LONGID

instance Show LexErrType where
	show UNREC	= "Unrecognized Symbol"
	show LONGINT	= "Extra Long Integer"
	show LONGWHOLE	= "Extra Long Whole Part"
	show LONGFRAC	= "Extra Long Fractional Part"
	show LONGEXP	= "Extra Long Exponent"
	show LONGID	= "Extra Long Identifier"
