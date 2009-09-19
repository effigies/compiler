{- Defs.hs
 - Compiler-specific type definitions
 -}

module Defs ( Line( Line, NoLine ),
	      Token( Token, SYNTAXERR ), line, sym,
	      State( State ), tape, table,
	      Symbol( WHITESPACE, ASSIGNOP, DELIM, RELOP, MULOP, ADDOP, NAME,
			ID, REF, RES, BIGREAL, REAL, INT, LEXERR, DOT, EOF,
			NUM, SIGN),
	      LexErrType( UNREC, LONGINT, LONGWHOLE, LONGFRAC, LONGEXP,
			LONGID),
	      {-isWS, isID, isINT, isREAL, isNum, isRELOP, isADDOP, isMULOP, isSIGN-})
	where

import Tape

-- Line is a string with an associated number
data Line	= Line Int String
		| NoLine
	deriving (Show, Eq)

-- Token is a symbol and its containing line
data Token	= Token {line :: Line, sym :: Symbol}
		| SYNTAXERR [Symbol] [Token]
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
		|  NUM				-- Exists entirely to show
		|  SIGN				-- Same

		-- Lexical errors require we retain both the token and
		|  LEXERR		LexErrType String

instance Show Symbol where
	show WHITESPACE		= "WHITESPACE"
	show ASSIGNOP		= "':='"
	show (DELIM s)		= "'" ++ s ++ "'"
	show (RELOP "_")	= "a relational operator ('>', '<', '=', '<=', '>=', '<>')"
	show (RELOP s)		= "'" ++ s ++ "'"
	show (MULOP "_")	= "a multiplicative operator ('*', '/', 'div', 'mod', 'and')"
	show (MULOP s)		= "'" ++ s ++ "'"
	show (ADDOP "_")	= "an addition operator ('+', '-', 'or')"
	show (ADDOP s)		= "'" ++ s ++ "'"
	show (NAME s)		= "'" ++ s ++ "'"
	show (ID "_")		= "an identifier"
	show (ID s)		= "'" ++ s ++ "'"
	show (REF i)		= "symbol table entry " ++ show i
	show (RES s)		= "'" ++ s ++ "'"
	show (BIGREAL s)	= "'" ++ s ++ "'"
	show (REAL s)		= "'" ++ s ++ "'"
	show (INT s)		= "'" ++ s ++ "'"
	show DOT		= "'.'"
	show EOF		= "EOF"
	show NUM		= "a number"
	show SIGN		= "a sign ('+', '-')"
	show (LEXERR t s)	= "Lexical Error (" ++ show t ++ "): " ++ show s
--	show _			= "Unimplemented show? Sorry."

instance Eq Symbol where
	{- Some of these make sense to have wildcards -}
	RELOP  _	== RELOP "_"	= True
	MULOP  _	== MULOP "_"	= True
	ADDOP  _	== ADDOP "_"	= True
	ADDOP "+"	== SIGN		= True
	ADDOP "-"	== SIGN		= True
	ID  _		== ID "_"	= True
	REF _		== ID "_"	= True
	BIGREAL _	== NUM		= True
	REAL _		== NUM		= True
	INT _		== NUM		= True
	BIGREAL _	== BIGREAL "_"	= True
	REAL _		== REAL "_"	= True
	INT _		== INT "_"	= True
	{- And reverse it... -}
	RELOP "_"	== RELOP  _	= True
	MULOP "_"	== MULOP  _	= True
	ADDOP "_"	== ADDOP  _	= True
	SIGN		== ADDOP "+"	= True
	SIGN		== ADDOP "-"	= True
	ID "_"		== ID  _	= True
	ID "_"		== REF _	= True
	NUM		== BIGREAL _	= True
	NUM		== REAL _	= True
	NUM		== INT _	= True
	BIGREAL _	== BIGREAL "_"	= True
	REAL "_"	== REAL _	= True
	INT "_"		== INT _	= True
	{- Now explicit equality -}
	WHITESPACE	== WHITESPACE	= True
	ASSIGNOP	== ASSIGNOP	= True
	EOF		== EOF		= True
	DOT		== DOT		= True
	RELOP  a	== RELOP  b	= a == b
	MULOP  a	== MULOP  b	= a == b
	ADDOP  a	== ADDOP  b	= a == b
	NAME a		== NAME b	= a == b
	ID a		== ID b		= a == b
	RES a		== RES b	= a == b
	REF a		== REF b	= a == b
	BIGREAL a	== BIGREAL b	= a == b
	REAL a		== REAL b	= a == b
	INT a		== INT b	= a == b
	DELIM a		== DELIM b	= a == b
	{- And everything else fails -}
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
