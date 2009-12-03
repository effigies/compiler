{- Symbol.hs
 - Define symbol types
 -}

module Symbol ( Symbol( WHITESPACE, ASSIGNOP, DELIM, RELOP, MULOP, ADDOP,
		ID, REF, RES, BIGREAL, REAL, INT, LEXERR, DOT, EOF, NUM, SIGN,
		NONSENSE, SYNTAXERR),
		LexErrType( UNREC, LONGINT, LONGWHOLE, LONGFRAC, LONGEXP,
		LONGID),
		isID, isLexErr, isSyntaxErr
		)
	where

import Type (Type)
import NameSpace (NameSpace)

-- Symbol is a (TOKEN, LEXEME) pair
-- The lexical analyzer will take a source string
-- and parse it into a list of these symbols

--   SYMBOL	= (TOKEN,	LEXEME) 		-- Will match:
data Symbol	=  WHITESPACE				-- Spaces, tabs, newlines
		|  ASSIGNOP				-- :=
		|  DELIM	String			-- [ ] ( ) , ;
		|  RELOP	String			-- = <> < > <= >=
		|  MULOP	String			-- * / div mod and
		|  ADDOP	String			-- + - or
		|  ID		String NameSpace Type	-- ID
		|  REF		Int			-- Symbol table reference
		|  RES		String			-- Reserved word
		|  BIGREAL	String			-- "Big" reals (find out)
		|  REAL		String			-- Reals
		|  INT		String			-- Integers
		|  DOT					-- .
		|  EOF
		|  VAR					-- Wildcard
		|  NUM					-- Wildcard
		|  SIGN					-- Wildcard
		|  NULL

		-- Lexical errors need a type along with a symbol
		|  LEXERR	LexErrType	Symbol
		|  NONSENSE	String			-- Unrecognized strings
		|  SYNTAXERR	[Symbol]	Symbol

{- This is intended to make debug or error output more friendly to read. -}
instance Show Symbol where
	show WHITESPACE		= "whitespace"
	show ASSIGNOP		= "':='"
	show (DELIM s)		= "'" ++ s ++ "'"
	show (RELOP "_")	= "a relational operator ('>', '<', '=', '<=', '>=', '<>')"
	show (RELOP s)		= "'" ++ s ++ "'"
	show (MULOP "_")	= "a multiplicative operator ('*', '/', 'div', 'mod', 'and')"
	show (MULOP s)		= "'" ++ s ++ "'"
	show (ADDOP "_")	= "an addition operator ('+', '-', 'or')"
	show (ADDOP s)		= "'" ++ s ++ "'"
	show (ID s ns t)	= show ns ++ "." ++ s ++ "::" ++ show t
	show (REF i)		= "symbol table entry " ++ show i
	show (RES s)		= "'" ++ s ++ "'"
	show (BIGREAL s)	= "'" ++ s ++ "'"
	show (REAL s)		= "'" ++ s ++ "'"
	show (INT s)		= "'" ++ s ++ "'"
	show DOT		= "'.'"
	show EOF		= "EOF"
	show NUM		= "a number"
	show VAR		= "an identifier"
	show SIGN		= "a sign ('+', '-')"
	show (LEXERR t s)	= "Lexical Error (" ++ show t ++ "): " ++ show s
	show (NONSENSE s)	= "'" ++ s ++ "'"
	show NULL		= "NULL (YOU SHOULDN'T EVER SEE THIS)"

instance Eq Symbol where
	{- Some of these make sense to have wildcards -}
	RELOP  _	== RELOP "_"	= True
	MULOP  _	== MULOP "_"	= True
	ADDOP  _	== ADDOP "_"	= True
	ADDOP "+"	== SIGN		= True
	ADDOP "-"	== SIGN		= True
	ID  _ _ _	== VAR		= True
	REF _		== VAR		= True
	BIGREAL _	== NUM		= True
	REAL _		== NUM		= True
	INT _		== NUM		= True
	BIGREAL _	== BIGREAL "_"	= True
	REAL _		== REAL "_"	= True
	INT _		== INT "_"	= True
	{- And reverse it... This may not be necessary,
	   but completeness compells me -}
	RELOP "_"	== RELOP  _	= True
	MULOP "_"	== MULOP  _	= True
	ADDOP "_"	== ADDOP  _	= True
	SIGN		== ADDOP "+"	= True
	SIGN		== ADDOP "-"	= True
	VAR		== ID _ _ _	= True
	VAR		== REF _	= True
	NUM		== BIGREAL _	= True
	NUM		== REAL _	= True
	NUM		== INT _	= True
	BIGREAL "_"	== BIGREAL _	= True
	REAL "_"	== REAL _	= True
	INT "_"		== INT _	= True
	{- By matching lexical errors to their embedded symbols, we allow our grammar
	   to skip over syntactically valid, but lexically invalid, tokens. -}
	LEXERR _ sym	== VAR		= sym == VAR
	LEXERR _ sym	== NUM		= sym == NUM
	VAR		== LEXERR _ sym	= sym == VAR
	NUM		== LEXERR _ sym = sym == NUM
	{- More standard notions of equality follow -}
	WHITESPACE	== WHITESPACE	= True
	ASSIGNOP	== ASSIGNOP	= True
	EOF		== EOF		= True
	DOT		== DOT		= True
	RELOP a		== RELOP b	= a == b
	MULOP a		== MULOP b	= a == b
	ADDOP a		== ADDOP b	= a == b
	RES a		== RES b	= a == b
	REF a		== REF b	= a == b
	BIGREAL a	== BIGREAL b	= a == b
	REAL a		== REAL b	= a == b
	INT a		== INT b	= a == b
	DELIM a		== DELIM b	= a == b
	ID n s t	== ID n' s' t'	= n == n'
				       && s == s'
				       && t == t'
	LEXERR _ a	== LEXERR _ b	= a == b

	{- I really only care that a syntax error be a syntax error.
	 - Specifics don't interest me. -}
	SYNTAXERR _ _	== SYNTAXERR _ _ = True
	{- And everything else fails -}
	_		== _		= False

{- These are the varieties of lexical errors we can encounter -}
data LexErrType	= UNREC
		| LONGINT
		| LONGWHOLE
		| LONGFRAC
		| LONGEXP
		| LONGID

{- The above are fine to type. Less fun to read. -}
instance Show LexErrType where
	show UNREC	= "Unrecognized Symbol"
	show LONGINT	= "Extra Long Integer"
	show LONGWHOLE	= "Extra Long Whole Part"
	show LONGFRAC	= "Extra Long Fractional Part"
	show LONGEXP	= "Extra Long Exponent"
	show LONGID	= "Extra Long Identifier"

isID :: Symbol -> Bool
isID (ID _ _ _)	= True
isID _		= False

isLexErr :: Symbol -> Bool
isLexErr (LEXERR _ _)	= True
isLexErr _		= False

isSyntaxErr :: Symbol -> Bool
isSyntaxErr (SYNTAXERR _ _)	= True
isSyntaxErr _			= False
