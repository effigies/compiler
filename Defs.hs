{- Defs.hs
 - Define compiler-related data structures
 -}

module Defs ( Line( Line, NoLine ),
	      Token( Token, SYNTAXERR ), line, sym, valid, skip, extract,
	      State( State ), tape, table,
	      Symbol( WHITESPACE, ASSIGNOP, DELIM, RELOP, MULOP, ADDOP, NAME,
		ID, REF, RES, BIGREAL, REAL, INT, LEXERR, DOT, EOF, NUM, SIGN),
	      LexErrType( UNREC, LONGINT, LONGWHOLE, LONGFRAC, LONGEXP,
		LONGID),
	      isID)
	where

import Tape

-- A Line has a line number and text. NoLine is a handy fill-in for EOF tokens.
data Line	= Line Int String
		| NoLine
	deriving (Show, Eq)

-- Token is a symbol and its containing line
-- Syntax errors contain a list of valid symbols (for reporting) and a list of
--	rejected tokens (swallowed up in panic mode).
data Token	= Token {line :: Line, sym :: Symbol}
		| SYNTAXERR {valid :: [Symbol], skip :: [Token]}

-- At the moment, it seems most interesting to know the line number and symbol
instance Show Token where
	show (Token (Line n l) s) = "Line " ++ show n ++ ": " ++ show s
	show (Token NoLine s) = show s
	show (SYNTAXERR v s) = "SYNTAXERR\nValid symbols: " ++ show v ++ "\nSkipped tokens: " ++ show s

-- State, for inability to think of a better term, is what the parser operates
--	on. We keep a tape of tokens so as not to consume them, and the symbol
--	table for when lookups are necessary.
data State = State {tape :: (Tape Token), table :: [Symbol] }

instance Show State where
	show (State t syms) = show t ++ "\n\n" ++ show syms

-- This is a very common operation in a couple files, so I'll put it here.
extract :: State -> Symbol
extract st = case focus . tape $ st of
		Token _ s	-> s
		SYNTAXERR _ _	-> NULL
--extract = sym . focus . tape

-- Symbol is a (TOKEN, LEXEME) pair
-- The lexical analyzer will take a source string
-- and parse it into a list of these symbols

--   SYMBOL	= (TOKEN	,	LEXEME) -- Will match:
data Symbol	=  WHITESPACE			-- Spaces, tabs, newlines
		|  ASSIGNOP			-- :=
		|  DELIM		String	-- [ ] ( ) , ;
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
		|  NUM				-- Wildcard
		|  SIGN				-- Same
		|  NULL

		-- Lexical errors need a type along with a lexeme
		|  LEXERR		LexErrType String

{- This is intended to make debug or error output more friendly to read. -}
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
	show (LEXERR t s)	= "Lexical Error (" ++ show t ++ "): " ++ s
	show NULL		= "NULL (YOU SHOULDN'T EVER SEE THIS)"

instance Eq Symbol where
	{- Some of these make sense to have wildcards -}
	RELOP  _		== RELOP "_"	= True
	MULOP  _		== MULOP "_"	= True
	ADDOP  _		== ADDOP "_"	= True
	ADDOP "+"		== SIGN		= True
	ADDOP "-"		== SIGN		= True
	ID  _			== ID "_"	= True
	REF _			== ID "_"	= True
	LEXERR LONGID _		== ID "_"	= True
	BIGREAL _		== NUM		= True
	REAL _			== NUM		= True
	INT _			== NUM		= True
	LEXERR LONGINT _	== NUM		= True
	LEXERR LONGWHOLE _	== NUM		= True
	LEXERR LONGFRAC _	== NUM		= True
	LEXERR LONGEXP _	== NUM		= True
	BIGREAL _		== BIGREAL "_"	= True
	REAL _			== REAL "_"	= True
	INT _			== INT "_"	= True
	{- And reverse it... This may not be necessary,
	   but completeness compells me-}
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
	BIGREAL "_"	== BIGREAL _	= True
	REAL "_"	== REAL _	= True
	INT "_"		== INT _	= True
	{- More standard notions of equality follow -}
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
isID (ID _)	= True
isID _		= False