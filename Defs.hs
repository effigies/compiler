{- Defs.hs
 - Define compiler-related data structures
 -}

module Defs ( Line( Line, NoLine ),
				Token( Token, SYNTAXERR ), line, sym, valid, skip,
				Symbol( WHITESPACE, ASSIGNOP, DELIM, RELOP, MULOP, ADDOP, NAME,
				ID, REF, RES, BIGREAL, REAL, INT, LEXERR, DOT, EOF, NUM, SIGN,
				NONSENSE),
				LexErrType( UNREC, LONGINT, LONGWHOLE, LONGFRAC, LONGEXP,
				LONGID),
				isID, isLexErr,
				Type (INT_t, REAL_t, ARRAY_t, NULL_t), len, baseType,
				NameSpace (GLOBAL, NS), visible,
				State (State), table, ctype, scope,
				Compute (Compute),
				getTable, getType, getScope, putTable, putType, putScope,
				pushScope, popScope, setType,
				typeof
				)
	where

-- A Line has a line number and text. NoLine is a handy fill-in for EOF tokens.
data Line	= Line Int String
			| NoLine
	deriving (Show, Eq)

-- Symbol is a (TOKEN, LEXEME) pair
-- The lexical analyzer will take a source string
-- and parse it into a list of these symbols

--   SYMBOL	= (TOKEN	,	LEXEME) 				-- Will match:
data Symbol	=  WHITESPACE							-- Spaces, tabs, newlines
			|  ASSIGNOP								-- :=
			|  DELIM		String					-- [ ] ( ) , ;
			|  RELOP		String					-- = <> < > <= >=
			|  MULOP		String					-- * / div mod and
			|  ADDOP		String					-- + - or
			|  NAME			String					-- IDs or reserved words
			|  ID			String NameSpace Type	-- ID
			|  REF			Int						-- Symbol table reference
			|  RES			String					-- Reserved word
			|  BIGREAL		String					-- "Big" reals (find out)
			|  REAL			String					-- Reals
			|  INT			String					-- Integers
			|  DOT									-- .
			|  EOF
			|  VAR									-- Wildcard
			|  NUM									-- Wildcard
			|  SIGN									-- Wildcard
			|  NULL

			-- Lexical errors need a type along with a symbol
			|  LEXERR		LexErrType Symbol
			|  NONSENSE		String					-- Unrecognized strings

{- This is intended to make debug or error output more friendly to read. -}
instance Show Symbol where
	show WHITESPACE				= "whitespace"
	show ASSIGNOP				= "':='"
	show (DELIM s)				= "'" ++ s ++ "'"
	show (RELOP "_")			= "a relational operator ('>', '<', '=', '<=', '>=', '<>')"
	show (RELOP s)				= "'" ++ s ++ "'"
	show (MULOP "_")			= "a multiplicative operator ('*', '/', 'div', 'mod', 'and')"
	show (MULOP s)				= "'" ++ s ++ "'"
	show (ADDOP "_")			= "an addition operator ('+', '-', 'or')"
	show (ADDOP s)				= "'" ++ s ++ "'"
	show (NAME s)				= "'" ++ s ++ "'"
	show (ID s ns t)			= show ns ++ "." ++ s ++ "::" ++ show t
	show (REF i)				= "symbol table entry " ++ show i
	show (RES s)				= "'" ++ s ++ "'"
	show (BIGREAL s)			= "'" ++ s ++ "'"
	show (REAL s)				= "'" ++ s ++ "'"
	show (INT s)				= "'" ++ s ++ "'"
	show DOT					= "'.'"
	show EOF					= "EOF"
	show NUM					= "a number"
	show VAR					= "an identifier"
	show SIGN					= "a sign ('+', '-')"
	show (LEXERR t s)			= "Lexical Error (" ++ show t ++ "): " ++ (show s)
	show (NONSENSE s)			= "'" ++ s ++ "'"
	show NULL					= "NULL (YOU SHOULDN'T EVER SEE THIS)"

instance Eq Symbol where
	{- Some of these make sense to have wildcards -}
	RELOP  _		== RELOP "_"	= True
	MULOP  _		== MULOP "_"	= True
	ADDOP  _		== ADDOP "_"	= True
	ADDOP "+"		== SIGN			= True
	ADDOP "-"		== SIGN			= True
	ID  _ _ _		== VAR			= True
	REF _			== VAR			= True
	BIGREAL _		== NUM			= True
	REAL _			== NUM			= True
	INT _			== NUM			= True
	BIGREAL _		== BIGREAL "_"	= True
	REAL _			== REAL "_"		= True
	INT _			== INT "_"		= True
	{- And reverse it... This may not be necessary,
	   but completeness compells me -}
	RELOP "_"		== RELOP  _		= True
	MULOP "_"		== MULOP  _		= True
	ADDOP "_"		== ADDOP  _		= True
	SIGN			== ADDOP "+"	= True
	SIGN			== ADDOP "-"	= True
	VAR				== ID _ _ _		= True
	VAR				== REF _		= True
	NUM				== BIGREAL _	= True
	NUM				== REAL _		= True
	NUM				== INT _		= True
	BIGREAL "_"		== BIGREAL _	= True
	REAL "_"		== REAL _		= True
	INT "_"			== INT _		= True
	{- By matching lexical errors to their embedded symbols, we allow our grammar
	   to skip over syntactically valid, but lexically invalid, tokens. -}
	LEXERR _ sym	== VAR			= sym == VAR
	LEXERR _ sym	== NUM			= sym == NUM
	VAR				== LEXERR _ sym	= sym == VAR
	NUM				== LEXERR _ sym = sym == NUM
	{- More standard notions of equality follow -}
	WHITESPACE		== WHITESPACE	= True
	ASSIGNOP		== ASSIGNOP		= True
	EOF				== EOF			= True
	DOT				== DOT			= True
	RELOP a			== RELOP b		= a == b
	MULOP a			== MULOP b		= a == b
	ADDOP a			== ADDOP b		= a == b
	NAME a			== NAME b		= a == b
	RES a			== RES b		= a == b
	REF a			== REF b		= a == b
	BIGREAL a		== BIGREAL b	= a == b
	REAL a			== REAL b		= a == b
	INT a			== INT b		= a == b
	DELIM a			== DELIM b		= a == b
	ID n s t		== ID n' s' t'	= n == n'
								   && s == s'
								   && t == t'
	LEXERR _ a		== LEXERR _ b	= a == b
	{- And everything else fails -}
	_					== _			= False

{- These are the varieties of lexical errors we can encounter -}
data LexErrType	= UNREC
				| LONGINT
				| LONGWHOLE
				| LONGFRAC
				| LONGEXP
				| LONGID

{- The above are fine to type. Less fun to read. -}
instance Show LexErrType where
	show UNREC		= "Unrecognized Symbol"
	show LONGINT	= "Extra Long Integer"
	show LONGWHOLE	= "Extra Long Whole Part"
	show LONGFRAC	= "Extra Long Fractional Part"
	show LONGEXP	= "Extra Long Exponent"
	show LONGID		= "Extra Long Identifier"

isID :: Symbol -> Bool
isID (ID _ _ _)	= True
isID _		= False

isLexErr :: Symbol -> Bool
isLexErr (LEXERR _ _)	= True
isLexErr _				= False


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

{- These are the data types understood by our language -}
data Type	= INT_t
			| REAL_t
			| ARRAY_t { len :: Int, baseType :: Type }
			| NULL_t
	deriving Eq

instance Show Type where
	show INT_t = "integer type"
	show REAL_t = "real type"
	show (ARRAY_t n t) = "array[" ++ show n ++ "] of " ++ show t
	show NULL_t = "no type"

{- Each variable has a scope, and we need to check that we are in its
   scope when we call it. -}
data NameSpace = GLOBAL | NS NameSpace String
	deriving Eq

instance Show NameSpace where
	show GLOBAL = "GLOBAL"
	show (NS parent name) = show parent ++ "." ++ name

{- visible scope variable
 -
 - test whether a variable is visible from the local scope
 -} 
visible :: NameSpace -> NameSpace -> Bool
visible _ GLOBAL = True
visible GLOBAL _ = False
visible local@(NS parent name) test = local == test || visible parent test

{- For aspects of compiler that can't be resolved purely by PDA, we
 - need some sense of state. The symbol table, the current type context,
 - and the local scope should cover us.
 -}
data State =	State {
						table :: [Symbol],
						ctype :: Type,
						scope :: NameSpace
				}
	deriving (Show)

{- Let's build a state monad to hide our state
 - I may be going to Haskell hell for using a different name, but I *really*
 - dislike the naming of the State monad. The monad itself is not a state, but
 - a function from a state to a (potentially) new state and result.
 - 
 - I choose to call it Compute because 1) I already have something that is
 - reasonably named state; 2) computation better describes what it's hiding.
 -}
newtype Compute a = Compute (State -> (State,a))

-- Standard State Monad definition
instance Monad (Compute) where
	return token = Compute (\st -> (st, token))
        Compute typestate >>= func =
                Compute $ \state0 ->
                        let (state1, tok) = typestate state0
                            Compute typestate' = func tok
                in typestate' state1

{-	Compute comp >>= production =
						Compute $ \state0 ->
								let (state1, tokens) = comp state0
									Compute comp' = production tokens
								in comp' state1
-}
{- Get and put methods for each field in the State -}
getTable :: Compute [Symbol]
getTable = Compute $ \state -> (state, table state)

getType :: Compute Type
getType = Compute $ \state -> (state, ctype state)

getScope :: Compute NameSpace
getScope = Compute $ \state -> (state, scope state)

putTable :: [Symbol] -> Compute ()
putTable tab = Compute $ \state -> (state { table = tab }, ())

putType :: Type -> Compute ()
putType tp = Compute $ \state -> (state { ctype = tp }, ())

putScope :: NameSpace -> Compute ()
putScope ns = Compute $ \state -> (state { scope = ns }, ())

{- Slightly more complex, and rather more useful for actual calling -}
pushScope :: String -> Compute ()
pushScope name = do
                ns <- getScope
                putScope (NS ns name)

popScope :: Compute ()
popScope = do
			ns <- getScope
			case ns of
				GLOBAL -> return ()
				NS base top -> putScope base

setType :: Symbol -> Compute ()
setType tok = do
				ctype <- typeof tok
				putType ctype

{- Put it all together, and we can now compute the type of any known object -}
typeof :: Symbol -> Compute Type
typeof (ID _ _ t) = return t
typeof (BIGREAL _) = return REAL_t
typeof (REAL _) = return REAL_t
typeof (INT _) = return INT_t
typeof (LEXERR _ s) = typeof s
typeof (REF n) = do
					tab <- getTable
					typeof (tab !! n)
typeof _ = return NULL_t
