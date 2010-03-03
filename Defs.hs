{- Defs.hs
 - Define generic, compiler-related data structures
 -}

module Defs ( Line( Line, NoLine ), Token( Token ), sym, line )
	where

import Symbol (Symbol)

-- A Line has a line number and text. NoLine is a handy fill-in for EOF tokens.
data Line	= Line Int String
		| NoLine
	deriving (Show, Eq)

instance Ord Line where
	compare (Line a _) (Line b _) = compare a b
	compare NoLine NoLine = EQ
	compare _ NoLine = GT
	compare NoLine _ = LT

-- Token is a symbol and its containing line
-- Syntax errors contain a list of valid symbols (for reporting) and a list of
--	rejected tokens (swallowed up in panic mode).
data Token	= Token {line :: Line, sym :: Symbol}

-- At the moment, it seems most interesting to know the line number and symbol
instance Show Token where
	show (Token (Line n l) s) = "Line " ++ show n ++ ": " ++ show s
	show (Token NoLine s) = show s
