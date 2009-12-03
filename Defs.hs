{- Defs.hs
 - Define generic, compiler-related data structures
 -}

module Defs ( Line( Line, NoLine ),
		Token( Token ), line, sym,
		)
	where

import Symbol (Symbol)

-- A Line has a line number and text. NoLine is a handy fill-in for EOF tokens.
data Line	= Line Int String
		| NoLine
	deriving (Show, Eq)

-- Token is a symbol and its containing line
-- Syntax errors contain a list of valid symbols (for reporting) and a list of
--	rejected tokens (swallowed up in panic mode).
data Token	= Token {line :: Line, sym :: Symbol}

-- At the moment, it seems most interesting to know the line number and symbol
instance Show Token where
	show (Token (Line n l) s) = "Line " ++ show n ++ ": " ++ show s
	show (Token NoLine s) = show s
