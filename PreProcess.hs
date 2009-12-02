{- PreProcess.hs
 - Lexical analyzer - error checking component
 -
 - We assume a sequence of tokens, and check for lexical errors,
 - along with other preprocessing functions.
 -}

module PreProcess (fixup) where

import Defs

import Token
import Tape

-- Constraints
maxIDLen, maxIntLen, maxWholeLen, maxFracLen, maxExpLen	:: Int
maxIDLen	= 10
maxIntLen	= 10
maxWholeLen	= 5
maxFracLen	= 5
maxExpLen	= 2

fixup :: Token -> [Token]
fixup t = case (sym t) of
	WHITESPACE	-> []
	INT i		-> [t { sym = intCheck	i }]
	REAL r		-> [t { sym = realCheck	r }]
	BIGREAL b	-> [t { sym = bigRealCheck	b }]
	_		-> [t]

intCheck :: String -> Symbol
intCheck int	| length int <= maxIntLen	= INT int
				| otherwise					= LEXERR LONGINT (INT int)

realCheck :: String -> Symbol
realCheck real = let (whole, dot:frac) = span (/= '.') real in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE (REAL real)
		else if length frac > maxFracLen then
			LEXERR LONGFRAC (REAL real)
		else REAL real

bigRealCheck :: String -> Symbol
bigRealCheck bigReal = let (real, e:exp)     = break (`elem` "eE") bigReal
			   (whole, frac) = span (/= '.') real
			   exp' = if head exp `elem` "+-"
				  then tail exp
				  else exp
		in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE (BIGREAL bigReal)
		else if length frac > (maxFracLen + 1) then
			LEXERR LONGFRAC (BIGREAL bigReal)
		else if length exp' > maxExpLen then
			LEXERR LONGEXP (BIGREAL bigReal)
		else BIGREAL bigReal
