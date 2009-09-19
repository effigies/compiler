{- PreProcess.hs
 - Lexical analyzer - error checking component
 -
 - We assume a sequence of tokens, and check for lexical errors,
 - along with other preprocessing functions.
 -}

module PreProcess (preprocess) where

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

{- preprocess
 -
 - Given a string of tokens, perform the following actions
 -
 - 1) Ignore whitespace; whitespace is never an error in Pascal
 - 2) Distinguish reserved words and identifiers
 - 3) Validate numerical constants
 -}
preprocess :: [String] -> [Symbol] -> [Symbol]
preprocess _ [] = []
preprocess rs (s:ss) = let next = preprocess rs ss in
			case s of
				WHITESPACE	-> 		    next
				NAME	n	-> nameCheck rs n : next
				INT	i	-> intCheck	i : next
				REAL	r	-> realCheck	r : next
				BIGREAL	b	-> bigRealCheck	b : next
				_		->		s : next

nameCheck :: [String] -> String -> Symbol
nameCheck rs name | length name > maxIDLen	= LEXERR LONGID name
		  | name `elem` rs		= RES name
		  | otherwise			= ID name


intCheck int   | length int <= maxIntLen	= INT int
	       | otherwise			= LEXERR LONGINT int

realCheck real = let (whole, dot:frac) = span (/= '.') real in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE real
		else if length frac > maxFracLen then
			LEXERR LONGFRAC real
		else REAL real

bigRealCheck :: String -> Symbol
bigRealCheck bigReal = let (real, e:exp)     = break (`elem` "eE") bigReal
			   (whole, frac) = span (/= '.') real
			   exp' = if head exp `elem` "+-"
				  then tail exp
				  else exp
		in
		if length whole > maxWholeLen then
			LEXERR LONGWHOLE bigReal
		else if length frac > (maxFracLen + 1) then
			LEXERR LONGFRAC bigReal
		else if length exp' > maxExpLen then
			LEXERR LONGEXP bigReal
		else BIGREAL bigReal