{- Grammar.hs
 -
 - LL(1) Grammar
 -}

module Grammar ( program ) where

{- We need to know what about Symbols -}
import Defs
import Test
import Data.List ( nub )

{- match and matchSym can be thought of as templates, which, when given an
 - argument, become productions which match a single terminal
 -}
match :: (Symbol -> Bool) -> [Token] -> [Token]
match b [] = error "Not enough tokens..."
match b (t:ts) = if b (sym t) then ts else error ("Failed match " ++ show (t:ts))

matchErr :: String -> Symbol -> Line -> a
matchErr e EOF _ = error ("Expected " ++ (show e) ++ "; received EOF.")
matchErr e g l = error ("Expected " ++ (show e) ++ "; received " ++ (show g) ++ ".\n" ++ (show l))

{- matchSym - match exact symbols
 - Not for use with generic symbols (like IDs or literals)
 -}
matchSym :: Symbol -> [Token] -> [Token]
matchSym s [] = matchErr (show s) EOF NoLine
matchSym s (t:ts) | sym t == s	= ts
		  | otherwise	= matchErr (show s) (sym t) (line t)

matchID :: [Token] -> [Token]
matchID [] = matchErr "identifier" EOF NoLine
matchID ((Token _ (REF _)):ts) = ts
matchID ((Token _ (ID _)):ts) = ts -- Only useful if we don't tabulate
matchID (t:ts) = matchErr "identifier" (sym t) (line t)

matchInt [] = matchErr "integer" EOF NoLine
matchInt ((Token _ (INT _)):ts) = ts
matchInt (t:ts) = matchErr "integer" (sym t) (line t)

eof :: [Token] -> [Token]
eof [] = []
eof ts = error ("Expected EOF, received: " ++ remainder ts)
	where
		remainder :: [Token] -> String
		remainder ts = unlines (map show (nub (map line ts)))

--remainder' :: Line -> String
--remainder' (Line _ s) = s

{- Here is our special-made sequencer, which allows us to write our productions
 - naturally (first -> last) and with far fewer parentheses than normal
 - function application would require. Compare the following:
 -
 - production e = e $> prod1 $> prod2 $> prod3
 - production e = prod3 (prod2 (prod1 e))
 -}
($>) = flip ($)

{- I feel the e convention for the list of tokens may be a bit unintuitive, so
 - I'll explain. I use this because the full list of tokens is identical to the
 - output of matching a null, or epsilon, production. Thus, we behave as if
 - each production first matches a null symbol, and in the case of actual null
 - productions, we follow the grammar. -}

{-
 - 1.1.1.1.1.1	program → program id ( identifier_list ) ; program'
 -}

program :: [Token] -> [Token]
program e	=  e
		$> matchSym (RES "program")
		$> matchID
		$> matchSym (DELIM "(")
		$> identifier_list
		$> matchSym (DELIM ")")
		$> matchSym (DELIM ";")
		$> program'

{-
 - 1.1.1.1.2.1	program' → declarations program''
 - 1.1.1.1.2.2	program' → program''
 -}
program' :: [Token] -> [Token]
program' e@(t:_) | sym t == RES "var"	=  e
					$> declarations
					$> program''
		 | otherwise		=  e
					$> program''

{-
 - 1.1.1.1.3.1	program'' → subprogram_declarations program'''
 - 1.1.1.1.3.2	program'' → program'''
 -}
program'' :: [Token] -> [Token]
program'' e@(t:_) | sym t == RES "function"	=  e
					$> subprogram_declarations
					$> program'''
		   | otherwise		=  e
					$> program'''

{-
 - 1.1.1.1.4.1	program''' → compound_statement .
 -}
program''' :: [Token] -> [Token]
program''' e	=  e
		$> compound_statement
		$> matchSym DOT
		$> eof

{-
 - 2.1.1.1.1.1	identifier_list → id identifier_list'
 -}
identifier_list :: [Token] -> [Token]
identifier_list e	=  e
			$> matchID
			$> identifier_list'

{-
 - 2.1.2.1.1.1	identifier_list' → , identifier_list
 - 2.1.2.2.1.1	identifier_list' → ε
 -}
identifier_list' :: [Token] -> [Token]
identifier_list' e@(t:ts) | sym t == DELIM ","	=  ts
						$> identifier_list
			  | otherwise		=  e

{-
 - 3.1.1.1.1.1	declarations → var id : type ; declarations'
 -}
declarations :: [Token] -> [Token]
declarations e	=  e
		$> matchSym (RES "var")
		$> matchID
		$> matchSym (DELIM ":")
		$> type_		-- Since type is a Haskell keyword
		$> matchSym (DELIM ";")
		$> declarations'

{-
 - 3.1.2.1.1.1	declarations' → declarations
 - 3.1.2.2.1.1	declarations' → ε
 -}
declarations' :: [Token] -> [Token]
declarations' e@(t:_) | sym t == RES "var"	=  e
					$> declarations
		      | otherwise	=  e

{-
 - 4.2.1.1.1.1	type → array [ num .. num ] of standard_type
 - 4.1.1.1.1.1	type → standard_type
 -}
type_ :: [Token] -> [Token]
type_ e@(t:ts) | sym t == RES "array"	=  ts
					$> matchSym (DELIM "[")
					$> matchInt
					$> matchSym (DELIM "..")
					$> matchInt
					$> matchSym (DELIM "]")
					$> matchSym (RES "of")
					$> standard_type
	      | otherwise		=  e
					$> standard_type

{-
 - 5.1.1.1.1.1	standard_type → integer
 - 5.2.1.1.1.1	standard_type → real
 -}
standard_type :: [Token] -> [Token]
standard_type (t:ts)	| sym t == RES "integer"	= ts
			| sym t == RES "real"	= ts

{-
 - 6.1.1.1.1.1	subprogram_declarations → subprogram_declaration ; subprogram_declarations'
 -}
subprogram_declarations :: [Token] -> [Token]
subprogram_declarations e	=  e
				$> subprogram_declaration
				$> matchSym (DELIM ";")
				$> subprogram_declarations'

{-
 - 6.1.2.1.1.1	subprogram_declarations' → subprogram_declarations
 - 6.1.2.2.1.1	subprogram_declarations' → ε
 -}
subprogram_declarations' :: [Token] -> [Token]
subprogram_declarations' e@(t:_) | sym t == RES "function"	=  e
							$> subprogram_declarations
				 | otherwise		=  e

{-
 - 7.1.1.1.1.1	subprogram_declaration → subprogram_head subprogram_declaration'
 -}
subprogram_declaration :: [Token] -> [Token]
subprogram_declaration e	=  e
				$> subprogram_head
				$> subprogram_declaration'

{-
 - 7.1.1.1.2.1	subprogram_declaration' → declarations subprogram_declaration''
 - 7.1.1.1.2.2	subprogram_declaration' → subprogram_declaration''
 -}
subprogram_declaration' :: [Token] -> [Token]
subprogram_declaration' e@(t:_)	| sym t == RES "var"	=  e
							$> declarations
							$> subprogram_declaration''
				| otherwise		=  e
							$> subprogram_declaration''

{-
 - 7.1.1.1.3.1	subprogram_declaration'' → subprogram_declarations compound_statement
 - 7.1.1.1.1.2	subprogram_declaration'' → compound_statement
 -}
subprogram_declaration'' :: [Token] -> [Token]
subprogram_declaration'' e@(t:_) | sym t == RES "function"	=  e
							$> subprogram_declarations
							$> compound_statement
				 | otherwise		=  e
							$> compound_statement

{-
 - 8.1.1.1.1.1	subprogram_head → function id subprogram_head'
 -}
subprogram_head :: [Token] -> [Token]
subprogram_head e	=  e
			$> matchSym (RES "function")
			$> matchID
			$> subprogram_head'

{-
 - 8.1.1.1.2.1	subprogram_head' → ( parameter_list ) subprogram_head'''
 - 8.1.1.1.2.2	subprogram_head' → subprogram_head'''
 -}
subprogram_head' :: [Token] -> [Token]
subprogram_head' e@(t:ts) | sym t == DELIM "("	=  ts
						$> parameter_list
						$> matchSym (DELIM ")")
						$> subprogram_head''
			  | otherwise		=  e
						$> subprogram_head''

{-
 - 8.1.1.1.3.1	subprogram_head'' → : standard_type ;
 -}
subprogram_head'' :: [Token] -> [Token]
subprogram_head'' e	=  e
			$> matchSym (DELIM ":")
			$> standard_type
			$> matchSym (DELIM ";")

{-
 - 10.1.1.1.1.1	parameter_list → id : type parameter_list'
 -}
parameter_list :: [Token] -> [Token]
parameter_list e	=  e
			$> matchID
			$> matchSym (DELIM ":")
			$> type_
			$> parameter_list'

{-
 - 10.1.2.1.1.1	parameter_list' → ; parameter_list
 - 10.1.2.2.1.1	parameter_list' → ε
 -}
parameter_list' :: [Token] -> [Token]
parameter_list' e@(t:ts) | sym t == DELIM ";"	=  ts
						$> parameter_list
			 | otherwise		=  e

{-
 - 11.1.1.1.1.1	compound_statement → begin compound_statement'
 -}
compound_statement :: [Token] -> [Token]
compound_statement e	=  e
			$> matchSym (RES "begin")
			$> compound_statement'

{-
 - 11.1.1.1.2.1	compound_statement' → statement_list end
 - 11.1.1.1.2.2	compound_statement' → end
 -}
compound_statement' :: [Token] -> [Token]
compound_statement' e@(t:ts) | sym t == RES "end"   =  ts
			     | otherwise	=  e
						$> statement_list
						$> matchSym (RES "end")

{-
 - 13.1.1.1.1.1	statement_list → statement statement_list'
 -}
statement_list :: [Token] -> [Token]
statement_list e	=  e
			$> statement
			$> statement_list'

{-
 - 13.1.2.1.1.1	statement_list' → ; statement_list
 - 13.1.2.2.1.1	statement_list' → ε
 -}
statement_list' :: [Token] -> [Token]
statement_list' e@(t:ts) | sym t == DELIM ";"	=  ts
						$> statement_list
			 | otherwise		=  e

{-
 - 14.1.1.1.1.1	statement → variable assignop expression
 - 14.2.1.1.1.1	statement → compound_statement
 - 14.5.1.1.1.1	statement → while expression do statement
 - 14.3.1.1.1.1	statement → if expression then statement statement'
 -}
statement :: [Token] -> [Token]
statement e@(t:ts) | sym t == RES "begin"	=  e
					$> compound_statement
		   | sym t == RES "while"	=  ts
					$> expression
					$> matchSym (RES "do")
					$> statement
		   | sym t == RES "if"	=  ts
					$> expression
					$> matchSym (RES "then")
					$> statement
					$> statement'
		   | otherwise		= e
					$> variable
					$> matchSym ASSIGNOP
					$> expression

{-
 - 14.3.1.1.2.1	statement' → else statement
 - 14.3.1.1.2.2	statement' → ε
 -}
statement' :: [Token] -> [Token]
statement' e@(t:ts) | sym t == RES "else"	=  ts
					$> statement
		    | otherwise		=  e

{-
 - 15.1.1.1.1.1	variable → id variable'
 -}
variable e	=  e
		$> matchID
		$> variable'

{-
 - 15.1.1.1.2.1	variable' → [ expression ]
 - 15.1.1.1.2.1	variable' → ε
 -}
variable' e@(t:ts) | sym t == DELIM "["	=  ts
					$> expression
					$> matchSym (DELIM "]")
		   | otherwise		=  e

{-
 - 16.1.1.1.1.1	expression_list → expression expression_list'
 -}
expression_list :: [Token] -> [Token]
expression_list e	=  e
			$> expression
			$> expression_list'

{-
 - 16.1.2.1.1.1	expression_list' → , expression_list
 - 16.1.2.2.1.1	expression_list' → ε
 -}
expression_list' :: [Token] -> [Token]
expression_list' e@(t:ts) | sym t == DELIM ","	=  ts
						$> expression_list
			  | otherwise		=  e

{-
 - 17.1.1.1.1.1	expression → simple_expression expression'
 -}
expression :: [Token] -> [Token]
expression e	=  e
		$> simple_expression
		$> expression'

{-
 - 17.1.1.1.2.1	expression' → relop simple_expression
 - 17.1.1.1.2.2	expression' → ε
 -}
expression' :: [Token] -> [Token]
expression' e@(t:ts) | isRELOP (sym t)	=  ts
					$> simple_expression
		     | otherwise	=  e

{-
 - 18.1.1.1.1.1	simple_expression → term simple_expression'
 - 18.2.1.1.1.1	simple_expression → sign term simple_expression'
 -}
simple_expression :: [Token] -> [Token]
simple_expression e@(t:ts) | isSIGN (sym t)	=  ts
					$> term
					$> simple_expression'
			   | otherwise	=  e
					$> term
					$> simple_expression'

{-
 - 18.3.2.1.1.1	simple_expression' → addop term simple_expression'
 - 18.3.2.2.1.1	simple_expression' → ε
 -}
simple_expression' :: [Token] -> [Token]
simple_expression' e@(t:ts) | isADDOP (sym t)	=  ts
					$> term
					$> simple_expression'
			    | otherwise	=  e

{-
 - 19.1.1.1.1.1	term → factor term'
 -}
term :: [Token] -> [Token]
term e	=  e
	$> factor
	$> term'

{-
 - 19.2.2.1.1.1	term' → mulop term
 - 19.2.2.2.1.1	term' → ε
 -}
term' :: [Token] -> [Token]
term' e@(t:ts)	| isMULOP (sym t)	=  ts
				$> term
		| otherwise	=  e

{-
 - 20.4.1.1.1.1	factor → num
 - 20.5.1.1.1.1	factor → not factor
 - 20.6.1.1.1.1	factor → ( expression )
 - 20.1.1.1.1.1	factor → id factor'
 -}
factor :: [Token] -> [Token]
factor e@(t:ts)	| isNum (sym t)		=  ts
		| sym t == RES "not"	=  ts
					$> factor
		| sym t == DELIM "("	=  ts
					$> expression
					$> matchSym (DELIM ")")
		| otherwise		=  e
					$> matchID
					$> factor'

{-
 - 20.1.1.1.2.1	factor' → ( expression_list )
 - 20.1.1.1.2.2	factor' → [ expression ]
 - 20.1.1.1.2.3	factor' → ε
 -}
factor' :: [Token] -> [Token]
factor' e@(t:ts) | sym t == DELIM "("	=  ts
					$> expression_list
					$> matchSym (DELIM ")")
		 | sym t == DELIM "["	=  ts
					$> expression
					$> matchSym (DELIM "]")
		 | otherwise		=  e