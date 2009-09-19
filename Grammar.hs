{- Grammar.hs
 -
 - LL(1) Grammar
 -}

module Grammar ( program ) where

{- We need to know what about Symbols -}
import Defs
import Match
import Test
import Tape

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

program :: State -> State
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
program' :: State -> State
program' e | extract e == RES "var"	=  e
					$> declarations
					$> program''
	   | extract e == RES "function"
	  || extract e == RES "begin"	=  e
					$> program''
	   | otherwise			=  syntaxErr first e
	where
		first = [RES "var", RES "function", RES "begin"]

{-
 - 1.1.1.1.3.1	program'' → subprogram_declarations program'''
 - 1.1.1.1.3.2	program'' → program'''
 -}
program'' :: State -> State
program'' e | extract e == RES "function"	=  e
						$> subprogram_declarations
						$> program'''
	    | extract e == RES "begin"		=  e
						$> program'''
	    | otherwise				=  syntaxErr first e
	where
		first = [RES "function", RES "begin"]

{-
 - 1.1.1.1.4.1	program''' → compound_statement .
 -}
program''' :: State -> State
program''' e	=  e
		$> compound_statement
		$> matchSym DOT
		$> matchEOF
	

{-
 - 2.1.1.1.1.1	identifier_list → id identifier_list'
 -}
identifier_list :: State -> State
identifier_list e	=  e
			$> matchID
			$> identifier_list'

{-
 - 2.1.2.1.1.1	identifier_list' → , identifier_list
 - 2.1.2.2.1.1	identifier_list' → ε
 -}
identifier_list' :: State -> State
identifier_list' e | extract e == DELIM ","	=  e { tape = mover (tape e) }
						$> identifier_list
		   | extract e == DELIM ")"	=  e
		   | otherwise			=  syntaxErr valid e
	where
		first = [DELIM ","]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 3.1.1.1.1.1	declarations → var id : type ; declarations'
 -}
declarations :: State -> State
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
declarations' :: State -> State
declarations' e | extract e == RES "var"	=  e
						$> declarations
		| extract e `elem` follow	=  e
		| otherwise			=  syntaxErr valid e
	where
		first = [RES "var"]
		follow = [RES "function", RES "begin"]
		valid = first ++ follow

{-
 - 4.2.1.1.1.1	type → array [ num .. num ] of standard_type
 - 4.1.1.1.1.1	type → standard_type
 -}
type_ :: State -> State
type_ e | extract e == RES "array"	=  e { tape = mover (tape e) }
					$> matchSym (DELIM "[")
					$> matchInt
					$> matchSym (DELIM "..")
					$> matchInt
					$> matchSym (DELIM "]")
					$> matchSym (RES "of")
					$> standard_type
	| extract e == RES "integer"
       || extract e == RES "real"	=  e
					$> standard_type
	| otherwise			=  syntaxErr first e
	where
		first = [RES "array", RES "integer", RES "real"]

{-
 - 5.1.1.1.1.1	standard_type → integer
 - 5.2.1.1.1.1	standard_type → real
 -}
standard_type :: State -> State
standard_type e | extract e == RES "integer"	= e { tape = mover (tape e) }
		| extract e == RES "real"	= e { tape = mover (tape e) }
		| otherwise			= syntaxErr first e
	where
		first = [RES "integer", RES "real"]

{-
 - 6.1.1.1.1.1	subprogram_declarations → subprogram_declaration ; subprogram_declarations'
 -}
subprogram_declarations :: State -> State
subprogram_declarations e	=  e
				$> subprogram_declaration
				$> matchSym (DELIM ";")
				$> subprogram_declarations'

{-
 - 6.1.2.1.1.1	subprogram_declarations' → subprogram_declarations
 - 6.1.2.2.1.1	subprogram_declarations' → ε
 -}
subprogram_declarations' :: State -> State
subprogram_declarations' e | extract e == RES "function"	=  e
								$> subprogram_declarations
			   | extract e == RES "begin"		=  e
			   | otherwise				=  syntaxErr valid e
	where
		first = [RES "function"]
		follow = [RES "begin"]
		valid = first ++ follow

{-
 - 7.1.1.1.1.1	subprogram_declaration → subprogram_head subprogram_declaration'
 -}
subprogram_declaration :: State -> State
subprogram_declaration e	=  e
				$> subprogram_head
				$> subprogram_declaration'

{-
 - 7.1.1.1.2.1	subprogram_declaration' → declarations subprogram_declaration''
 - 7.1.1.1.2.2	subprogram_declaration' → subprogram_declaration''
 -}
subprogram_declaration' :: State -> State
subprogram_declaration' e | extract e == RES "var"	=  e
							$> declarations
							$> subprogram_declaration''
			  | extract e `elem` follow	=  e
							$> subprogram_declaration''
			  | otherwise			=  syntaxErr valid e
	where
		first = [RES "var"]
		follow = [RES "function", RES "begin"]
		valid = first ++ follow

{-
 - 7.1.1.1.3.1	subprogram_declaration'' → subprogram_declarations compound_statement
 - 7.1.1.1.1.2	subprogram_declaration'' → compound_statement
 -}
subprogram_declaration'' :: State -> State
subprogram_declaration'' e | extract e == RES "function"	=  e
								$> subprogram_declarations
								$> compound_statement
			   | extract e == RES "begin"		=  e
								$> compound_statement
			   | otherwise				=  syntaxErr first e
	where
		first = [RES "function", RES "begin"]

{-
 - 8.1.1.1.1.1	subprogram_head → function id subprogram_head'
 -}
subprogram_head :: State -> State
subprogram_head e	=  e
			$> matchSym (RES "function")
			$> matchID
			$> subprogram_head'

{-
 - 8.1.1.1.2.1	subprogram_head' → ( parameter_list ) subprogram_head'''
 - 8.1.1.1.2.2	subprogram_head' → subprogram_head'''
 -}
subprogram_head' :: State -> State
subprogram_head' e | extract e == DELIM "("	=  e { tape = mover (tape e) }
						$> parameter_list
						$> matchSym (DELIM ")")
						$> subprogram_head''
		   | extract e == DELIM ":"	=  e
						$> subprogram_head''
		   | otherwise			=  syntaxErr first e
	where
		 first = [DELIM "(", DELIM ":"]

{-
 - 8.1.1.1.3.1	subprogram_head'' → : standard_type ;
 -}
subprogram_head'' :: State -> State
subprogram_head'' e	=  e
			$> matchSym (DELIM ":")
			$> standard_type
			$> matchSym (DELIM ";")

{-
 - 10.1.1.1.1.1	parameter_list → id : type parameter_list'
 -}
parameter_list :: State -> State
parameter_list e	=  e
			$> matchID
			$> matchSym (DELIM ":")
			$> type_
			$> parameter_list'

{-
 - 10.1.2.1.1.1	parameter_list' → ; parameter_list
 - 10.1.2.2.1.1	parameter_list' → ε
 -}
parameter_list' :: State -> State
parameter_list' e | extract e == DELIM ";"	=  e { tape = mover (tape e) }
						$> parameter_list
		  | extract e == DELIM ")"	=  e
		  | otherwise			=  syntaxErr first e
	where
		first = [DELIM ";"]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 11.1.1.1.1.1	compound_statement → begin compound_statement'
 -}
compound_statement :: State -> State
compound_statement e	=  e
			$> matchSym (RES "begin")
			$> compound_statement'

{-
 - 11.1.1.1.2.1	compound_statement' → statement_list end
 - 11.1.1.1.2.2	compound_statement' → end
 -}
compound_statement' :: State -> State
compound_statement' e | extract e == RES "end"	=  e { tape = mover (tape e) }
		      | testID (extract e)	
		     || extract e == RES "begin"
		     || extract e == RES "while"
		     || extract e == RES "if"	=  e
						$> statement_list
						$> matchSym (RES "end")
		      | otherwise		=  syntaxErr first e
	where
		first = [RES "end", RES "begin", RES "while", RES "if", ID "_"]

{-
 - 13.1.1.1.1.1	statement_list → statement statement_list'
 -}
statement_list :: State -> State
statement_list e	=  e
			$> statement
			$> statement_list'

{-
 - 13.1.2.1.1.1	statement_list' → ; statement_list
 - 13.1.2.2.1.1	statement_list' → ε
 -}
statement_list' :: State -> State
statement_list' e | extract e == DELIM ";"	=  e { tape = mover (tape e) }
						$> statement_list
		  | extract e == RES "end"	=  e
		  | otherwise			=  syntaxErr valid e
	where
		first = [DELIM ";"]
		follow = [RES "end"]
		valid = first ++ follow

{-
 - 14.1.1.1.1.1	statement → variable assignop expression
 - 14.2.1.1.1.1	statement → compound_statement
 - 14.5.1.1.1.1	statement → while expression do statement
 - 14.3.1.1.1.1	statement → if expression then statement statement'
 -}
statement :: State -> State
statement e | extract e == RES "begin"	=  e
					$> compound_statement
	    | extract e == RES "while"	=  e { tape = mover (tape e) }
					$> expression
					$> matchSym (RES "do")
					$> statement
	    | extract e == RES "if"	=  e { tape = mover (tape e) }
					$> expression
					$> matchSym (RES "then")
					$> statement
					$> statement'
	    | testID (extract e)	= e
					$> variable
					$> matchSym ASSIGNOP
					$> expression
	    | otherwise			=  syntaxErr first e
	where
		first = [RES "begin", RES "while", RES "if", ID "_"]

{-
 - 14.3.1.1.2.1	statement' → else statement
 - 14.3.1.1.2.2	statement' → ε
 -}
statement' :: State -> State
statement' e | extract e == RES "else"	=  e { tape = mover (tape e) }
					$> statement
	     | extract e `elem` follow	=  e
	     | otherwise		=  syntaxErr valid e
	where
		first = [RES "else"]
		follow = [DELIM ";", RES "end"]
		valid = first ++ follow

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
variable' e | extract e == DELIM "["	=  e { tape = mover (tape e) }
					$> expression
					$> matchSym (DELIM "]")
	    | extract e == ASSIGNOP	=  e
	    | otherwise			=  syntaxErr valid e
	where
		first = [DELIM "["]
		follow = [ASSIGNOP]
		valid = first ++ follow

{-
 - 16.1.1.1.1.1	expression_list → expression expression_list'
 -}
expression_list :: State -> State
expression_list e	=  e
			$> expression
			$> expression_list'

{-
 - 16.1.2.1.1.1	expression_list' → , expression_list
 - 16.1.2.2.1.1	expression_list' → ε
 -}
expression_list' :: State -> State
expression_list' e | extract e == DELIM ","	=  e { tape = mover (tape e) }
						$> expression_list
		   | extract e == DELIM ")"	=  e
		   | otherwise			=  syntaxErr valid e
	where
		first = [DELIM ","]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 17.1.1.1.1.1	expression → simple_expression expression'
 -}
expression :: State -> State
expression e	=  e
		$> simple_expression
		$> expression'

{-
 - 17.1.1.1.2.1	expression' → relop simple_expression
 - 17.1.1.1.2.2	expression' → ε
 -}
expression' :: State -> State
expression' e | isRELOP (extract e)	=  e { tape = mover (tape e) }
					$> simple_expression
	      | extract e `elem` follow	=  e
	      | otherwise		=  syntaxErr valid e
	where
		first = [RELOP "_"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else"]
		valid = first ++ follow

{-
 - 18.1.1.1.1.1	simple_expression → term simple_expression'
 - 18.2.1.1.1.1	simple_expression → sign term simple_expression'
 -}
simple_expression :: State -> State
simple_expression e | isSIGN (extract e)	=  e { tape = mover (tape e) }
						$> term
						$> simple_expression'
		    | extract e `elem` first	=  e
						$> term
						$> simple_expression'
		    | otherwise			=  syntaxErr first e
	where
		first = [ID "_", DELIM "(", NUM, RES "not", SIGN]

{-
 - 18.3.2.1.1.1	simple_expression' → addop term simple_expression'
 - 18.3.2.2.1.1	simple_expression' → ε
 -}
simple_expression' :: State -> State
simple_expression' e | isADDOP (extract e)	=  e { tape = mover (tape e) }
						$> term
						$> simple_expression'
		     | extract e `elem` follow	=  e
		     | otherwise		=  syntaxErr valid e
	where
		first = [ADDOP "_"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]", RES "do",
			  RES "end", RES "then", RES "else", RELOP "_"]
		valid = first ++ follow
{-
 - 19.1.1.1.1.1	term → factor term'
 -}
term :: State -> State
term e	=  e
	$> factor
	$> term'

{-
 - 19.2.2.1.1.1	term' → mulop term
 - 19.2.2.2.1.1	term' → ε
 -}
term' :: State -> State
term' e	| isMULOP (extract e)		=  e { tape = mover (tape e) }
					$> term
	| extract e `elem` follow	=  e
	| otherwise			=  syntaxErr valid e
	where
		first = [MULOP "_"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else",
			  RELOP "_", ADDOP "_"]
		valid = first ++ follow

{-
 - 20.4.1.1.1.1	factor → num
 - 20.5.1.1.1.1	factor → not factor
 - 20.6.1.1.1.1	factor → ( expression )
 - 20.1.1.1.1.1	factor → id factor'
 -}
factor :: State -> State
factor e	| isNum (extract e)		=  e { tape = mover (tape e) }
		| testID (extract e) 		=  e { tape = mover (tape e) }
						$> factor'
		| extract e == DELIM "("	=  e { tape = mover (tape e) }
						$> expression
						$> matchSym (DELIM ")")
		| extract e == RES "not"	=  e { tape = mover (tape e) }
						$> factor
		| otherwise			= syntaxErr first e
	where
		first = [NUM, ID "_", DELIM "(", RES "not"]

{-
 - 20.1.1.1.2.1	factor' → ( expression_list )
 - 20.1.1.1.2.2	factor' → [ expression ]
 - 20.1.1.1.2.3	factor' → ε
 -}
factor' :: State -> State
factor' e | extract e == DELIM "("	=  e { tape = mover (tape e) }
					$> expression_list
					$> matchSym (DELIM ")")
	  | extract e == DELIM "["	=  e { tape = mover (tape e) }
					$> expression
					$> matchSym (DELIM "]")
	  | extract e `elem` follow	=  e
	  | otherwise			=  syntaxErr valid e
	where
		first = [DELIM "(", DELIM "["]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else",
			  RELOP "_", ADDOP "_", MULOP "_"]
		valid = first ++ follow
