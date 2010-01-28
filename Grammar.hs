{- Grammar.hs
 -
 - LL(1) Grammar
 -}

module Grammar ( program ) where

import Symbol ( Symbol (..) )
import Defs ( sym )
import Production
import Match
import Error ( syntaxErr, resolveErr )
import Test

import Type ( Type (NULL_t, INT_t, REAL_t, ARRAY_t, FUNCTION_t) )

{- Remember that we have a state/writer monad Compute, and
 - Production = [Token] -> Compute [Token] -}

{-
 - 1.1.1.1.1.1	program → program id ( identifier_list ) ; program'
 -}

program :: Production
program ts   = match (RES "program") ts
	   >>= matchProgName
	   >>= match (DELIM "(")
	   >>= identifier_list
	   >>= match (DELIM ")")
	   >>= matchSynch (DELIM ";")
	   >>= program'
	where
		first = [RES "program"]
		follow = [EOF]

{-
 - 1.1.1.1.2.1	program' → declarations program''
 - 1.1.1.1.2.2	program' → program''
 -}
program' :: Production
program' (t:ts) | sym t == RES "var"   = declarations (t:ts)
				     >>= program''
	   	| sym t `elem` first   = program'' (t:ts)
		| otherwise	       = syntaxErr first (t:ts)
				     >>= resolveErr follow
	where
		first = [RES "function", RES "begin", RES "var"]
		follow = [EOF]

{-
 - 1.1.1.1.3.1	program'' → subprogram_declarations program'''
 - 1.1.1.1.3.2	program'' → program'''
 -}
program'' :: Production
program'' (t:ts) | sym t == RES "function"   = subprogram_declarations (t:ts)
					   >>= program'''
		 | sym t == RES "begin"	     = program''' (t:ts)
		 | otherwise		     = syntaxErr first (t:ts)
					   >>= resolveErr follow
	where
		first = [RES "function", RES "begin"]
		follow = [EOF]

{-
 - 1.1.1.1.4.1	program''' → compound_statement .
 -}
program''' :: Production
program''' ts   = compound_statement ts
	      >>= match DOT
	      >>= match EOF
	      >>= resolveErr follow
	where
		first = [RES "begin"]
		follow = [EOF]

{-
 - 2.1.1.1.1.1	identifier_list → id identifier_list'
 -}
identifier_list :: Production
identifier_list ts   = matchIdent ts
		   >>= identifier_list'
	where
		first = [VAR]
		follow = [DELIM ")"]

{-
 - 2.1.2.1.1.1	identifier_list' → , identifier_list
 - 2.1.2.2.1.1	identifier_list' → ε
 -}
identifier_list' :: Production
identifier_list' (t:ts) | sym t == DELIM ","   = identifier_list ts
			| sym t == DELIM ")"   = epsilon (t:ts)
			| otherwise	       = syntaxErr valid (t:ts)
					     >>= resolveErr follow
	where
		first = [DELIM ","]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 3.1.1.1.1.1	declarations → var id : type ; declarations'
 -}
declarations :: Production
declarations ts	= match (RES "var") ts
	      >>= matchName
	      >>= match (DELIM ":")
	      >>= type_		-- Since type is a Haskell keyword
	      >>= makeDecl
	      >>= matchSynch (DELIM ";")
	      >>= declarations'
	where
		first = [RES "var"]
		follow = [RES "function", RES "begin"]

{-
 - 3.1.2.1.1.1	declarations' → declarations
 - 3.1.2.2.1.1	declarations' → ε
 -}
declarations' :: Production
declarations' (t:ts) | sym t == RES "var"    = declarations (t:ts)
		     | sym t `elem` follow   = epsilon (t:ts)
		     | otherwise	     = syntaxErr valid (t:ts)
					   >>= resolveErr follow
	where
		first = [RES "var"]
		follow = [RES "function", RES "begin"]
		valid = first ++ follow

{-
 - 4.2.1.1.1.1	type → array [ num .. num ] of standard_type
 - 4.1.1.1.1.1	type → standard_type
 -}
type_ :: Production
type_ (t:ts) | sym t == RES "array" = pushType (ARRAY_t 0 0 NULL_t)
				  >>  match (DELIM "[") ts
				  >>= matchLowerBound
				  >>= match (DELIM "..")
				  >>= matchUpperBound
				  >>= match (DELIM "]")
				  >>= match (RES "of")
				  >>= standard_type
				  >>= makeArray
	     | sym t `elem` first   = standard_type (t:ts)
	     | otherwise	    = syntaxErr first (t:ts)
				  >>= resolveErr follow
	where
		first = [RES "integer", RES "real", RES "array"]
		follow = [DELIM ";", DELIM ")"]

{-
 - 5.1.1.1.1.1	standard_type → integer
 - 5.2.1.1.1.1	standard_type → real
 -}
standard_type :: Production
standard_type (t:ts) | sym t == RES "integer"	= pushType INT_t >> return ts
		     | sym t == RES "real"	= pushType REAL_t >> return ts
		     | otherwise		= syntaxErr first (t:ts)
					      >>= resolveErr follow
	where
		first = [RES "integer", RES "real"]
		follow = [DELIM ";", DELIM ")"]

{-
 - 6.1.1.1.1.1	subprogram_declarations → subprogram_declaration ; subprogram_declarations'
 -}
subprogram_declarations :: Production
subprogram_declarations ts   = subprogram_declaration ts
			   >>= matchSynch (DELIM ";")
			   >>= subprogram_declarations'
	where
		first = [RES "function"]
		follow = [RES "begin"]

{-
 - 6.1.2.1.1.1	subprogram_declarations' → subprogram_declarations
 - 6.1.2.2.1.1	subprogram_declarations' → ε
 -}
subprogram_declarations' :: Production
subprogram_declarations' (t:ts) | sym t == RES "function"   = subprogram_declarations (t:ts)
				| sym t == RES "begin"	    = epsilon (t:ts)
				| otherwise		    = syntaxErr valid (t:ts)
							  >>= resolveErr follow
	where
		first = [RES "function"]
		follow = [RES "begin"]
		valid = first ++ follow

{-
 - 7.1.1.1.1.1	subprogram_declaration → subprogram_head subprogram_declaration'
 -}
subprogram_declaration :: Production
subprogram_declaration ts   = subprogram_head ts
			  >>= subprogram_declaration'
	where
		first = [RES "function"]
		follow = [DELIM ";"]

{-
 - 7.1.1.1.2.1	subprogram_declaration' → declarations subprogram_declaration''
 - 7.1.1.1.2.2	subprogram_declaration' → subprogram_declaration''
 -}
subprogram_declaration' :: Production
subprogram_declaration' (t:ts) | sym t == RES "var"   = declarations (t:ts)
						    >>= subprogram_declaration''
			       | sym t `elem` first   = subprogram_declaration'' (t:ts)
			       | otherwise	      = syntaxErr first (t:ts)
						    >>= resolveErr follow
	where
		first = [RES "function", RES "begin", RES "var"]
		follow = [DELIM ";"]

{-
 - 7.1.1.1.3.1	subprogram_declaration'' → subprogram_declarations compound_statement
 - 7.1.1.1.1.2	subprogram_declaration'' → compound_statement
 -}
subprogram_declaration'' :: Production
subprogram_declaration'' (t:ts) | sym t == RES "function"   = subprogram_declarations (t:ts)
							  >>= compound_statement
-- 							 =>>= popScope
				| sym t == RES "begin"	    = compound_statement (t:ts)
-- 							 =>>= popScope
				| otherwise		    = syntaxErr first (t:ts)
							  >>= resolveErr follow
	where
		first = [RES "function", RES "begin"]
		follow = [DELIM ";"]

{-
 - 8.1.1.1.1.1	subprogram_head → function id subprogram_head'
 -}
subprogram_head :: Production
subprogram_head ts   = match (RES "function") ts
		   >>= matchName
		   >>= subprogram_head' ts''
		   >>= makeFunction
	where
		first = [RES "function"]
		follow = [RES "var", RES "function", RES "begin"]
{-
 - 8.1.1.1.2.1	subprogram_head' → ( parameter_list ) subprogram_head'''
 - 8.1.1.1.2.2	subprogram_head' → subprogram_head'''
 -}
subprogram_head' :: Production
subprogram_head' (t:ts) | sym t == DELIM "("	=   parameter_list ts
						>>= match (DELIM ")")
						>>= subprogram_head''
			| sym t == DELIM ":"	=   subprogram_head'' (t:ts)
			| otherwise		=   syntaxErr first (t:ts)
						>>= resolveErr follow
	where
		first = [DELIM "(", DELIM ":"]
		follow = [RES "var", RES "function", RES "begin"]

{-
 - 8.1.1.1.3.1	subprogram_head'' → : standard_type ;
 -}
subprogram_head'' :: Production
subprogram_head'' ts	=   match (DELIM ":") ts
			>>= standard_type
			>>= matchSynch (DELIM ";")
	where
		first = [DELIM ":"]
		follow = [RES "var", RES "function", RES "begin"]

{-
 - 10.1.1.1.1.1	parameter_list → id : type parameter_list'
 -}
parameter_list :: Production
parameter_list ts	=   match VAR ts
			>>= match (DELIM ":")
			>>= type_
			>>= parameter_list'
	where
		first = [VAR]
		follow = [DELIM ")"]

{-
 - 10.1.2.1.1.1	parameter_list' → ; parameter_list
 - 10.1.2.2.1.1	parameter_list' → ε
 -}
parameter_list' :: Production
parameter_list' (t:ts) | sym t == DELIM ";"	=   parameter_list ts
		       | sym t == DELIM ")"	=   epsilon (t:ts)
		       | otherwise		=   syntaxErr valid (t:ts)
						>>= resolveErr follow
	where
		first = [DELIM ";"]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 11.1.1.1.1.1	compound_statement → begin compound_statement'
 -}
compound_statement :: Production
compound_statement ts	=   match (RES "begin") ts
			>>= compound_statement'
	where
		first = [RES "begin"]
		follow = [DELIM ".", DELIM ";", RES "end", RES "else"]

{-
 - 11.1.1.1.2.1	compound_statement' → statement_list end
 - 11.1.1.1.2.2	compound_statement' → end
 -}
compound_statement' :: Production
compound_statement' (t:ts) | sym t == RES "end"	=   epsilon ts
			   | sym t `elem` first	=   statement_list (t:ts)
						>>= match (RES "end")
			   | otherwise		=   syntaxErr first (t:ts)
						>>= resolveErr follow
	where
		first = [VAR, RES "begin", RES "while", RES "if", RES "end"]
		follow = [DELIM ".", DELIM ";", RES "end", RES "else"]

{-
 - 13.1.1.1.1.1	statement_list → statement statement_list'
 -}
statement_list :: Production
statement_list ts	=   statement ts
			>>= statement_list'
	where
		first = [VAR, RES "begin", RES "while", RES "if"]
		follow = [RES "end"]

{-
 - 13.1.2.1.1.1	statement_list' → ; statement_list
 - 13.1.2.2.1.1	statement_list' → ε
 -}
statement_list' :: Production
statement_list' (t:ts) | sym t == DELIM ";"	=   statement_list ts
		       | sym t `elem` follow	=   epsilon (t:ts)
		       | otherwise		=   syntaxErr valid (t:ts)
						>>= resolveErr follow
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
statement :: Production
statement (t:ts) | sym t == RES "begin"	=  compound_statement (t:ts)
		 | sym t == RES "while"	=   expression ts
					>>= match (RES "do")
					>>= statement
		| sym t == RES "if"	=   expression ts
					>>= match (RES "then")
					>>= statement
					>>= statement'
		| sym t == VAR		=   variable (t:ts)
					>>= match ASSIGNOP
					>>= expression
		| otherwise		=   syntaxErr first (t:ts)
					>>= resolveErr follow
	where
		first = [RES "begin", RES "while", RES "if", VAR]
		follow = [RES "end", RES "else", DELIM ";"]

{-
 - 14.3.1.1.2.1	statement' → else statement
 - 14.3.1.1.2.2	statement' → ε
 -}
statement' :: Production
statement' (t:ts) | sym t == RES "else"	=   statement ts
		  | sym t `elem` follow	=   epsilon (t:ts)
		  | otherwise		=   syntaxErr valid (t:ts)
					>>= resolveErr follow
	where
		first = [RES "else"]
		follow = [DELIM ";", RES "end", RES "else"]
		valid = follow -- first is a subset of follow

{-
 - 15.1.1.1.1.1	variable → id variable'
 -}
variable :: Production
variable ts	=   match VAR ts
		>>= variable'
	where
		first = [VAR]
		follow = [ASSIGNOP]

{-
 - 15.1.1.1.2.1	variable' → [ expression ]
 - 15.1.1.1.2.1	variable' → ε
 -}
variable' :: Production
variable' (t:ts) | sym t == DELIM "["	=   expression ts
					>>= match (DELIM "]")
		 | sym t `elem` follow	=   epsilon (t:ts)
		 | otherwise		=   syntaxErr valid (t:ts)
-- ASSIGNOP is a pretty silly thing to synch to
--					>>= resolveErr follow
	where
		first = [DELIM "["]
		follow = [ASSIGNOP]
		valid = first ++ follow

{-
 - 16.1.1.1.1.1	expression_list → expression expression_list'
 -}
expression_list :: Production
expression_list ts	=   expression ts
			>>= expression_list'
	where
		first = [NUM, RES "not", DELIM "(", VAR, SIGN]
		follow = [DELIM ")"]

{-
 - 16.1.2.1.1.1	expression_list' → , expression_list
 - 16.1.2.2.1.1	expression_list' → ε
 -}
expression_list' :: Production
expression_list' (t:ts) | sym t == DELIM ","	=   expression_list ts
			| sym t == DELIM ")"	=   epsilon (t:ts)
			| otherwise		=   syntaxErr valid (t:ts)
						>>= resolveErr follow
	where
		first = [DELIM ","]
		follow = [DELIM ")"]
		valid = first ++ follow

{-
 - 17.1.1.1.1.1	expression → simple_expression expression'
 -}
expression :: Production
expression ts	=   simple_expression ts
		>>= expression'
	where
		first = [NUM, RES "not", DELIM "(", VAR, SIGN]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else"]
{-
 - 17.1.1.1.2.1	expression' → relop simple_expression
 - 17.1.1.1.2.2	expression' → ε
 -}
expression' :: Production
expression' (t:ts) | sym t == RELOP "_"		=   simple_expression ts
		   | sym t `elem` follow	=   epsilon (t:ts)
		   | otherwise			=   syntaxErr valid (t:ts)
						>>= resolveErr follow
	where
		first = [RELOP "_"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else"]
		valid = first ++ follow

{-
 - 18.1.1.1.1.1	simple_expression → term simple_expression'
 - 18.2.1.1.1.1	simple_expression → sign term simple_expression'
 -}
simple_expression :: Production
simple_expression (t:ts) | sym t == SIGN	=   term ts
						>>= simple_expression'
			 | sym t `elem` first	=   term (t:ts)
						>>= simple_expression'
			 | otherwise		=   syntaxErr first (t:ts)
						>>= resolveErr follow
	where
		first = [VAR, DELIM "(", RES "not", NUM, SIGN]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]", RES "do",
			  RES "end", RES "then", RES "else", RELOP "_"]

{-
 - 18.3.2.1.1.1	simple_expression' → addop term simple_expression'
 - 18.3.2.2.1.1	simple_expression' → ε
 -}
simple_expression' :: Production
simple_expression' (t:ts) | sym t == ADDOP "_"	=   term ts
						>>= simple_expression'
			  | sym t `elem` follow	=   epsilon (t:ts)
			  | otherwise		=   syntaxErr valid (t:ts)
						>>= resolveErr follow
	where
		first = [ADDOP "_"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]", RES "do",
			  RES "end", RES "then", RES "else", RELOP "_"]
		valid = first ++ follow
{-
 - 19.1.1.1.1.1	term → factor term'
 -}
term :: Production
term ts	=   factor ts
	>>= term'
	where
		first = [VAR, DELIM "(", RES "not", NUM]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else",
			  RELOP "_", ADDOP "_"]

{-
 - 19.2.2.1.1.1	term' → mulop term
 - 19.2.2.2.1.1	term' → ε
 -}
term' :: Production
term' (t:ts)	| sym t == MULOP "_"	=   term ts
		| sym t `elem` follow	=   epsilon (t:ts)
		| otherwise		=   syntaxErr valid (t:ts)
					>>= resolveErr follow
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
factor :: Production
factor (t:ts)	| sym t == NUM			=   epsilon ts
		| sym t == VAR			=   factor' ts
		| sym t == DELIM "("		=   expression ts
						>>= match (DELIM ")")
		| sym t == RES "not"		=   factor ts
		| otherwise			=   syntaxErr first (t:ts)
						>>= resolveErr follow
	where
		first = [NUM, VAR, DELIM "(", RES "not"]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else",
			  RELOP "_", ADDOP "_", MULOP "_"]

{-
 - 20.1.1.1.2.1	factor' → ( expression_list )
 - 20.1.1.1.2.2	factor' → [ expression ]
 - 20.1.1.1.2.3	factor' → ε
 -}
factor' :: Production
factor' (t:ts) | sym t == DELIM "("	=   expression_list ts
					>>= match (DELIM ")")
	  | sym t == DELIM "["		=   expression ts
					>>= match (DELIM "]")
	  | sym t `elem` follow		=   epsilon (t:ts)
	  | otherwise			=   syntaxErr valid (t:ts)
					>>= resolveErr follow
	where
		first = [DELIM "(", DELIM "["]
		follow = [DELIM ")", DELIM ";", DELIM ",", DELIM "]",
			  RES "end", RES "do", RES "then", RES "else",
			  RELOP "_", ADDOP "_", MULOP "_"]
		valid = first ++ follow
