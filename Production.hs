{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, epsilon, wrap,
			pushType, popType, peekType, flushTypes,
			pushName, popName, peekName, flushNames,
			makeArray, insertVariable, makeDecl,
			makeFunction, ascendDisplay,
			reportErr
		)
	where

import Control.Monad (liftM)
import Control.Monad.State (get, runStateT)
import Control.Monad.Writer ( runWriter )

import Compute ( Compute, Context (Context),
			getTypes, modifyTypes, putTypes,
			getDisplay, modifyDisplay, putDisplay,
			getNames, modifyNames, putNames,
			tellLeft, tellRight)
import Space (Space (Space), Cxt (Top), Spacer, ascend, descend, insertSubr,
		insertLocalr, lookupInScope)
import Type ( Type (ARRAY_t, NULL_t, INT_t, REAL_t, FUNCTION_t), baseType )
import Display (Display, Namespace)
import Symbol ( Symbol (..) )
import Defs ( Token )
import Data.Map (empty, fromList)
import Util ( tail' )

{-
 - Every production will take a list of tokens, act upon it, potentially
 - updating state or producing output.
 -}
type Production = [Token] -> Compute [Token]

{- An epsilon production does not consume any tokens -}
epsilon :: Production
epsilon = return

{- This lets us wrap up a monadic action, passing the results from the
 - previous action as the input to the next.
 -
 - Here, we will use it to turn actions of type Compute () into Productions
 -}
wrap :: Monad m => m a -> b -> m b
wrap f n = f >> return n

{- Treat our [Type] as a stack -}
headType [] = NULL_t
headType ts = head ts

peekType :: Compute Type
peekType = headType `liftM` getTypes

pushType :: Type -> Compute [Type]
pushType = modifyTypes . (:) 

popType :: Compute Type
popType = headType `liftM` modifyTypes tail'

headName [] = ""
headName ts = head ts

peekName :: Compute String
peekName = headName `liftM` getNames

pushName :: String -> Compute [String]
pushName = modifyNames . (:) 

popName :: Compute String
popName = headName `liftM` modifyNames tail'

{- Clear the type stack, returning its contents -}
flushTypes :: Compute [Type]
flushTypes = modifyTypes $ const []

flushNames :: Compute [String]
flushNames = modifyNames $ const []

makeArray :: Production
makeArray = wrap $ modifyTypes (\(b:a:ts) -> a { baseType = b } : ts)



ascendDisplay :: Production
ascendDisplay = wrap $ modifyDisplay ascend

{- For the moment, we'll be unsafe, and just not do anything if we cannot
 - descend
 -}
descendDisplay :: String -> Compute ()
descendDisplay key = do
			res <- descend key `liftM` getDisplay
			case res of
				Just d -> putDisplay d
				Nothing -> return ()

insertNamespace :: Namespace -> Compute Display
insertNamespace = modifyDisplay . insertSubr

insertVariable :: String -> Type -> Compute Display
insertVariable k = modifyDisplay . insertLocalr k

makeDecl :: Production
makeDecl = wrap $ do
		n <- popName
		t <- popType
		insertVariable n t

makeFunction :: Production
makeFunction = wrap $ do
			(n:ns) <- reverse `liftM` flushNames
			(t:ts) <- flushTypes
			let ts' = reverse ts
			let f = FUNCTION_t ts' t
			let locals = fromList $ zip ns ts'
			insertNamespace $ Space n f locals empty
			descendDisplay n

reportErr :: Token -> Compute ()
reportErr = tellLeft . show

typeof :: Symbol -> Compute Type
typeof (RELOP _)			= return $ FUNCTION_t [REAL_t, REAL_t] INT_t
typeof (MULOP m)	| m == "*"
		       || m == "/"	= return $ FUNCTION_t [REAL_t, REAL_t] REAL_t
			| otherwise	= return $ FUNCTION_t [INT_t, INT_t] INT_t -- div/mod/and
typeof (ADDOP a)	| a == "or"	= return $ FUNCTION_t [INT_t, INT_t] INT_t
			| otherwise	= return $ FUNCTION_t [REAL_t, REAL_t] REAL_t -- +/-
typeof (ID n)				= do
						mt <- lookupInScope n `liftM` getDisplay
						case mt of
							Just t	-> return t
							Nothing	-> return NULL_t
typeof (BIGREAL _)			= return REAL_t
typeof (REAL _)				= return REAL_t
typeof (INT _)				= return INT_t
typeof (LEXERR _ s)			= typeof s
typeof _				= return NULL_t

