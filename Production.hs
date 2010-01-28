{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, epsilon, wrap,
			pushType, popType, peekType, flushTypes,
			makeArray, insertVariable, makeDecl, makeParam,
			makeFunction
		)
	where

import Control.Monad (liftM)

import Compute ( Compute, Context (Context),
			getTypes, modifyTypes, putTypes,
			getDisplay, modifyDisplay, putDisplay,
			getName, modifyName, putName,
			tellLeft, tellRight)
import Space (Space (Space), Cxt (Top), Spacer, ascend, descend, insertSubr,
		insertLocalr)
import Type ( Type (ARRAY_t, NULL_t, INT_t, REAL_t, FUNCTION_t), baseType )
import Display (Display, Namespace)
import Defs ( Token )
import Data.Map (empty)

import Control.Monad.Writer ( runWriter )
import Control.Monad.State ( runStateT )

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
peekType :: Compute Type
peekType = head `liftM` getTypes

pushType :: Type -> Compute [Type]
pushType = modifyTypes . (:) 

popType :: Compute Type
popType = head `liftM` modifyTypes tail

{- Clear the type stack, returning its contents -}
flushTypes :: Compute [Type]
flushTypes = modifyTypes $ const []

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
insertVariable k v = modifyDisplay $ insertLocalr k v

makeDecl :: Production
makeDecl = wrap $ do
		n <- getName
		t <- popType
		insertVariable n t

makeParam :: Production
makeParam = wrap $ do
		n <- getName
		t <- peekType
		insertVariable n t

makeFunction :: Production
makeFunction = wrap $ do
			n <- getName
			(t:ts) <- flushTypes
			let f = FUNCTION_t (reverse ts) t
			insertNamespace $ Space n f empty empty
			descendDisplay n
