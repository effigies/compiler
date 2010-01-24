{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, epsilon, wrap,
			pushType, popType, peekType, flushTypes,
		)
	where

import Control.Monad (liftM)

import Compute ( Compute, Context (Context),
			getTypes, modifyTypes, modifyTypes,
			getDisplay, modifyDisplay, putDisplay)
import Space (ascend, descend, insertSubr, insertLocalr)
import Type ( Type (ARRAY_t), baseType )
import Display (Display, Namespace)
import Defs ( Token )

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

pushType :: Type -> Production
pushType = wrap . modifyTypes . (:) 

popType :: Compute Type
popType = head `liftM` modifyTypes tail

{- Clear the type stack, returning its contents -}
flushTypes :: Compute [Type]
flushTypes = modifyTypes $ const []

makeArray :: Int -> Int -> Production
makeArray l u = wrap $ modifyTypes (\(b:ts) -> ARRAY_t l u b : ts)

ascendDisplay :: Production
ascendDisplay = wrap $ modifyDisplay ascend

{- For the moment, we'll be unsafe, and just not do anything if we cannot
 - descend
 -}
descendDisplay :: String -> Production
descendDisplay key = wrap $ do
			res <- descend key `liftM` getDisplay
			case res of
				Just d -> putDisplay d
				Nothing -> return ()

insertNamespace :: Namespace -> Production
insertNamespace = wrap . modifyDisplay . insertSubr

insertVariable :: String -> Type -> Production
insertVariable k v = wrap . modifyDisplay $ insertLocalr k v