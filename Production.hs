{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, Compute, Context (Context), epsilon, (=>>=),
			pushType, popType, peekType, flushTypes,
			tellLeft, tellRight
		)
	where

import Control.Monad (liftM)
import Control.Monad.State (StateT, put, get)
import Control.Monad.Writer (Writer, tell, MonadWriter)

import Data.Monoid (mempty)

import Space (ascend, descend, insertSubr)
import Symbol ( Symbol (ID, INT, REAL, BIGREAL, LEXERR ) )
import Type ( Type ( REAL_t, INT_t, NULL_t ) )
import Display (Display, Namespace)
import Defs ( Token )

{- 
 - For aspects of compiler that can't be resolved purely by PDA, we need some
 - sense of context.
 - A display is essentially the accessible scope. I have implemented it as a
 - zippered tree, where the nodes represent scopes, and the leaves represent
 - varaibles. It thus also incorporates the information which would
 - traditionally be represented in a symbol table.
 - I also maintain a stack of types.
 -}
data Context =	Context {
			display :: Display,
			types :: [Type]
		}
	deriving (Show)

{- 
 - To keep our productions clean, we want to thread some sense of state through
 - our PDA.
 -
 - First, we need our context, which we will read and update repeatedly.
 - Also, we will want a couple of lists of strings for output. The first is for
 - error messages, and the second for variable size information.
 -
 - Therefore, we need a state/writer monad.
 -}
type Compute = StateT Context (Writer ([String],[String]))

{-
 - Every production will take a list of tokens, act upon it, potentially
 - updating state or producing output.
 -}
type Production = [Token] -> Compute [Token]

{- An epsilon production does not consume any tokens -}
epsilon :: Production
epsilon = return

{-
 - This is a handy little function that allows us to insert functions which
 - update the state or output, while stringing the result of the last
 - computation forward.
 -
 - Should be the same as (<*), but I've found this works as expected slightly
 - more often.
 -}
infixl 1 =>>=
(=>>=) :: Monad m => m a -> m b -> m a
-- x =>>= f = x >>= (\n -> f >> return n)
{- Pointfree version. Because why not? -}
(=>>=) = flip $ (=<<) . flip (flip (>>) . return)

{- >>= wrap $ ACTION
 - would be equivalent to
 - =>>= ACTION
 -
 - I'm not sure which I prefer, yet.
wrap :: Monad m => m a -> b -> m b
wrap f n = f >> return n
wrap = flip $ flip (>>) . return
-}

{- Get, modify, and put methods for each field in the State -}
getDisplay :: Compute Display
getDisplay = display `liftM` get

modifyDisplay :: (Display -> Display) -> Compute Display
modifyDisplay f = do
		context <- get
		let d = display context
		put context { display = f d }
		return d

modifyDisplay_ :: (Display -> Display) -> Compute ()
modifyDisplay_ f = modifyDisplay f >> return ()

putDisplay :: Display -> Compute ()
putDisplay = modifyDisplay_ . const

getTypes :: Compute [Type]
getTypes = types `liftM` get

modifyTypes :: ([Type] -> [Type]) -> Compute [Type]
modifyTypes f = do
		context <- get
		let ts = types context
		put context { types = f ts }
		return ts

modifyTypes_ :: ([Type] -> [Type]) -> Compute ()
modifyTypes_ f = modifyTypes f >> return ()

putTypes :: [Type] -> Compute ()
putTypes = modifyTypes_ . const

{- Slightly more sophisticated actions on our state -}

{- Treat our [Type] as a stack -}
peekType :: Compute Type
peekType = head `liftM` getTypes

pushType :: Type -> Compute ()
pushType = modifyTypes_ . (:) 

popType :: Compute Type
popType = head `liftM` modifyTypes tail

{- Clear the type stack, returning its contents -}
flushTypes :: Compute [Type]
flushTypes = modifyTypes $ const []

ascendDisplay :: Compute ()
ascendDisplay = modifyDisplay_ ascend

{- For the moment, we'll be unsafe, and just not do anything if we cannot
 - descend
 -}
descendDisplay :: String -> Compute ()
descendDisplay key = do
			res <- descend key `liftM` getDisplay
			case res of
				Just d -> putDisplay d
				Nothing -> return ()

insertNamespace :: Namespace -> Compute ()
insertNamespace = modifyDisplay_ . insertSubr

insertVariable

{- Writer methods -}
tellLeft :: String -> Compute ()
tellLeft s = tell ([s],[])

tellRight :: String -> Compute ()
tellRight s = tell ([],[s])

{-
setType :: Symbol -> Compute ()
setType tok = typeof tok >>= putType

typeof :: Symbol -> Maybe Type
typeof (BIGREAL _) = Just REAL_t
typeof (REAL _) = Just REAL_t
typeof (INT _) = Just INT_t
typeof (LEXERR _ s) = typeof s
typeof (REF ((sub,_),name)) = lookupBoth name subspace
typeof _ = Nothing
-}