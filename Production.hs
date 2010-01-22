{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, Compute, Context (Context), epsilon, (=>>=),
			pushType, popType, peekType
		)
	where

import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Writer (Writer)

import Space (ascend, descend)
import Symbol ( Symbol (ID, INT, REAL, BIGREAL, LEXERR ) )
import Type ( Type ( REAL_t, INT_t, NULL_t ) )
import Display (Display)
import Defs ( Token )

{- For aspects of compiler that can't be resolved purely by PDA, we
 - need some sense of state. The symbol table, the current type context,
 - and the local scope should cover us.
 -}
data Context =	Context {
			display :: Display,
			types :: [Type]
		}
	deriving (Show)

{- Let's build a state monad to hide our state
 - I may be going to Haskell hell for using a different name, but I *really*
 - dislike the naming of the State monad. The monad itself is not a state, but
 - a function from a state to a (potentially) new state and result.
 - 
 - I choose to call it Compute because 1) I already have something that is
 - reasonably named state; 2) computation better describes what it's hiding.
 -}
type Compute = StateT Context (Writer ([String],[String]))

type Production = [Token] -> Compute [Token]

epsilon :: Production
epsilon = return

infixl 1 =>>=
(=>>=) :: Monad m => m a -> m b -> m a
x =>>= f = x >>= \n -> f >> return n

{- Get and put methods for each field in the State -}
getDisplay :: Compute Display
getDisplay = display `liftM` get

getTypes :: Compute [Type]
getTypes = types `liftM` get

putDisplay :: Display -> Compute ()
putDisplay d = do
		context <- get
		put context { display = d }

putTypes :: [Type] -> Compute ()
putTypes ts = do
		context <- get
		put context { types = ts }

modifyDisplay :: (Display -> Display) -> Compute ()
modifyDisplay f = do
		context <- get
		put context { display = f $ display context }

modifyTypes :: ([Type] -> [Type]) -> Compute ()
modifyTypes f = do
		context <- get
		put context { types = f $ types context }

{- Slightly more sophisticated actions on our state -}
peekType :: Compute Type
peekType = head `liftM` getTypes

pushType :: Type -> Compute ()
pushType t = modifyTypes (t:) 

popType :: Compute Type
popType = peekType =>>= modifyTypes (tail)

ascendDisplay :: Compute ()
ascendDisplay = ascend `liftM` getDisplay >>= putDisplay

-- insertDisplay :: Display -> Compute String

descendDisplay :: String -> Compute ()
descendDisplay key = do
			res <- descend key `liftM` getDisplay
			case res of
				Just d -> putDisplay d
				Nothing -> return ()

{-
pushScope :: String -> Compute ()
pushScope name = do
			ns <- getScope
			putScope (NS ns name)

popScope :: Compute ()
popScope = do
		ns <- getScope
		case ns of
			GLOBAL -> return ()
			NS base top -> putScope base

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
