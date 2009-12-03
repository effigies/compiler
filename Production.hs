{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, epsilon,
		getTable, getType, getScope, putTable, putType, putScope,
		pushScope, popScope, setType,
		typeof
		)
	where

import Control.Monad (liftM)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Writer (Writer)

import Symbol ( Symbol (ID, REF, INT, REAL, BIGREAL, LEXERR ) )
import Type ( Type ( REAL_t, INT_t, NULL_t ) )
import NameSpace ( NameSpace ( GLOBAL, NS ) )
import Defs ( Token )

{- For aspects of compiler that can't be resolved purely by PDA, we
 - need some sense of state. The symbol table, the current type context,
 - and the local scope should cover us.
 -}
data Context =	Context {
			table :: [Symbol],
			ctype :: Type,
			scope :: NameSpace
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
type Compute = StateT Context (Writer [String])

--runComputation = runWriter

type Production = [Token] -> Compute [Token]

epsilon :: Production
epsilon = return

{- Get and put methods for each field in the State -}
getTable :: Compute [Symbol]
getTable = liftM table get

getType :: Compute Type
getType = liftM ctype get

getScope :: Compute NameSpace
getScope = liftM scope get

putTable :: [Symbol] -> Compute ()
putTable tab = do
		context <- get
		put context { table = tab}

putType :: Type -> Compute ()
putType tp = do
		context <- get
		put context { ctype = tp }

putScope :: NameSpace -> Compute ()
putScope ns = do
		context <- get
		put context { scope = ns }

{- Slightly more complex, and rather more useful for actual calling -}
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

{- Put it all together, and we can now compute the type of any known object -}
typeof :: Symbol -> Compute Type
typeof (ID _ _ t) = return t
typeof (BIGREAL _) = return REAL_t
typeof (REAL _) = return REAL_t
typeof (INT _) = return INT_t
typeof (LEXERR _ s) = typeof s
typeof (REF n) = do
			tab <- getTable
			typeof (tab !! n)
typeof _ = return NULL_t
