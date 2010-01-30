{- Compute.hs
 - 
 - Here we define the monad that surrounds our computations, and primitive
 - methods for accessing its state.
 - 
 -}

module Compute ( Compute, Context (Context),
		getTypes, modifyTypes, modifyTypes_, putTypes,
		getDisplay, modifyDisplay, modifyDisplay_, putDisplay,
		getNames, modifyNames, modifyNames_, putNames,
		tellLeft, tellRight,
		display, types, names
		)
	where

import Control.Monad (liftM)
import Control.Monad.State (StateT, put, get)
import Control.Monad.Writer (Writer, tell)

import Type ( Type )
import Display (Display, Namespace)

{- 
 - For aspects of compiler that can't be resolved purely by PDA, we need some
 - sense of context.
 - A display is essentially the accessible scope. I have implemented it as a
 - zippered tree, where the nodes represent scopes, and the leaves represent
 - varaibles. It thus also incorporates the information which would
 - traditionally be represented in a symbol table.
 - I also maintain a stack of types, and a stack of names. Typically, these
 - will only hold a single value, but should grow (at identical rates) while
 - function parameters are parsed.
 -}
data Context =	Context {
			display :: Display,
			types :: [Type],
			names :: [String]
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


getNames :: Compute [String]
getNames = names `liftM` get

modifyNames :: ([String] -> [String]) -> Compute [String]
modifyNames f = do
		context <- get
		let n = names context
		put context { names = f n }
		return n

modifyNames_ :: ([String] -> [String]) -> Compute ()
modifyNames_ f = modifyNames f >> return ()

putNames :: [String] -> Compute ()
putNames = modifyNames_ . const

{- Writer methods -}
tellLeft :: String -> Compute ()
tellLeft s = tell ([s],[])

tellRight :: String -> Compute ()
tellRight s = tell ([],[s])
