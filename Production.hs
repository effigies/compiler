{- Production.hs
 - Define the mode of computation for our grammar productions
 -}

module Production ( Production, epsilon, wrap,
			pushType, popType, peekType, dropTypes,
			pushName,
			makeArray, insertVariable, makeDecl,
			makeFunction, ascendDisplay,
			dereferenceArray,
			reportErr
		)
	where

import Control.Monad (liftM)

import Compute ( Compute,
		getTypes, modifyTypes, putTypes,
		getDisplay, modifyDisplay,
		getNames, modifyNames,
		tellLeft, tellRight)
import Space (Space (Space), ascend, insertLocalr, insertAndDescend, labels, checkBoth, lookupInScope)
import Type ( Type (NULL_t, FUNCTION_t), baseType, sizeof, isArray )
import Display (Display)
import Defs ( Token (Token), Line (Line) )
import Data.Map (empty, fromList)
import Util ( tail', join )

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

pushName :: String -> Compute [String]
pushName = modifyNames . (:) 

popName :: Compute String
popName = headName `liftM` modifyNames tail'

{- Clear the type stack, returning its contents -}
flushTypes :: Compute [Type]
flushTypes = modifyTypes $ const []

dropTypes :: Production
dropTypes = wrap flushTypes

flushNames :: Compute [String]
flushNames = modifyNames $ const []

{- If our type stack looks like [t,ARRAY_t,...], we can set the base type
 - of the array to t.
 -}
makeArray :: Production
makeArray = wrap $ modifyTypes (\(b:a:ts) -> a { baseType = b } : ts)

{-
 - 
 -}
ascendDisplay :: Production
ascendDisplay = wrap $ modifyDisplay ascend

insertVariable :: String -> Type -> Compute Display
insertVariable k = modifyDisplay . insertLocalr k

makeDecl :: Token -> Production
makeDecl (Token l@(Line num _) _) = wrap $ do
		n <- popName
		t <- popType
		d <- getDisplay
		let ns = join "::" $ labels d
		let var = join "::" [ns,n]
		if checkBoth n $ fst d
			then do
				tellLeft l $ "Line " ++ show num ++ ": Duplicate "
					++ "declaration: " ++ var 
				return d
			else do
				tellRight $ var ++ " : " ++ show (sizeof t)
				insertVariable n t

{- makeFunction - build a namespace from the values in the name and type
 -		  stacks, insert into the display, and descend into this space
 -}
makeFunction :: Token -> Production
makeFunction (Token l@(Line num _) _) = wrap $ do
		(n:ns) <- reverse `liftM` flushNames
		(t:ts) <- flushTypes
		let ts' = reverse ts
		let f = FUNCTION_t ts' t
		let locals = fromList $ zip ns ts'
		d <- getDisplay
		case lookupInScope n d of
			Just _ -> do
				let ns = join "::" $ labels d
				let var = join "::" [ns,n]
				tellLeft l $ "Line " ++ show num ++ ": Duplicate"
					++ " function declaration: " ++ var
			Nothing -> return ()
		modifyDisplay . insertAndDescend $ Space n f locals empty

reportErr :: Token -> Compute ()
reportErr t@(Token l _) = tellLeft l $ show t

dereferenceArray :: Production
dereferenceArray = wrap $ do
				popType
				array <- popType
				if isArray array
					then pushType $ baseType array
					else pushType array