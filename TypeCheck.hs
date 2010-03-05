{- TypeCheck.hs
 -
 -}

module TypeCheck (typeof,
		assertFunction, assertArray, assertPrimitive,
		assertFirstClass, assertTopType,
		assertType, assertWider, validateAssignment,
		reduceRelop, reduceAddop, reduceMulop,
		validateFunction) where

import Production ( Production, wrap, popType, pushType, peekType )
import Compute ( Compute, getDisplay, tellLeft, getTypes, putTypes )
import Space ( lookupInScope )
import Symbol ( Symbol (ID, INT, REAL, BIGREAL, RELOP, MULOP, ADDOP, LEXERR), isSyntaxErr )
import Type ( Type (INT_t, REAL_t, ARRAY_t, FUNCTION_t, NULL_t), returnType,
		isPrimitive, isArray, isFirstClass, isFunction, isWider,
		wider)
import Defs ( Token (Token), Line (Line), sym )
import Util ( join )

import Control.Monad ( liftM, when )

typeof :: Symbol -> Type
typeof (BIGREAL _)			= REAL_t
typeof (REAL _)				= REAL_t
typeof (INT _)				= INT_t
typeof (LEXERR _ s)			= typeof s
typeof _				= NULL_t

assert :: (Type -> Bool) -> (Type -> Token -> String) -> Type -> Token -> Compute ()
assert test errMsg t tok@(Token l s)	| test t	= return ()
			| isSyntaxErr s	= return ()	
			| otherwise	= tellLeft l $ errMsg t tok

describe :: String -> Type -> Token -> String
describe s t (Token (Line num line) sy) = "Line " ++ show num ++ ": " ++ s
	++ "; having type " ++ show t ++ ", we received: " ++ show sy

assertType :: Type -> Type -> Token -> Compute ()
assertType t = assert (== t) $ mismatch t
	where
		mismatch :: Type -> Type -> Token -> String
		mismatch = describe . ("Type mismatch. Expecting " ++) . show

assertTopType :: Type -> Token -> Production
assertTopType t tok = wrap $ do
			t' <- peekType
			assertType t t' tok

{- We can recognize function calls in the grammar, so it's best to
 - ensure that the name we're calling is, in fact, a function.
 -}
assertFunction :: Token -> Compute ()
assertFunction tok = do
		t <- peekType
		assert isFunction expectFunction t tok
	where
		expectFunction :: Type -> Token -> String
		expectFunction = describe "Expecting function"

assertFirstClass :: Production
assertFirstClass (tok:toks) = do
				t <- peekType
				assert isFirstClass expectFC t tok
				return (tok:toks)
	where
		expectFC :: Type -> Token -> String
		expectFC = describe $ "Expecting a first class (primitive or array) \
			\type before "

assertArray :: Token -> Compute ()
assertArray tok = do
			t <- peekType
			assert isArray expectArray t tok
	where
		expectArray :: Type -> Token -> String
		expectArray = describe "Expecting array"

assertPrimitive :: Type -> Token -> Compute ()
assertPrimitive = assert isPrimitive expectPrimitive
	where
		expectPrimitive :: Type -> Token -> String
		expectPrimitive = describe "Expecting real or int"				

assertWider :: Type -> Type -> Token -> Compute ()
assertWider t = assert (isWider t) $ invalidCoercion t
	where
		invalidCoercion :: Type -> Type -> Token -> String
		invalidCoercion t1 t2 (Token (Line num line) sy) =
			"Line " ++ show num ++ ": Invalid type coercion: " ++
			show t2 ++ " -> " ++ show t1 ++ " at token " ++ show sy

validateAssignment :: Token -> Production
validateAssignment var = wrap $ do
				rtype <- popType
				ltype <- peekType
				if isFunction ltype
					then assertWider (returnType ltype) rtype var
					else assertWider ltype rtype var

validateFunction :: Token -> Production
validateFunction (Token l@(Line num _) sy) = wrap $ do
	types <- getTypes
	let (args, remnants) = break isFunction types
	if null remnants
		then
			tellLeft l $ "Line " ++ show num ++ ": " ++ show sy ++
				" was called as a function, but was not."
		else do
			let (FUNCTION_t params ret:stack) = remnants
			putTypes (ret:stack)
			when (params /= reverse args) $
				tellLeft l ("Line " ++ show num ++ ": Invalid function call. \
					\Function has type " ++ show (head remnants) ++
					"; argument types were (" ++
					join "," (map show $ reverse args) ++ ")")

reduceRelop :: Token -> Production
reduceRelop rel = wrap $ do
			t2 <- popType
			t1 <- popType
			assertPrimitive t1 rel
			assertPrimitive t2 rel
			pushType INT_t

reduceAddop :: Token -> Production
reduceAddop add = wrap $ do
			t2 <- popType
			t1 <- popType
			if sym add == ADDOP "or"
				then do
					assertType INT_t t1 add
					assertType INT_t t2 add
					pushType INT_t
				else do
					assertPrimitive t1 add
					assertPrimitive t2 add
					pushType $ wider t1 t2

reduceMulop :: Token -> Production
reduceMulop mul = wrap $ do
			t2 <- popType
			t1 <- popType
			if sym mul `elem` [MULOP "*", MULOP "/"]
				then do
					assertPrimitive t1 mul
					assertPrimitive t2 mul
					pushType $ wider t1 t2
				else do
					assertType INT_t t1 mul
					assertType INT_t t2 mul
					pushType INT_t
