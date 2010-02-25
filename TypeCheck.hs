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
import Symbol ( Symbol (ID, INT, REAL, BIGREAL, RELOP, MULOP, ADDOP, LEXERR) )
import Type ( Type (INT_t, REAL_t, ARRAY_t, FUNCTION_t, NULL_t), returnType,
		isPrimitive, isArray, isFirstClass, isFunction, isWider,
		wider)
import Defs ( Token, sym )
import Util ( join )

import Control.Monad ( liftM, when )

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
typeof _ = return NULL_t

assert :: (Type -> Bool) -> (Type -> Token -> String) -> Type -> Token -> Compute ()
assert test errMsg t	| test t	= \_ -> return ()
			| otherwise	= tellLeft . errMsg t

describe :: Type -> Token -> String
describe t tok = "; having type " ++ show t ++ ", we received: " ++ show tok

assertType :: Type -> Type -> Token -> Compute ()
assertType t = assert (== t) $ mismatch t
	where
		mismatch :: Type -> Type -> Token -> String
		mismatch t1 t2 tok = "Type mismatch. Expecting " ++
					show t1 ++ describe t2 tok

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
		expectFunction t tok = "Expecting function" ++ describe t tok

assertFirstClass :: Production
assertFirstClass (tok:toks) = do
				t <- peekType
				assert isFirstClass expectFC t tok
				return (tok:toks)
	where
		expectFC :: Type -> Token -> String
		expectFC t tok = "Expecting a first class (primitive or array)"
			++ " type before " ++ show tok ++ "; Received: " ++ show t

assertArray :: Token -> Compute ()
assertArray tok = do
			t <- peekType
			assert isArray expectArray t tok
	where
		expectArray :: Type -> Token -> String
		expectArray t tok = "Expecting array" ++ describe t tok

assertPrimitive :: Type -> Token -> Compute ()
assertPrimitive = assert isPrimitive expectPrimitive
	where
		expectPrimitive :: Type -> Token -> String
		expectPrimitive t tok = "Expecting real or int" ++ describe t tok				

assertWider :: Type -> Type -> Token -> Compute ()
assertWider t = assert (isWider t) $ invalidCoercion t
	where
		invalidCoercion :: Type -> Type -> Token -> String
		invalidCoercion t1 t2 tok = "Invalid type coercion: " ++
			show t2 ++ " -> " ++ show t1 ++ " at " ++ show tok

validateAssignment :: Token -> Production
validateAssignment var = wrap $ do
				rtype <- popType
				ltype <- peekType
				if isFunction ltype
					then assertWider (returnType ltype) rtype var
					else assertWider ltype rtype var

validateFunction :: Token -> Production
validateFunction tok = wrap $ do
	types <- getTypes
	let (args, remnants) = break isFunction types
	if null remnants
		then
			tellLeft "You don't seem to have called a function, and yet, here we are..."
		else do
			let (FUNCTION_t params ret:stack) = remnants
			putTypes (ret:stack)
			when (params /= reverse args) $
				tellLeft ("Invalid function call. Function has "
					++ "type " ++ show (head remnants) ++
					"; argument types were (" ++
					join "," (map show $ reverse args) ++ ") at " ++
					show tok)

reduceRelop :: Token -> Production
reduceRelop rel = wrap $ do
			t2 <- popType
			t1 <- popType
			assertType INT_t t1 rel
			assertType INT_t t2 rel
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
