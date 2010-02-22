{- Type.hs
 - Define the types recognized by our language
 -}

module Type ( Type (INT_t, REAL_t, ARRAY_t, FUNCTION_t, NULL_t),
		lBound, uBound, baseType, parameters, returnType,
		sizeof, wider, 
		isWider, isFunction, isArray, isPrimitive, isFirstClass)
	where

import Util (join)

data Type = INT_t
	  | REAL_t
	  | ARRAY_t { lBound :: Int, uBound :: Int, baseType :: Type }
	  | FUNCTION_t { parameters :: [Type], returnType :: Type }
	  | NULL_t
	deriving Eq

instance Show Type where
	show INT_t = "int"
	show REAL_t = "real"
	show (ARRAY_t l u t) = "array[" ++ show l ++ " .. " ++ show u ++ "] of " ++ show t
	show (FUNCTION_t ps r) = show r ++ "(" ++ join "," (map show ps) ++ ")"
	show NULL_t = "void"

sizeof :: Type -> Int
sizeof NULL_t		= 0
sizeof INT_t		= 4
sizeof REAL_t		= 8
sizeof (ARRAY_t l u b)	= (u - l + 1) * sizeof b
sizeof (FUNCTION_t p r) = sum . map sizeof $ p

wider :: Type -> Type -> Type
wider INT_t INT_t = INT_t
wider _ _ = REAL_t

isWider :: Type -> Type -> Bool
isWider t1 t2 | t1 == t2	= True
isWider REAL_t INT_t		= True
isWider _ _			= False

isFunction :: Type -> Bool
isFunction (FUNCTION_t _ _)	= True
isFunction _			= False

isArray :: Type -> Bool
isArray (ARRAY_t _ _ _)		= True
isArray _			= False

isPrimitive :: Type -> Bool
isPrimitive INT_t	= True
isPrimitive REAL_t	= True
isPrimitive _		= False

isFirstClass :: Type -> Bool
isFirstClass t = isPrimitive t || isArray t