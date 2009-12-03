{- Type.hs
 - Define the types recognized by our language
 -}

module Type ( Type (INT_t, REAL_t, ARRAY_t, NULL_t), len, baseType )
	where

data Type = INT_t
          | REAL_t
          | ARRAY_t { len :: Int, baseType :: Type }
          | NULL_t
	deriving Eq

instance Show Type where
	show INT_t = "integer type"
	show REAL_t = "real type"
	show (ARRAY_t n t) = "array[" ++ show n ++ "] of " ++ show t
	show NULL_t = "no type"
