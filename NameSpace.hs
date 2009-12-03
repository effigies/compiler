{- NameSpace.hs
 - Define namespace structure and visibility checker
 -}

module NameSpace ( NameSpace (GLOBAL, NS), visible )
	where

{- Each variable has a scope, and we need to check that we are in its
   scope when we call it. -}
data NameSpace = GLOBAL | NS NameSpace String
	deriving Eq

instance Show NameSpace where
	show GLOBAL = "GLOBAL"
	show (NS parent name) = show parent ++ "." ++ name

{- visible scope variable
 -
 - test whether a variable is visible from the local scope
 -} 
visible :: NameSpace -> NameSpace -> Bool
visible _ GLOBAL = True
visible GLOBAL _ = False
visible local@(NS parent name) test = local == test || visible parent test
