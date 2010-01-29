
module Display (Namespace, Display, Reference) where

import Data.Map hiding (map)
import Space
import Type
import Monad (mplus)

type Namespace = Space String Type
type Display = Spacer String Type
type Reference = (Display, String)

findRef :: String -> Display -> Maybe Reference
findRef sym disp@(space, Top) | checkBoth sym space = Just (disp, sym)
			     | otherwise	   = Nothing
findRef sym disp@(space, _)   | checkBoth sym space = Just (disp, sym)
			     | otherwise	   = findRef sym $ ascend disp

inScope :: String -> Display -> Bool
inScope sym disp@(space, Top) = checkBoth sym space
inScope sym disp@(space,   _) = checkBoth sym space || inScope sym (ascend disp)
{-
instance Show Namespace where
	show (Space l m loc sub) = l ++ show m ++ map ("\n\t" ++) toList loc

instance Show Display where
	show disp = show $ top disp
-}
