
module Display (Display, Reference) where

import Space
import Type
import Monad (mplus)

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