
module SymbolTree (Display, Reference) where

import Space
import Type


type Display = Spacer String Type
type Reference = (Display, String)

findRef :: String -> Display -> Maybe Reference

inScope :: String -> Display -> Bool
inScope = (Nothing /=) . findRef
