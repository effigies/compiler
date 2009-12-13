import Space
import Type


type Display = Spacer String Type
type Name = (Display, String)

typeof :: Name -> Maybe Type
typeof ((subspace,_),name) = lookupBoth name subspace
