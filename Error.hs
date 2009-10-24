{- Error.hs
 -
 - This stuff is sort of related to match, but it's messy enough to warrant its
 - own file.
 -}

module Error ( syntaxErr, resolveErr, onErr ) where

{- We need to know about Symbols -}
import Defs
import Tape

onErr :: State -> Bool
onErr (State (Tape _ (SYNTAXERR _ _) _) _) = True
onErr _ = False

-- syntaxErr
-- Simply note that the current symbol is a syntax error, store a list of valid
-- symbols (for reporting). We'll continue on until we reach a nice collection
-- point, at which time we will calmly panic.
syntaxErr :: [Symbol] -> State -> State

-- We don't want nested syntax errors. If we already have an error, we're
-- waiting to panic.
syntaxErr _ st | onErr st = st

syntaxErr val (State (Tape l f r) s) = State (Tape l (SYNTAXERR val [f]) r) s

-- resolveErr
-- Discover and resolve syntax errors, panicking until we find a synchronizing
-- token
resolveErr :: [Symbol] -> State -> State
resolveErr f st = st { tape = resolveErr' f (tape st) }

-- resolveErr'
-- This is where we actually do those things I just said. I'll explain each
-- line.
resolveErr' :: [Symbol] ->  Tape Token -> Tape Token
-- If we've reached an early EOF, there's not much we can do except keep
-- moving.
resolveErr' f t@(Tape _ (SYNTAXERR _ _) []) = t
-- If we've found something in the synch set, we can move on.
resolveErr' f t@(Tape _ (SYNTAXERR _ _) (r:_)) | inSynch (sym r) f = mover t
-- If not, let's put that token in our syntax error, so we can report it later
					| otherwise = resolveErr' f (panic t)
-- There's no syntax error. Moving right along.
resolveErr' _ t = t

-- inSynch
-- Check to see if we're in the synch set
-- synch(P) = follow(P) ++ [EOF]
inSynch :: Symbol -> [Symbol] -> Bool
inSynch EOF _ = True
inSynch r follow = r `elem` follow

-- panic
-- I figured that, given the name of the technique, one of these functions
-- should be named panic. This just grabs the next token, and sticks it into
-- our list of failed tokens.
panic :: Tape Token -> Tape Token
panic (Tape l (SYNTAXERR v f) (r:rs)) = Tape l (SYNTAXERR v (f ++ [r])) rs