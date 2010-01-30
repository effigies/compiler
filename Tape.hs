{- Tape.hs
 - A traversible tape data structure
 -
 - Consists of a left stack, a right stack, and a focus
 -}

module Tape (Tape(Tape), tapify, tapify', detape, mover, movel, cutl, cutr, right, left, focus, left') where

type Stack = []
data Tape a = Tape { left :: Stack a,
		     focus :: a,
		     right :: Stack a }

left' = reverse . left

instance (Show a) => Show (Tape a) where
	show (Tape l h r) = show (reverse l) ++ " <" ++ show h ++ "> " ++ show r

-- Create a new tape from a list
tapify :: [a] -> Tape a
tapify (x:xs) = Tape [] x xs

tapify' :: a -> [a] -> Tape a
tapify' trail = tapify . (++ [trail])

detape :: Tape a -> [a]
detape (Tape l h r) = reverse l ++ h : r

-- Motions on a tape
mover, movel :: (Show a) => Tape a -> Tape a
mover (Tape    ls  h    [] ) = error . show $ reverse (h:ls)
mover (Tape    ls  h (r:rs)) = Tape (h:ls) r    rs

movel (Tape    []  h    rs ) = error $ show (h:rs)
movel (Tape (l:ls) h    rs ) = Tape    ls  l (h:rs)

-- Cut the tape
cutl, cutr :: Tape a -> Tape a
cutl (Tape  _ h rs ) = Tape [] h rs
cutr (Tape ls h  _ ) = Tape ls h []