module Space (Space (Space), label, meta, local, subs,
		insertLocal, insertSub, lookupLocal, lookupSub, lookupBoth,
		checkLocal, checkSub, checkBoth,
		Cxt (Cxt, Top), parent, siblings,
		Spacer, descend, ascend, top, insertLocalr, insertSubr,
		trail, labels, metatrail)
	where

import Prelude hiding (lookup)
import Data.Map as Map hiding (map)
import Monad (mplus, liftM)

{- A space is a structure with a set of local objects and a set of subspaces.
 - 
 - The inspiration, and how I shall restrict my usage, later, is namespaces,
 - where each space has a name, a type, a set of uniquely named local variables
 - (with an associated type), and a set of uniquely named subspaces.
 -}
data Ord a => Space a b = Space {
			label	:: a,
			meta	:: b,
			local	:: Map a b,
			subs	:: Map a (Space a b)
		}
	deriving Eq
	
instance (Ord a, Show a, Show b) => Show (Space a b) where
	show (Space la me lo su) = unlines final
		where
			final = (showBase:showLocals) ++ map ('\t':) (lines showSubs)
			showBase = (show la) ++ "::" ++ (show me)
			showLocals = map showPairs $ toList lo 
			showPairs (l,m) = "\t" ++ show l ++ "::" ++ show m
			join _ [] = ""
			join x (y:ys) = x ++ y ++ join x ys
			showSubs = concatMap show $ elems su

{- insertLocal is essentially an insert but cuddled into our
 - structure
 -}
insertLocal :: Ord a => a -> b -> Space a b -> Space a b
insertLocal k v space = space {
		local = insert k v (local space)
	}

{- insertSub enforces that label(subs[k]) = k -}
insertSub :: Ord a => Space a b -> Space a b -> Space a b
insertSub sub@(Space k _ _ _) space = space {
		subs = insert k sub (subs space)
	}

checkLocal :: Ord a => a -> Space a b -> Bool
checkLocal k (Space _ _ l _) = member k l

checkSub :: Ord a => a -> Space a b -> Bool
checkSub k (Space _ _ _ s) = member k s

checkBoth :: Ord a => a -> Space a b -> Bool
checkBoth k s = checkLocal k s || checkSub k s

lookupLocal :: Ord a => a -> Space a b -> Maybe b
lookupLocal k (Space _ _ l _) = lookup k l

lookupSub :: Ord a => a -> Space a b -> Maybe b
lookupSub k (Space _ _ _ s) = meta `liftM` lookup k s

lookupBoth :: Ord a => a -> Space a b -> Maybe b
lookupBoth k s = lookupLocal k s `mplus` lookupSub k s

{- This is the context for a zipper for Space -}
data Cxt a b = Cxt {
			parent :: (a, b, Map a b, Cxt a b),
			siblings :: Map a (Space a b)
		}
		| Top
	deriving Show

{- Spacer = Space zipper -}
type Spacer a b = (Space a b, Cxt a b)

{-
 - Now that we have a zipper, we can implement interesting movement functions
 -}

{- Descend one level, indexed by key -}
descend :: (Ord a) => a -> Spacer a b -> Maybe (Spacer a b)
descend k (space, spacer) = do
	s <- lookup k $ subs space
	return (s, Cxt (label space, meta space, local space, spacer) $
		delete k (subs space) )

{- Ascend one level -}
ascend :: Ord a => Spacer a b -> Spacer a b
ascend (space, Top) = (space, Top)
ascend (h@(Space k _ _ _), Cxt (l, m, c, p) s) = (Space l m c (insert k h s), p)

{- Get the full unzipped Space from the current zipper -}
top :: Ord a => Spacer a b -> Space a b
top (space, Top) = space
top subspace = top . ascend $ subspace

{- Pull insertLocal into our zipper -}
insertLocalr :: Ord a => a -> b -> Spacer a b -> Spacer a b
insertLocalr k v (space, cxt) = (insertLocal k v space, cxt)

{- Pull insertSub into our zipper -}
insertSubr :: Ord a => Space a b -> Spacer a b -> Spacer a b
insertSubr sub (space, cxt) = (insertSub sub space, cxt)

{-
 - Follow the contexts up to get a sequence of (label, meta) pairs
 -}
trail :: Ord a => Spacer a b -> [(a,b)]
trail v = trail' v []

trail' :: Ord a => Spacer a b -> [(a,b)] -> [(a,b)]
trail' (Space l m _ _, Top) = ((l,m) :) -- consider id, instead
trail' v@(Space l m _ _, _) = trail' (ascend v) . ((l,m) :)

labels :: Ord a => Spacer a b -> [a]
labels = map fst . trail

metatrail :: Ord a => Spacer a b -> [b]
metatrail = map snd . trail
