-- | Trie-like data structure, specialized for storing n-grams.
module DissociatedPress.NGram (
    NGram (..),
    empty, insert, DissociatedPress.NGram.lookup, delete, (!), elems,
    fold, merge
  ) where
import qualified Data.Map as M
import Data.Maybe

data NGram a = NGram {
    children :: M.Map a (NGram a)
  } deriving Show

-- | An empty n-gram trie
empty :: NGram a
empty = NGram {children = M.empty}

-- | Returns the set of keys following the given key.
(!) :: Ord a => NGram a -> [a] -> [a]
t ! k = fromJust $ DissociatedPress.NGram.lookup k t

-- | Standard fold over all n-grams in the trie.
fold :: Ord b => (a -> [b] -> a) -> a -> NGram b -> a
fold f acc ngram = foldl f acc (elems ngram)

-- | Merge two n-gram tries together.
merge :: Ord a => NGram a -> NGram a -> NGram a
merge a b = fold (flip insert) b a

-- | Return a list of all n-grams in the trie.
elems :: Ord a => NGram a -> [[a]]
elems (NGram t) = M.foldWithKey extract [] t
  where
    extract k v acc =
      case elems v of
        [] -> [k]:acc
        ev ->  (map (k:) ev) ++ acc

-- | Insert a new n-gram into the trie.
insert :: Ord a => [a] -> NGram a -> NGram a
insert (k:ks) t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = Just $ insert ks t'
      f _         = Just $ insert ks $ NGram M.empty
insert _ t =
  t

-- | Return all keys following the given key in the trie.
lookup :: Ord a => [a] -> NGram a -> Maybe [a]
lookup (k:ks) t = do
  child <- M.lookup k (children t)
  DissociatedPress.NGram.lookup ks child
lookup [] (NGram t) =
  if null $ M.keys t
     then Nothing
     else Just $ M.keys t

-- | Delete a key from the trie. Note that deleting a key will also remove all
--   children of that key. For example, delete "abc" $ insert "abcde" will
--   leave you with an empty trie.
delete :: Ord a => [a] -> NGram a -> NGram a
delete (k:ks@(_:_)) t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = Just $ delete ks t'
      f _         = Nothing
delete [_] t =
  DissociatedPress.NGram.empty
delete [] t  =
  DissociatedPress.NGram.empty
