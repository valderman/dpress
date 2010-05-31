-- | Trie-like data structure, specialized for storing n-grams.
{-# LANGUAGE MagicHash #-}
module DissociatedPress.NGram (
    NGram (..),
    empty, insert, DissociatedPress.NGram.lookup, delete, (!), elems,
    fold, merge, weightKey, weightIn
  ) where
import qualified Data.Map as M
import Data.Maybe
import GHC.Prim
import GHC.Int

data NGram a = NGram {
    weight   :: Int#,
    children :: M.Map a (NGram a)
  } deriving Show

-- | An empty n-gram trie
empty :: NGram a
empty = NGram {weight = 0#, children = M.empty}

-- | Returns the set of keys following the given key.
(!) :: Ord a => NGram a -> [a] -> [(a, Int)]
t ! k = fromJust $ DissociatedPress.NGram.lookup k t

-- | Standard fold over all n-grams in the trie.
fold :: Ord b => (a -> [(b, Int)] -> a) -> a -> NGram b -> a
fold f acc ngram = foldl f acc (elems ngram)

-- | Merge two n-gram tries together.
merge :: Ord a => NGram a -> NGram a -> NGram a
merge a b = let x = fold ins b a in x `seq` x
  where
    ins t k = let x = insert (map fst k) t in x `seq` x

-- | Return a list of all n-grams in the trie.
elems :: Ord a => NGram a -> [[(a, Int)]]
elems (NGram w t) = M.foldWithKey extract [] t
  where
    extract k v acc =
      case elems v of
        [] -> [(k, fromIntegral $ I32# w)]:acc
        ev ->  (map ((k, fromIntegral $ I32# w):) ev) ++ acc

-- | Insert a new n-gram into the trie.
insert :: Ord a => [a] -> NGram a -> NGram a
insert (k:ks) t =
  t {weight = weight t +# 1#,
     children = let x = M.alter f k (children t) in x `seq` x}
    where
      f (Just t') = Just $! insert ks t'
      f _         = Just $! insert ks empty
insert _ t =
  t {weight = weight t +# 1#}

-- | Return all keys following the given key in the trie.
lookup :: Ord a => [a] -> NGram a -> Maybe [(a, Int)]
lookup (k:ks) t = do
  child <- M.lookup k (children t)
  DissociatedPress.NGram.lookup ks child
lookup [] (NGram _ t) =
  if null $ M.keys t
     then Nothing
     else Just $ map (\(k, NGram w _) -> (k, fromIntegral $ I32# w))
               $ M.toList t

-- | Returns the probability weights for each section of the given key.
weightKey :: Ord a => [a] -> NGram a -> Maybe [(a, Int)]
weightKey (k:ks) (NGram w ch) = do
  child@(NGram w' ch') <- M.lookup k ch
  child' <- weightKey ks child
  return $ (k, fromIntegral $ I32# w') : child'
weightKey [] (NGram _ t) =
  return []

-- | Get the weight of the given key in the given dictionary.
--   This weight is calculated so that the closer to the beginning of the key
--   a subkey appears, the more it affects the key's weight. The first word
--   is worth twice as much as the second, which is worth twice as much as the
--   third, etc.
weightIn :: Ord a => [a] -> NGram a -> Int
weightIn k t =
  case weightKey k t of 
    Just k' -> (`div` length k) $ fst
             $ foldr (\w (n, w') -> (w*w'+n, w' `div` 2)) (0, startWeight)
             $ map snd k'
    _       -> 0
  where startWeight = 2^length k

-- | Delete a key from the trie. Note that deleting a key will also remove all
--   children of that key. For example, delete "abc" $ insert "abcde" will
--   leave you with an empty trie.
delete :: Ord a => [a] -> NGram a -> NGram a
delete (k:ks@(_:_)) t =
  t {children = let x = M.alter f k (children t) in x `seq` x}
    where
      f (Just t') = Just $! delete ks t'
      f _         = Nothing
delete [_] t =
  DissociatedPress.NGram.empty
delete [] t  =
  DissociatedPress.NGram.empty
