-- | Trie-like data structure, specialized for storing n-grams.
{-# LANGUAGE MagicHash #-}
module DissociatedPress.NGram (
    NGram (..),
    empty, insert, DissociatedPress.NGram.lookup, delete, (!), elems,
    fold, merge, weightKey, weightIn, numChildren, subNGram, childList
  ) where
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List (foldl')
import Data.Int

data NGram = NGram {
    weight   :: !Int,
    children :: !(M.Map Int NGram)
  } deriving Show

-- | An empty n-gram trie
empty :: NGram
empty = NGram {weight = 0, children = id $! M.empty}

-- | Returns the set of keys following the given key.
(!) :: NGram -> [Int] -> [(Int, Int)]
t ! k = fromJust $ DissociatedPress.NGram.lookup k t

-- | Standard fold over all n-grams in the trie.
fold :: (a -> [(Int, Int)] -> a) -> a -> NGram -> a
fold f acc ngram = foldl' f acc (elems ngram)

-- | Return the number of children this node has.
numChildren :: NGram -> Int
numChildren = M.size . children

-- | Return a list of all children belonging to this node.
childList :: NGram -> [(Int, Int)]
childList (NGram _ ch) =
  M.foldrWithKey (\k (NGram w _) xs ->
                    xs `seq` (k, fromIntegral w):xs) [] ch

-- | Return the n-gram trie pointed to by the given key.
subNGram :: [Int] -> NGram -> Maybe NGram
subNGram (x:xs) n =
  M.lookup x (children n) >>= subNGram xs
subNGram _ n =
  return n

-- | Merge two n-gram tries together.
merge :: NGram -> NGram -> NGram
merge a b = fold ins b a
  where
    ins t k = insert (map fst k) t

-- | Return a list of all n-grams in the trie.
elems :: NGram -> [[(Int, Int)]]
elems (NGram w t) = M.foldrWithKey extract [] t
  where
    extract k v acc =
      case elems v of
        [] -> [(k, fromIntegral w)]:acc
        ev ->  (map ((k, fromIntegral w):) ev) ++ acc

-- | Insert a new n-gram into the trie.
insert :: [Int] -> NGram -> NGram
insert (k:ks) t =
  t {weight = weight t + 1,
     children = let x = M.alter f k (children t) in x `seq` x}
    where
      f (Just t') = Just $! insert ks $! t'
      f _         = Just $! insert ks $! empty
insert _ t =
  t {weight = weight t + 1}

-- | Return all keys following the given key in the trie.
lookup :: [Int] -> NGram -> Maybe [(Int, Int)]
lookup (k:ks) t = do
  child <- M.lookup k (children t)
  DissociatedPress.NGram.lookup ks child
lookup [] (NGram _ t) =
  if null $ M.keys t
     then Nothing
     else Just $ map (\(k, NGram w _) -> (k, fromIntegral w))
               $ M.toList t

-- | Returns the probability weights for each section of the given key.
weightKey :: [Int] -> NGram -> Maybe [(Int, Int)]
weightKey (k:ks) (NGram w ch) = do
  child@(NGram w' ch') <- M.lookup k ch
  child' <- weightKey ks child
  return $ (k, fromIntegral w') : child'
weightKey [] (NGram _ t) =
  return []

-- | Get the weight of the given key in the given dictionary.
--   This weight is calculated so that the closer to the beginning of the key
--   a subkey appears, the less it affects the key's weight. The first word
--   is worth half as much as the second, which is worth half as much as the
--   third, etc.
weightIn :: [Int] -> NGram -> Int
weightIn k t =
  case weightKey k t of 
    Just k' -> (`div` length k) $ fst
             $ foldr (\w (n, w') -> (w*w'+n, w'*2)) (0, startWeight)
             $ map snd k'
    _       -> 0
  where startWeight = 1

-- | Delete a key from the trie. Note that deleting a key will also remove all
--   children of that key. For example, delete "abc" $ insert "abcde" will
--   leave you with an empty trie.
delete :: [Int] -> NGram -> NGram
delete (k:ks@(_:_)) t =
  t {children = id $! M.alter f k (children t)}
    where
      f (Just t') = Just $! delete ks t'
      f _         = Nothing
delete [_] t =
  DissociatedPress.NGram.empty
delete [] t  =
  DissociatedPress.NGram.empty
