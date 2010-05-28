module Trie where
import qualified Data.Map as M

data Trie k v = Trie {
    item     :: Maybe v,
    children :: M.Map k (Trie k v)
  } deriving Show

empty :: Trie k v
empty = Trie {item = Nothing, children = M.empty}

(!) :: Ord k => Trie k v -> [k] -> v
t ! k =
  case Trie.lookup k t of
    Just v -> v
    _      -> error "Element not found in trie!"

elems :: Trie k v -> [v]
elems (Trie (Just v) children) =
  v:(concat $ M.elems $ M.map elems children)
elems (Trie _ children) =
  concat $ M.elems $ M.map elems children

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert (k:ks) v t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = Just $ insert ks v t'
      f _         = Just $ insert ks v $ Trie Nothing M.empty
insert [] v t =
  t {item = Just v}

insertNGram :: Ord a => [a] -> Trie a [a] -> Trie a [a]
insertNGram (k:ks) t =
  t {item = Just v', children = M.alter f k (children t)}
    where
      v'          = case item t of
        Just i -> k:i
        _      -> [k]
      f (Just t') = Just $ insertNGram ks t'
      f _         = Just $ insertNGram ks $ Trie Nothing M.empty
insertNGram _ t =
  t

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup (k:ks) t = do
  child <- M.lookup k (children t)
  Trie.lookup ks child
lookup [] t =
  item t

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete (k:ks) t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = let t'' = delete ks t' in
                        if (M.size $ children t'') == 0
                           then Nothing
                           else Just t''
      f _         = Nothing
delete [] t =
  t {item = Nothing}

alter :: Ord k => (Maybe v -> Maybe v) -> [k] -> Trie k v -> Trie k v
alter transform keys t =
  case transform $ Trie.lookup keys t of
    Just v' -> Trie.insert keys v' t
    _       -> Trie.delete keys t