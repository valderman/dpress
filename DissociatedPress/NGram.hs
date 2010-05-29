module DissociatedPress.NGram (
    NGram (..),
    empty, insert, DissociatedPress.NGram.lookup, delete, (!)
  ) where
import qualified Data.Map as M
import Data.Maybe

data NGram a = NGram {
    children :: M.Map a (NGram a)
  } deriving Show

empty :: NGram a
empty = NGram {children = M.empty}

(!) :: Ord a => NGram a -> [a] -> [a]
t ! k = fromJust $ DissociatedPress.NGram.lookup k t

insert :: Ord a => [a] -> NGram a -> NGram a
insert (k:ks) t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = Just $ insert ks t'
      f _         = Just $ insert ks $ NGram M.empty
insert _ t =
  t

lookup :: Ord a => [a] -> NGram a -> Maybe [a]
lookup (k:ks) t = do
  child <- M.lookup k (children t)
  DissociatedPress.NGram.lookup ks child
lookup [] (NGram t) =
  if null $ M.keys t
     then Nothing
     else Just $ M.keys t

delete :: Ord a => [a] -> NGram a -> NGram a
delete (k:ks) t =
  t {children = M.alter f k (children t)}
    where
      f (Just t') = let t'' = delete ks t' in
                        if (M.size $ children t'') == 0
                           then Nothing
                           else Just t''
      f _         = Nothing
delete [] t =
  DissociatedPress.NGram.empty
