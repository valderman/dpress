-- | Core functionality for generating random sequences using n-grams.
module DissociatedPress.Core (
    Dictionary (..),
    newDict, defDict, updateDict, setPreferredKeyLength,
    disPress, disPressBack, randomPress,
    randomKey, toKey, isKeyIn, optKey, merge
  ) where
import qualified DissociatedPress.NGram as N
import Data.List
import Data.Maybe (fromJust, isNothing)
import System.Random

data Ord a => Dictionary a = Dictionary {
    maxKeyLen    :: Int,
    preferKeyLen :: Int,
    twoWay       :: Bool,
    dict         :: N.NGram a,
    dict2        :: N.NGram a
  } deriving Show

-- | Create a dictionary with default settings. This dictionary is optimized
--   for producing nice text with short keylengths. See 'newDict' for the
--   default settings used for this dictionary.
defDict :: Ord a => Dictionary a
defDict = Dictionary {
    maxKeyLen    = 3,
    preferKeyLen = 3,
    twoWay       = True,
    dict         = N.empty,
    dict2        = N.empty
  }

-- | Sets the preferred key length of the given dictionary. The value is
--   to the maximum key length.
setPreferredKeyLength :: Ord a => Int -> Dictionary a -> Dictionary a
setPreferredKeyLength len d =
  d {preferKeyLen = min len (maxKeyLen d)}

-- | Generate a new dictionary with custom settings.
newDict :: Ord a
        => Int          -- ^ Maximum key length that can be used with this
                        --   dictionary. Dictionary size increases linear
                        --   to this parameter.
                        --   Default: 3
        -> Int          -- ^ Preferred key length. If a key is used that is
                        --   shorter than this, the generator will try to
                        --   derive a key of this length to improve the
                        --   generated results. Setting this to 0 means that
                        --   the generator will never try to optimize keys.
                        --   Default: 3
        -> Bool         -- ^ Is this a two way dictionary or not? A two way
                        --   dictionary may be used to generate sentences
                        --   backwards from the provided key, whereas one way
                        --   dictionaries can only be used to generate forward.
                        --   A two way dictionary takes up roughly twice as
                        --   as much memory as the equivalent one way dict.
                        --   Default: True
        -> Dictionary a
newDict max prefer twoway = defDict {
    maxKeyLen    = max,
    preferKeyLen = prefer,
    twoWay       = twoway
  }

-- | Merges two dictionaries. The resulting dictionaries will use lowest
--   common denominator settings; lowest max and preferred key lengths, only
--   use forward dictionary unless both dictionaries are two way.
--   It is worth noting that the first dictionary will be merged into the
--   second one. Thus, if you have a small dictionary a and a large dictionary
--   b that you wish to merge, doing merge a b would be faster than doing
--   merge b a.
merge :: Ord a => Dictionary a -> Dictionary a -> Dictionary a
merge a b =
  Dictionary {
      maxKeyLen    = min (maxKeyLen a) (maxKeyLen b),
      preferKeyLen = min (preferKeyLen a) (preferKeyLen b),
      twoWay       = twoWay a && twoWay b,
      dict         = N.merge (dict a) (dict b),
      dict2        = if twoWay a && twoWay b
                       then N.merge (dict2 a) (dict2 b)
                       else N.empty
    }

-- | Update a dictionary with the associations from the given list of words.
updateDict :: Ord a => [a] -> Dictionary a -> Dictionary a
updateDict words d = d2 where
    d1 = if twoWay d
            then updateDict' (reverse words) d{dict=dict2 d, dict2=dict d}
            else d {dict = dict2 d, dict2 = dict d}
    d2 = updateDict' words d {dict = dict2 d1, dict2 = dict d1}

-- | Update only the forward dictionary using the given word list.
updateDict' :: Ord a => [a] -> Dictionary a -> Dictionary a
updateDict' words@(w:ws) d =
  updateDict' ws d {dict = N.insert (take (maxKeyLen d+1) words) (dict d)}
updateDict' _ dict        =
  dict

-- | Try to use the given key and random generator to derive a preferred length
--   key for this dictionary. If the key's length is >= the preferred key
--   length, it is returned without modification.
optKey :: Ord a
       => (Dictionary a -> N.NGram a) -- ^ Use dict or dict2?
       -> Dictionary a                   -- ^ Dictionary to work on
       -> StdGen                         -- ^ Random generator
       -> [a]                            -- ^ Key to optimize
       -> [a]
optKey whatDict dic gen key
  | length key >= preferKeyLen dic =
    key
  | otherwise =
    if isNothing mPossible || not (null possible)
       then optKey whatDict dic gen' key'
       else key
    where
      mPossible   = N.lookup key (whatDict dic)
      possible    = fromJust mPossible
      key'        = key ++ [fst $ pickOne (possible) gen]
      (idx, gen') = randomR (0, length possible-1) gen

-- | Generate text backward from the given key
disPressBack :: Ord a => [a] -> Dictionary a -> StdGen -> [a]
disPressBack key d gen = disPress' dict2 (reverse key) d gen

-- | Generate text forward from the given key
disPress :: Ord a => [a] -> Dictionary a -> StdGen -> [a]
disPress = disPress' dict

-- | Helper for disPress and disPressBack; generates text forward or backward
--   depending on if the first parameter is dict or dict2.
disPress' :: Ord a
          => (Dictionary a -> N.NGram a)
          -> [a]
          -> Dictionary a
          -> StdGen
          -> [a]
disPress' whatDict key@(w:ws) d gen =
  case word of
    Just (word', gen') ->
      w : disPress' whatDict (optKey whatDict d gen $ ws++[word']) d gen'
    _         ->
      w : disPress' whatDict ws d gen
  where
    word = do
      possible <- N.lookup key (whatDict d)
      if null possible
        then fail ""
        else return ()
      return $ pickOne possible gen
disPress' _ k _ _ = k

-- | Chooses a random item from a list of weighted items. If the list was,
--   for example, [(a, 2), (b, 10)], then b would be five times as likely
--   as a to be chosen. An item with weight <= 0 will not be chosen.
--   If all items in the list has a <= 0 weight, all items will be assigned
--   a weight of 1.
pickOne :: (Random b, Ord b, Num b) => [(a, b)] -> StdGen -> (a, StdGen)
pickOne [] _    = error "pickOne: empty list!"
pickOne items g = if null items'
                     then (pick (map (\(x, _) -> (x, 1)) items) num, g')
                     else (pick items' num, g')
  where
    items'    = filter ((> 0) . snd) items
    pick ((x, w):xs) n | n <= w    = x
                       | otherwise = pick xs (n-w)
    (num, g') = randomR (1, sum $ map snd items') g

-- | Randomly chooses a key for the map. The key uses only a single word,
--   so that it can be used properly for both forward and backward generation.
randomKey :: (Ord a, Show a) => Dictionary a -> StdGen -> [a]
randomKey dic gen = getKey (preferKeyLen dic) (dict dic) gen
  where
    getKey 0 _  _=
      []
    getKey n trie g =
      let elems        = N.childList trie
          maxWeight    = maximum $ map snd elems
          invWElems    = map (\(x, w) -> (x, maxWeight - w)) elems
          (subkey, g') = pickOne invWElems g
          next         = fromJust $ N.subNGram [subkey] trie
          in if null invWElems
                then []
                else subkey : getKey (n-1) next g'

-- | Takes a (possibly) too long key and returns a subset of the key that
--   actually exists in the dictionary. The returned subset is obtained by
--   randomly picking one of the valid subsets, weighted by inverse
--   commonality, and with an extra bias towards less common keys.
toKey :: (Show a, Ord a) => [a] -> Dictionary a -> StdGen -> [a]
toKey s d gen =
  if s `isKeyIn` d
     then s
     else if not $ null keys'
             then fst $ pickOne keys' gen
             else []
  where
    seqs = subsequences s
    -- get avg. weights for all keys, filter out the nonexistent ones
    -- we also square the weights of the keys, to give less common keys an
    -- extra bias.
    combinedWeight x = (N.weightIn x (dict d) `div` length x)
                     + (N.weightIn (reverse x) (dict2 d) `div` length x)
    keys = filter (\(_, w) -> w > 0)
         $ map (\x -> (x, combinedWeight x))
         $ filter (/= []) seqs
    
    -- get the heaviest key
    maxWeight = maximum $ map snd keys

    -- invert each key by subtracting its average weight from maxWeight.
    -- this ensures that pickOne will prefer less common keys to more common
    -- ones.
    -- we also subtract half the max weight and filter out anything that goes
    -- subzero, to make sure that the most common alternatives never get
    -- chosen.
    calcWeight w = (maxWeight - w)
    keys'        = filter (\(_, w) -> w > 0)
                 $ map (\(x, w) -> (x, calcWeight w)) keys

-- | Returns true if the given key is valid for the given dictionary; that is,
--   if it points to something.
isKeyIn :: Ord a => [a] -> Dictionary a -> Bool
k `isKeyIn` d = case N.lookup k (dict d) of 
  Nothing -> N.lookup k (dict2 d) /= Nothing
  _       -> True

-- | Generates text using a randomly selected key
randomPress :: (Show a, Ord a) => Dictionary a -> StdGen -> [a]
randomPress dic gen = disPress (randomKey dic gen) dic gen
