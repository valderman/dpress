module DissociatedPress.Core (
    Dictionary (..),
    newDict, defDict, updateDict, setPreferredKeyLength,
    disPress, disPressBack, {-randomPress,
    randomKey,-} toKey, isKeyIn, optKey, merge
  ) where
import qualified DissociatedPress.NGram as N
import Data.List
import Data.Maybe (fromJust)
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
      maxKeyLen    = kLen,
      preferKeyLen = kPref,
      twoWay       = tWay,
      dict         = d1,
      dict2        = d2
    }
  where
    kLen  = min (maxKeyLen a) (maxKeyLen b)
    kPref = min (preferKeyLen a) (preferKeyLen b)
    tWay  = twoWay a && twoWay b
    d1    = N.merge (dict a) (dict b)
    d2    = if tWay
               then N.merge (dict2 a) (dict2 b)
               else N.empty

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
  updateDict' ws d {dict = dict'}
  where
    dict' = N.insert (take (maxKeyLen d+1) words) (dict d)
updateDict' _ dict        =
  dict

-- | Try to use the given key and random generator to derive a preferred length
--   key for this dictionary.
optKey :: Ord a
       => (Dictionary a -> N.NGram a) -- ^ Use dict or dict2?
       -> Dictionary a                   -- ^ Dictionary to work on
       -> StdGen                         -- ^ Random generator
       -> [a]                            -- ^ Key to optimize
       -> [a]
optKey whatDict dic gen key
  | length key >= preferKeyLen dic =
    key
  | otherwise                      =
    case mPossible of
      Nothing -> key
      _       -> optKey whatDict dic gen' key'
    where
      key'        = key ++ [fst $ pickOne (possible) gen]
      possible    = fromJust mPossible
      mPossible   = N.lookup key (whatDict dic)
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
      return $ pickOne possible gen
disPress' _ k _ _ = k

pickOne :: (Random b, Ord b, Num b) => [(a, b)] -> StdGen -> (a, StdGen)
pickOne items g = (pick items num, g')
  where
    pick ((x, w):xs) n | n <= w    = x
                       | otherwise = pick xs (n-w)
    pick _ n                       = error $  "pick ran out of items "
                                           ++ "to choose from! (n = "
                                           ++ show n ++ ")"
    total     = sum $ map snd items
    (num, g') = randomR (1, total) g

-- | Randomly chooses a key for the map. The key uses only a single word,
--   so that it can be used properly for both forward and backward generation.
{-randomKey :: Ord a => Dictionary a -> StdGen -> [a]
randomKey dic gen = [key]
  where
    (idx, gen')   = randomR (0, 0 {-(N.size $ dict dic) - 1-}) gen
    (idx2, gen'') = randomR (0, length possible - 1) gen'
    possible      = (N.elems $ dict dic) !! idx
    key           = possible !! idx2-}

-- | Takes a (possibly) too long key and returns a subset of the key that
--   actually exists in the dictionary. The returned subset is obtained by
--   randomly picking one of the valid subsets, weighted by inverse
--   commonality, and with an extra bias towards less common keys.
toKey :: Ord a => [a] -> Dictionary a -> StdGen -> [a]
toKey s d gen =
  if s `isKeyIn` d
     then s
     else fst $ pickOne keys' gen
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
-- randomPress :: Ord a => Dictionary a -> StdGen -> [a]
-- randomPress dic gen = disPress (randomKey dic gen) dic gen
