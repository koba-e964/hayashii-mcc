module Graph where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | The type of graph whose vertices are of type n.
type Graph n = Map.Map n (Set.Set n)

empty :: Graph n
empty = Map.empty

union :: (Eq n, Ord n) => Graph n -> Graph n -> Graph n
union i1 i2 = Map.unionWith Set.union i1 i2


clique :: (Eq n, Ord n) => Set.Set n -> Graph n
clique set = Map.fromSet (\x -> set `Set.difference` Set.singleton x) set

vertices :: (Eq n, Ord n) => Graph n -> [n]
vertices = Map.keys

degree :: (Eq n, Ord n) => Graph n -> n -> Int
degree i v = Set.size (i Map.! v)

removeVertex :: (Eq n, Ord n) => Graph n -> n -> Graph n
removeVertex intGr v = Map.map (Set.delete v) $ Map.delete v intGr

neighbors :: (Eq n, Ord n) => Graph n -> n -> [n]
neighbors i v = Set.toList $ i Map.! v

