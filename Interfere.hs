module Interfere where

import Id (VId)

import qualified Data.Map as Map
import qualified Data.Set as Set

type Interference = Map.Map VId (Set.Set VId)

empty :: Interference
empty = Map.empty

union :: Interference -> Interference -> Interference
union i1 i2 = Map.unionWith Set.union i1 i2


clique :: Set.Set VId -> Interference
clique set = Map.fromSet (\x -> set `Set.difference` Set.singleton x) set


