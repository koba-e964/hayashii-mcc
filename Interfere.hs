module Interfere where

import Id (VId)

import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Interference = Map.Map VId (Set.Set VId)

empty :: Interference
empty = Map.empty

union :: Interference -> Interference -> Interference
union i1 i2 = Map.unionWith Set.union i1 i2


clique :: Set.Set VId -> Interference
clique set = Map.fromSet (\x -> set `Set.difference` Set.singleton x) set

vertices :: Interference -> [VId]
vertices = Map.keys

degree :: Interference -> VId -> Int
degree i v = Set.size (i Map.! v)

removeVertex :: Interference -> VId -> Interference
removeVertex intGr v = Map.map (Set.delete v) $ Map.delete v intGr

neighbors :: Interference -> VId -> [VId]
neighbors i v = Set.toList $ i Map.! v

-- | Trial of k-coloring using heuristics. 
-- | If this function succeeds to color the given graph, it returns the mapping of color wrapped with Right.
-- | Otherwise, it returns the set of variables not colored by this function, wrapped with Left.
tryColoring :: Interference -> Int -> Either (Set.Set VId) (Map.Map VId Int)
tryColoring intGr k = f [0 .. k - 1] intGr where
  f cols gr | null $ vertices gr = Right Map.empty
  f cols gr = let verts = vertices gr in
    let mi = List.minimumBy (compare `on` degree gr) verts in
    let induced = removeVertex gr mi in
    let res = f cols induced in
    case res of
      Left set -> Left set
      Right sub -> let nb = neighbors gr mi in
        let used = map (sub Map.!) nb in
        let remainder = cols List.\\ used in
        if null remainder then
          Left (Set.fromList verts) {- no colors are remaining -}
        else
          Right $ Map.insert mi (head remainder) sub

