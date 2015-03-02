module Interfere where

import Id (VId)

import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Graph

type Interference = Graph VId

-- | Trial of k-coloring using heuristics.
-- | If this function succeeds to color the given graph, it returns the mapping of color wrapped with Right.
-- | Otherwise, it returns the set of variables not colored by this function, wrapped with Left.
-- | We can specify the color of some vertices.
-- | Besides, for each vertex v, we can specify the set of colors in which v's color is selected.
tryColoring :: Interference -> Map.Map VId [Int] -> Map.Map VId Int -> Either (Set.Set VId) (Map.Map VId Int)
tryColoring intGr colset precol = f intGr where
  precols = Map.keys precol -- the list of colored vertices in precol
  f gr | null (vertices gr List.\\ precols) = Right precol
  f gr = let verts = vertices gr in
    let mi = List.minimumBy (compare `on` degree gr) (verts List.\\ precols) in
    let induced = removeVertex gr mi in
    let res = f induced in
    case res of
      Left set -> Left set
      Right sub -> let nb = neighbors gr mi in
        let used = map (sub Map.!) nb in
        let remainder = (colset Map.! mi) List.\\ used in
        case remainder of
          [] -> Left (Set.fromList verts) {- no colors are remaining -}
          col : _ -> Right $ Map.insert mi col sub

