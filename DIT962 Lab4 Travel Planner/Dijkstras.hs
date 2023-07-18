module Dijkstras
(dijkstra
)
where
import Route
import Graph 
import Data.Map (Map)
import qualified Data.Map as M
import Data.PSQueue as PSQ
    ( insert, key, minView, null, prio, empty ,Binding, PSQ )
import Data.Maybe



-- | Implementation of dijkstras algorithm, using a Map as a "Visited-List", a priority Queue for 
-- | path/edge traversal, and a Graph to operate on. When priority Queue is empty, the function
-- | terminates and all edges (paths) have been analyzed.
dijkstra :: (Ord a, Ord b, Num b) => a -> Graph a b -> PSQ (a, a) b -> Map a (b, a) -> Map a (b, a)
dijkstra node graph queue map
  | PSQ.null queue = map                                     
  | M.member (dst min) map   = dijkstra node graph rest map 
  | otherwise = dijkstra node graph queue' map'
    where
      map' = M.insert (dst min) (label min, src min) map
      adjacents = adj (dst min) graph
      (minBinding, rest) = fromJust (PSQ.minView queue)
      queue' = insertToPQ adjacents (label min) rest
      min = fromBinding minBinding



-- | Helper function for "dijkstra", binding is the pair of a key and a priority, this function
-- | extracts the key.
fromBinding :: Binding (a, a) b -> Edge a b
fromBinding b = Edge src dst (prio b)
  where
    (src, dst) = key b

-- | Function for inserting an edge into our priority Queue, edge is taken from a list of edges
-- | given by the function "adj" in Graph.
insertToPQ :: (Ord a, Ord b, Num b) => [Edge a b] -> b -> PSQ (a, a) b -> PSQ (a, a) b
insertToPQ [] accCost psq = psq
insertToPQ (edge:edges) accCost psq = insertToPQ edges accCost (PSQ.insert (src edge, dst edge) (label edge + accCost) psq)












