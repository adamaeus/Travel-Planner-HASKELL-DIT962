{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Graph 
  ( -- * Edge
    Edge (..)                   -- type
                               -- querying an Edge

    -- * Graph
  , Graph                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source vertex
  , dst   :: a  -- ^ Destination vertex
  , label :: b  -- ^ The label
  } deriving Show

-- A graph with nodes of type a and labels of type b.
data Graph a b = Graph (M.Map a [Edge a b]) deriving Show
  

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v (Graph g)  
  | v `M.member` g = Graph g
  | otherwise = Graph $ M.insert v [] g


-- | Add a list of vertices to a graph
-- Note, skapar ingen lista, behöver en lista som input.
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l (Graph map) = Graph $ M.insertWith (++) v [Edge v w l] map

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l map = addEdge v w l $ addEdge w v l map

-- | Get all adjacent vertices (nodes) for a given node
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v (Graph g) =
  case M.lookup v g of
    Just listOfEdges -> listOfEdges
    Nothing         -> []

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices (Graph g) = M.keys g

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges (Graph g) = concat $ M.elems g






