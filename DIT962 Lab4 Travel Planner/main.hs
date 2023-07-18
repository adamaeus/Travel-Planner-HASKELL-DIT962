import Route
import RouteGUI
import Graph

import Dijkstras
import qualified Data.Map as M
import Data.Maybe ()
import qualified Data.PSQueue as PSQ

import System.Environment(getArgs)


-- | Main method for finding the shortest path from two nodes "a" to another "a" node. Calls on 
-- | dijkstras function. 
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph from to = case M.lookup to shortestPaths of
  Nothing -> Nothing
  Just (cost,_) -> Just  (buildPath to [], cost)
  where
    shortestPaths = dijkstra from graph (PSQ.insert (from, from) 0 PSQ.empty) M.empty
    buildPath node path
      | node == from = node : path
      | otherwise = buildPath prevNode (node : path)
      where
        (_, prevNode) = shortestPaths M.! node



-- | Function to use the whole program. Call to four arguments, two files and two stops
main :: IO ()
main = do
  (s1:s2:s3:s4:_) <- getArgs

  -- Read stops and lines from files
  Right stops <- readStops s1
  Right lines <- readLines s2
        
  -- Generate graph and find the shortest path
  let graph = generateGraph stops lines
      path = shortestPath graph s3 s4
        
  -- Print the result
  case path of
    Nothing -> return ()
    Just (list, time) -> do
      print time
      putStr $ unlines list



-- | The only thing we do is to apply our functions to generate a Graph, and then change text file
-- | inputs accordingly.
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "stops-air.txt" --"stops-gbg.txt"
  Right lines <- readLines "lines-air.txt"--"lines-gbg.txt"
  let graph = generateGraph stops lines
  runGUI stops lines graph shortestPath

  


-- | Main function to generate a Graph. Calls on helper functions to add nodes and edges to the Graph
-- | with given "stops" read from textfiles.
generateGraph :: [Stop] -> [LineTable] -> Graph String Integer
generateGraph stops lineTables = addEdgesToGraph lineTables (createGraph stops)
  
-- | Add nodes to a graph, given their stops as "names".
createGraph :: [Stop] -> Graph String Integer
createGraph stops = addVertices (assistStopNames stops) Graph.empty
  
-- | Reads the name from the "stop", returns the names as list of String.
assistStopNames :: [Stop] -> [String]
assistStopNames stops = map getName stops
  where getName (Stop name _) = name
  
-- | Adds paths (edges) to the graph between nodes. Read from LineTable as the "time" from one stop
-- | to another.
addEdgesToGraph :: [LineTable] -> Graph String Integer -> Graph String Integer
addEdgesToGraph lineTables graph = foldr assistAddEdge graph (concatMap assistStopPairs lineTables)
  

-- | Helper function to extract a list of tuples, made of stops. Helps to add edge between the two
-- | tuple elements.
assistStopPairs :: LineTable -> [(LineStop, LineStop)]
assistStopPairs (LineTable _ stops) = zip stops (tail stops)
  

-- | Helper function to add edge between two stops (nodes).
assistAddEdge :: (LineStop, LineStop) -> Graph String Integer -> Graph String Integer
assistAddEdge ((LineStop from _), (LineStop to weight)) graph =
  addEdge from to weight graph



