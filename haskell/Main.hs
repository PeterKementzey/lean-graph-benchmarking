module Main where

import Data.Graph ( buildG, topSort, Edge, Graph, Vertex )
import System.IO ()
import System.Environment ( getArgs )
import Criterion.Measurement ( getTime, initializeTime, secs )
import Control.DeepSeq ( NFData(rnf) )
import Control.Exception ( evaluate )

createGraph :: (Vertex, [Edge]) -> Graph
createGraph (nodeCount, edgeList) =
    let bounds = (0, nodeCount-1) in
    buildG bounds edgeList

parseEdgeList :: [String] -> [(Int, Int)]
parseEdgeList (edge:edgeList) = let pair = words edge in (read (head pair), read (pair !! 1)) : parseEdgeList edgeList
parseEdgeList [] = []

parseLines :: [String] -> (Vertex, [Edge])
parseLines (nodeCount:edgeList) = (read nodeCount, parseEdgeList edgeList)
parseLines [] = error "Cannot parse empty file"

parseFile :: FilePath -> IO Graph
parseFile filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
    let parsed = parseLines linesOfFile
    return (createGraph parsed)

benchmarkParsing :: FilePath -> IO Graph
benchmarkParsing filePath = do
    initialTime <- getTime
    contents <- readFile filePath
    let linesOfFile = lines contents
    let parsed = parseLines linesOfFile
    evaluate (rnf parsed)
    startTime <- getTime
    let graph = createGraph parsed
    evaluate (rnf graph)
    endTme <- getTime
    putStrLn ("Read from file and converted to int in: " ++ secs (startTime - initialTime))
    putStrLn ("Parsed graph in:  " ++ secs (endTme - startTime))
    return graph

benchmarkTopSort :: Graph -> IO ()
benchmarkTopSort graph = do
    startTime <- getTime
    let sorted = topSort graph
    evaluate (rnf sorted)
    endTime <- getTime
    putStrLn ("Sorted in: " ++ secs (endTime - startTime))


main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    initializeTime
    graph <- benchmarkParsing filePath
    benchmarkTopSort graph