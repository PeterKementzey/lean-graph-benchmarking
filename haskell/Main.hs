module Main where

import Data.Graph
import System.IO
import Criterion.Measurement
import Control.DeepSeq
import Control.Exception

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


bitcoinSize = "../generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
maximumSize = "../generated-graphs/maximum-working-size-topsort-gen.txt"
mediumDense = "../generated-graphs/medium-dense-topsort-gen.txt"
mediumVeryDense = "../generated-graphs/medium-very-dense-topsort-gen.txt"
mediumSparse = "../generated-graphs/medium-sparse-topsort-gen.txt"
mediumVerySparse = "../generated-graphs/medium-very-sparse-topsort-gen.txt"
smallDense = "../generated-graphs/small-dense-topsort-gen.txt"
smallSparse = "../generated-graphs/small-sparse-topsort-gen.txt"
testGraph :: [Char]
testGraph = "../generated-graphs/test-topsort-gen.txt"
huge = "../generated-graphs/huge-topsort-gen.txt"

-- Note: can use command line arguments I think like this: (args !! 0)

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
    let sorted = topSort graph -- deepseq (topSort graph) (topSort graph)
    evaluate (rnf sorted)
    endTime <- getTime
    putStrLn ("Sorted in: " ++ secs (endTime - startTime))


main :: IO ()
main = do
    let filePath = huge
    initializeTime
    graph <- benchmarkParsing filePath
    benchmarkTopSort graph