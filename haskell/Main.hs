module Main where

import Data.Graph
import System.IO
import Criterion.Measurement
import Control.DeepSeq
import Control.Exception

graphFromTutorial :: Graph
nodeFromVertex :: Vertex -> ([Char], Char, [Char])
vertexFromKey :: Char -> Maybe Vertex
(graphFromTutorial, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]

exampleGraph8 :: Graph
(exampleGraph8, _, _) = graphFromEdges [
    (0, 0, [1]),
    (1,1,[2,5,4,7]),
    (2,2,[3,6]),
    (3,3,[9]),
    (4,4,[]),
    (5,5,[8]),
    (6,6,[8,9]),
    (7,7,[8]),
    (8,8,[]),
    (9,9,[])
    ]



createGraph :: (Vertex, [Edge]) -> Graph
createGraph (nodeCount, edgeList) =
    let bounds = (0, nodeCount-1) in
    buildG bounds edgeList

parseEdgeList :: [String] -> [(Int, Int)]
parseEdgeList (edge:edgeList) = let pair = words edge in (read (head pair), read (pair !! 1)) : parseEdgeList edgeList
parseEdgeList [] = []

parseLines :: [String] -> (Int, [(Int, Int)])
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

filePath = huge

parseGraphTest :: IO Int
parseGraphTest = do
    graph <- parseFile filePath
    return (seq graph 0)

runTest :: IO Int
runTest = do
    graph <- parseFile filePath
    return (seq (topSort graph) 7)

-- main = secs <$> time_ someIOFunction >>= print

main :: IO ()
main = do
    graph <- parseFile filePath
    evaluate (rnf graph)
    putStrLn filePath
    initializeTime
    startTime <- getTime
    let sorted = deepseq (topSort graph) (topSort graph)
    print (last sorted)
    endTime <- getTime
    putStrLn (secs (endTime - startTime))

-- main = defaultMain [
--   bgroup "topsort" [ bench "parse graph"  $ nfIO parseGraphTest
--                ],
--   bgroup "topsort" [ bench "parse and topsort"  $ nfIO runTest
--                ]
--   ]
