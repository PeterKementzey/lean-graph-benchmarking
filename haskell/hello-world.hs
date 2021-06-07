import Data.Graph
import System.IO
-- import Criterion.Main

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


testGraph :: [Char]
testGraph = "../generated-graphs/test-topsort-gen.txt"

createGraph :: Vertex -> [Edge] -> Graph
createGraph nodeCount edgeList =
    let bounds = (0, nodeCount-1) in
    buildG bounds edgeList


-- Note: can use command line arguments I think like this: (args !! 0)
main :: IO ()
main =
    let nodeCount = 10 in
    let edgeList = [(4, 0), (9, 8), (2, 8), (5, 0), (4, 8), (5, 2), (6, 3), (1, 3)] in
    -- let filePath = testGraph
    -- contents <- readFile filePath
    -- print contents
    -- let linesOfFile = lines contents
    -- print linesOfFile
    
    print (topSort (createGraph nodeCount edgeList))

-- main = defaultMain [
--   bgroup "fib" [ bench "1"  $ whnf show (topSort exampleGraph8)
--                ]
--   ]