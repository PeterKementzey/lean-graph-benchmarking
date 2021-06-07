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

-- Note: can use command line arguments I think like this: (args !! 0)
main :: IO ()
main = do
    
    
    print (topSort exampleGraph8)

-- main = defaultMain [
--   bgroup "fib" [ bench "1"  $ whnf show (topSort exampleGraph8)
--                ]
--   ]