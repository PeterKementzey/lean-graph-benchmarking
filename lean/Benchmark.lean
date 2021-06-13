import Graph

import Graph.ExampleGraphs
import Graph.TraverseExample


def euMails := "../stanford-graphs/email-EuAll.txt"
def gnutella8 := "../stanford-graphs/p2p-Gnutella08.txt"


def bitcoinSize := "../generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
def maximumSize := "../generated-graphs/maximum-working-size-topsort-gen.txt"
def mediumDense := "../generated-graphs/medium-dense-topsort-gen.txt"
def mediumVeryDense := "../generated-graphs/medium-very-dense-topsort-gen.txt"
def mediumSparse := "../generated-graphs/medium-sparse-topsort-gen.txt"
def mediumVerySparse := "../generated-graphs/medium-very-sparse-topsort-gen.txt"
def smallDense := "../generated-graphs/small-dense-topsort-gen.txt"
def smallSparse := "../generated-graphs/small-sparse-topsort-gen.txt"
def huge := "../generated-graphs/huge-topsort-gen.txt"
def testGraph := "../generated-graphs/test-topsort-gen.txt"

-- def main (argv : List String) : IO Unit := do

--   let filePath := huge

--   let graph <- parseGraphFromEdgeListFile filePath
--   let res <- graph.breadthFirstCompleteTraversalOrder5
--   -- let res <- exampleGraph3.breadthFirstCompleteTraversalOrder4
--   -- let res <- exampleLineGraph.depthFirstTraversalOrderWithLeaving4
--   -- let res <- exampleGraph3.getAllVertexIDs
--   -- let res <- graph.topSortUnsafe
--   -- let res <- graph.topSort
--   -- let res <- graph.dijkstraWithTarget 8717 27573

--   IO.println (res)
  

def main (argv : List String) : IO Unit := do

  let filePath := huge

  let graph <- parseGraphFromEdgeListFile filePath
  -- IO.println graph.vertices.back.payload
  -- IO.println "Parsed graph"
  let start <- IO.monoMsNow
  -- let res <- graph.topSortUnsafe
  let res <- graph.breadthFirstCompleteTraversalOrder
  -- let res <- graph.topSort
  let stop <- IO.monoMsNow
  IO.println ("Sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  IO.println (res.back)
  -- IO.println (res.get!.back)
  