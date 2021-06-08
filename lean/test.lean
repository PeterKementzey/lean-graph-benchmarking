import Graph.Parser
import Graph.TopologicalSort

def e : Graph Nat Nat := (Graph.empty.addVertex 0).1

def bitcoinSize := "../generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
def maximumSize := "../generated-graphs/maximum-working-size-topsort-gen.txt"
def mediumDense := "../generated-graphs/medium-dense-topsort-gen.txt"
def mediumVeryDense := "../generated-graphs/medium-very-dense-topsort-gen.txt"
def mediumSparse := "../generated-graphs/medium-sparse-topsort-gen.txt"
def mediumVerySparse := "../generated-graphs/medium-very-sparse-topsort-gen.txt"
def smallDense := "../generated-graphs/small-dense-topsort-gen.txt"
def smallSparse := "../generated-graphs/small-sparse-topsort-gen.txt"

def main : IO Unit := do

  let filePath := maximumSize

  let graph <- parseGraphFromEdgeList filePath
  -- IO.println (graph)
  -- IO.println (graph.depthFirstTraversalOrderWithLeaving 0)

  let sorted := graph.topSort
  IO.println (sorted.get![0])