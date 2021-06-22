import Graph.Graph
import Graph.Parser
import Graph.TopologicalSort


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


/-- This function ensures that the expression does not get dropped by compiler optimizations. -/
def evaluate [ToString α] (expression : α) : IO Unit := IO.FS.writeFile "/dev/null" (toString expression)

def benchmarkParsing (filePath : String) : IO (Graph Bool Nat) := do
  let initial <- IO.monoMsNow
  let input <- IO.FS.lines filePath
  let (nodeCount, edgeList) := parseEdgeList input
  let start <- IO.monoMsNow
  let graph <- parse nodeCount edgeList
  let stop <- IO.monoMsNow
  IO.println ("Read from file and converted to nat in: " ++ (toString (start - initial)) ++ " ms")
  IO.println ("Parsed graph in: " ++ (toString (stop - start)) ++ " ms")
  graph

def benchmarkTopSort (graph : Graph Bool Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.topSortUnsafe
  let stop <- IO.monoMsNow
  IO.println ("Sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res

def benchmarkTopSortSafe (graph : Graph Bool Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.topSort
  let stop <- IO.monoMsNow
  IO.println ("Safely sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res


def main (argv : List String) : IO Unit := do

  let filePath := huge
  
  let graph <- benchmarkParsing filePath
  benchmarkTopSort graph
  benchmarkTopSortSafe graph

  
  