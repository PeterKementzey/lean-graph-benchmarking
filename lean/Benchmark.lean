import Graph.Graph
import Graph.Parser
import Graph.TopologicalSort
import Graph.Search

/-- This function ensures that the expression does not get dropped by compiler optimizations. -/
def evaluate [ToString α] (expression : α) : IO Unit := IO.FS.writeFile "/dev/null" (toString expression)

def benchmarkParsing (filePath : String) : IO (Graph Bool Nat) := do
  let initial <- IO.monoMsNow
  let input <- IO.FS.lines filePath
  let (nodeCount, edgeList) := parseEdgeList input
  let start <- IO.monoMsNow
  let graph <- parse nodeCount edgeList
  let stop <- IO.monoMsNow
  -- IO.println ("Read from file and converted to nat in: " ++ (toString (start - initial)) ++ " ms")
  -- IO.println ("Parsed graph in: " ++ (toString (stop - start)) ++ " ms")
  graph

def benchmarkTopSort (graph : Graph Bool Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.topSortUnsafe
  let stop <- IO.monoMsNow
  IO.println ((toString (stop - start)) ++ " ms")
  -- IO.println ("Sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res.back

def benchmarkTopSortSafe (graph : Graph Bool Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.topSort
  let stop <- IO.monoMsNow
  IO.println ((toString (stop - start)) ++ " ms")
  -- IO.println ("Safely sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res.get!.back

def benchmarkReachableDepthFirst (graph : Graph Bool Nat) (source : Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.reachableDepthFirst source
  let stop <- IO.monoMsNow
  IO.println ((toString (stop - start)) ++ " ms")
  -- IO.println ("Safely sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res.back

def benchmarkReachableBreadthFirst (graph : Graph Bool Nat) (source : Nat) : IO Unit := do
  let start <- IO.monoMsNow
  let res <- graph.reachableBreadthFirst source
  let stop <- IO.monoMsNow
  IO.println ((toString (stop - start)) ++ " ms")
  -- IO.println ("Safely sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  evaluate res.back

def main (argv : List String) : IO Unit := do

  let filePath := argv.head!
  let benchmark := argv.get! 1
  
  let graph <- benchmarkParsing filePath

  match benchmark with
    | "topsort" =>
      benchmarkTopSort graph
      benchmarkTopSortSafe graph
    | "reachable" =>
      benchmarkReachableDepthFirst graph 3
      benchmarkReachableBreadthFirst graph 3
    | _ => panic! "Benchmark type not recognized"

