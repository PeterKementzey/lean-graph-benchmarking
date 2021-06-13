import Graph

def g : Graph Nat Nat := (Graph.empty.addVertex 0).1

def main : IO Unit :=
  IO.println g
