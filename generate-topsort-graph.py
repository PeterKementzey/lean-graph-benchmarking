#!/usr/bin/python3

import random, sys

def generateDAG (nodeCount, edgeCount, fileName):
    nodes = list(range(nodeCount))

    random.shuffle(nodes) # create a random order for the nodes, this will be one possible result for the topological sorting and this will guarantee that there are no cycles

    def getPosition(x): return nodes[x]
    def swapTuple(t): return (t[1], t[0])

    edges = []

    for i in range(edgeCount):
        newPair = (random.randint(0,nodeCount-1), random.randint(0,nodeCount-1))
        while newPair[0] == newPair[1]:
            newPair = (random.randint(0,nodeCount-1), random.randint(0,nodeCount-1))
        orderedPair = newPair if getPosition(newPair[0]) <= getPosition(newPair[1]) else swapTuple(newPair)
        edges.append(orderedPair)

    sys.stdout = open(fileName + "-topsort-gen.txt", 'w')
    print(nodeCount)
    for pair in edges:
        print (str(pair[0]) + " " + str(pair[1]))
    sys.stdout.close()

    sys.stdout = open(fileName + "-topsorted-sequence.txt", 'w')
    sortedOrder = list(enumerate(nodes)); sortedOrder.sort(key = lambda x : x[1]); sortedOrder = list(map(lambda x : x[0], sortedOrder))
    print(sortedOrder)
    sys.stdout.close()

# TODO use seed to generate predictable graph

# bitcoinSize = generateDAG(5881, 35592, "generated-graphs/stanford-bitcoin-sized")
maximumSize = generateDAG(6600, 450000, "generated-graphs/maximum-working-size") # these values seem to be close to the max the system can handle with crashes not happening too often
# mediumDense = generateDAG(3000, 8500000, "generated-graphs/medium-dense")
# mediumVeryDense = generateDAG(3000, 12000000, "generated-graphs/medium-very-dense")
# mediumSparse = generateDAG(3000, 500000, "generated-graphs/medium-sparse")
# mediumVerySparse = generateDAG(3000, 10000, "generated-graphs/medium-very-sparse")
# smallDense = generateDAG(500, 225000, "generated-graphs/small-dense")
# smallSparse = generateDAG(500, 2000, "generated-graphs/small-sparse")

# testGraph = generateDAG(10, 8, "generated-graphs/test")
