#!/usr/bin/python3

import random, sys

def generateDAG (nodeCount, edgeCount, fileName):
    # nodeCount = 20
    # edgeCount = 200

    nodes = list(range(nodeCount))

    random.shuffle(nodes) # create a random order for the nodes, this will be one possible result for the topological sorting and this will guarantee that there are no cycles

    # print(nodes)

    def getPlaceInOrder(x): return nodes[x]

    def swapTuple(t): return (t[1], t[0])

    edges = []

    for i in range(edgeCount):
        newPair = (random.randint(0,nodeCount-1), random.randint(0,nodeCount-1))
        while newPair[0] == newPair[1]:
            newPair = (random.randint(0,nodeCount-1), random.randint(0,nodeCount-1))
        orderedPair = newPair if getPlaceInOrder(newPair[0]) <= getPlaceInOrder(newPair[1]) else swapTuple(newPair)
        edges.append(orderedPair)

    sys.stdout = open(fileName, 'w')

    for pair in edges:
        print (str(pair[0]) + " " + str(pair[1]))
    
    sys.stdout.close()
    
    sys.stdout = open(fileName + "-original-sequence.txt", 'w')
    sortedOrder = list(enumerate(nodes)); sortedOrder.sort(key = lambda x : x[1]); sortedOrder = list(map(lambda x : x[0], sortedOrder))
    print(sortedOrder)
    sys.stdout.close()



generateDAG(20, 10000, "topsort-gen.txt")