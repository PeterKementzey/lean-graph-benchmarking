#!/usr/bin/python3

import random, subprocess

graphDirectory = "generated-graphs/"
fileExtension = ".txt"

def generateDAG (nodeCount, edgeCount, fileName, seed):
    random.seed(seed)

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

    fileHandle = open(graphDirectory + fileName + fileExtension, 'w')
    print(nodeCount, file=fileHandle)
    for pair in edges:
        print (str(pair[0]) + " " + str(pair[1]), file=fileHandle)
    fileHandle.close()

    # This part of the code prints the topological ordering that the graph is based on
    # fileHandle = open(graphDirectory + fileName + "-topsorted-sequence.txt", 'w')
    # sortedOrder = list(enumerate(nodes)); sortedOrder.sort(key = lambda x : x[1]); sortedOrder = list(map(lambda x : x[0], sortedOrder))
    # print(sortedOrder, file=fileHandle)
    # fileHandle.close()

def printControlMessage(msg):
    print("\033[96m\n\n" + msg + "\n\n\033[0m")

graphParameters = [
    # (500, 2000, "small-sparse"),
    # (500, 225000, "small-dense"),
    # (3000, 10000, "medium-very-sparse"),
    # (3000, 500000, "medium-sparse"),
    # (3000, 8500000, "medium-dense"),
    # (3000, 12000000, "medium-very-dense"),
    # (6600, 450000, "maximum-working-size") ,
    (30000, 2000000, "huge")
]


# remove all previously generated graphs first

subprocess.run("rm -f " + graphDirectory + "*" + fileExtension, shell=True)


# build generated graphs

printControlMessage("Generating graphs:")

fileNames = []

for p in graphParameters:
    for i in (range(1)):
        fileNames.append(p[2] + str(i))
        print(fileNames[-1])
        generateDAG(p[0], p[1], fileNames[-1], i)


# build lean and haskell

printControlMessage("Building Lean:")
subprocess.run("cd lean; ./build.sh ", shell=True)
printControlMessage("Building Haskell:")
subprocess.run("cd haskell; stack clean; stack build ", shell=True)


# run tests

fileNames = list(map(lambda f: "../" + graphDirectory + f + fileExtension, fileNames))

printControlMessage("Running Lean Benchmarks")
subprocess.run("cd lean; ./build/bin/Benchmark " + fileNames[-1], shell=True)
printControlMessage("Running Haskell Benchmarks")
subprocess.run("cd haskell; stack run " + fileNames[-1], shell=True)

print("\n\n")