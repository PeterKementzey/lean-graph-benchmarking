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
    # (500, 2000, "small-sparse"), # too small
    (500, 225000, "small-dense"),
    # (3000, 10000, "medium-very-sparse"), # too small
    (3000, 500000, "medium-sparse"),
    # (3000, 8500000, "medium-dense"),
    # (3000, 12000000, "medium-very-dense"),
    # (6600, 450000, "maximum-working-size") ,
    # (30000, 2000000, "huge"),
    # (70000, 7000000, "superhuge")
]
iterationCount = 3
graphMulitiplicationCount = 1

# 
# generate graphs
# 

# remove all previously generated graphs first

subprocess.run("rm -f " + graphDirectory + "*" + fileExtension, shell=True)

printControlMessage("Generating graphs:")

fileNames = []

for p in graphParameters:
    for i in (range(graphMulitiplicationCount)):
        fileNames.append(p[2] + str(i))
        print(fileNames[-1])
        generateDAG(p[0], p[1], fileNames[-1], i)


# 
# build lean and haskell
# 

printControlMessage("Building Lean:")
# subprocess.run("cd lean; ./build.sh ", shell=True)
printControlMessage("Building Haskell:")
subprocess.run("cd haskell; stack build ", shell=True)


# 
# run tests
# 

subprocess.run("cd lean/res; rm -f *; touch .keep ", shell=True)
subprocess.run("cd haskell/res; rm -f *; touch .keep ", shell=True)

relativeFileNames = list(map(lambda f: "../" + graphDirectory + f + fileExtension, fileNames))
resultsFolder = "res/"

printControlMessage("Running Benchmarks:")
for (fileName, relativeFileName) in zip(fileNames, relativeFileNames):
    print(fileName)
    for i in range(iterationCount):
        subprocess.run("cd lean; ./build/bin/Benchmark " + relativeFileName + " >> " + resultsFolder + fileName + fileExtension, shell=True)
        subprocess.run("cd haskell; stack run " + relativeFileName + " >> " + resultsFolder + fileName + fileExtension, shell=True)

print("\n\n")

# 
# compile results
#

def parseResults(fileLocation):
    fileHandle = open(fileLocation, 'r')
    fileLines = []
    for line in fileHandle:
        if "μs" in line:
            # raise Exception("Result is too small, below 1 ms: '" + line.strip('\n') + "' in " + fileLocation) # print filename
            print("Result is too small, below 1 ms: '" + line.strip('\n') + "' in " + fileLocation) # print filename
        elif "ms" in line:
            temp = line.strip(" ms\n")
            fileLines.append(int(float(temp)))
        elif "s" in line:
            temp = line.strip(" s\n")
            fileLines.append(int(float(temp) * 1000)) # convert to ms
        else:
            raise Exception("Time unit not found: " + line)
    fileHandle.close()
    # print(fileLines)
    return fileLines

printControlMessage("Compiling results:")

# don't forget lean has safe and unsafe

leanResultFileLocations = list(map(lambda f: "lean/" + resultsFolder + f + fileExtension, fileNames))
haskellResultFileLocations = list(map(lambda f: "haskell/" + resultsFolder + f + fileExtension, fileNames))
haskellResultSaveLocations = list(map(lambda f: resultsFolder + "haskell-" + f + fileExtension, fileNames))
leanUnsafeResultSaveLocations = list(map(lambda f: resultsFolder + "lean-unsafe-" + f + fileExtension, fileNames))
leanSafeResultSaveLocations = list(map(lambda f: resultsFolder + "lean-safe-" + f + fileExtension, fileNames))

print("Lean:")
for (fileName, fileLocation, unsafeSaveLocation, safeSaveLocation) in zip(fileNames, leanResultFileLocations, leanUnsafeResultSaveLocations, leanSafeResultSaveLocations):
    print(fileName)
    res = parseResults(fileLocation)
    safe = []
    unsafe = []
    for i in range(len(res)):
        if i % 2 == 0: unsafe.append(res[i])
        else: safe.append(res[i])
    
    # print("unsafe:")
    # print("mean: " +  str(statistics.mean(unsafe)))
    # print("standard deviation: " +  str(statistics.stdev(unsafe)))
    # print("safe:")
    # print("mean: " +  str(statistics.mean(safe)))
    # print("standard deviation: " +  str(statistics.stdev(safe)) + '\n')

print("\nHaskell:")
for (fileName, fileLocation, saveLocation) in zip(fileNames, haskellResultFileLocations, haskellResultSaveLocations):
    print(fileName)
    res = parseResults(fileLocation)
    # print("mean: " +  str(statistics.mean(res)))
    # print("standard deviation: " +  str(statistics.stdev(res)) + '\n')

    fileHandle = open(saveLocation, 'w')
    print(res, file=fileHandle)
    fileHandle.close()


