#!/usr/bin/python3

import random, subprocess, statistics

graphDirectory = "generated-graphs/"
fileExtension = ".txt"

graphParameters = [
    (1500, 700000, "sparse"),
    (1500, 2000000, "dense"),
    (1500, 3000000, "very-dense"),
    (20000, 4000000, "large"),
]
iterationCount = 5
graphMulitiplicationCount = 5

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
subprocess.run("cd lean; chmod +x build.sh; ./build.sh ", shell=True)
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
print("This might take a while...\n")
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
            raise Exception("Result is too small, below 1 ms: '" + line.strip('\n') + "' in " + fileLocation)
            # print("Result is too small, below 1 ms: '" + line.strip('\n') + "' in " + fileLocation)
        elif "ms" in line:
            temp = line.strip(" ms\n")
            fileLines.append(int(float(temp)))
        elif "s" in line:
            temp = line.strip(" s\n")
            fileLines.append(int(float(temp) * 1000)) # convert to ms
        else:
            raise Exception("Time unit not found: " + line)
    fileHandle.close()
    return fileLines

def arrayToCSVLine(array):
    res = ""
    for x in array:
        res = res + str(x) + ','
    res = res[:-1]
    return res


printControlMessage("Compiling results:")

leanResultFileLocations = list(map(lambda f: "lean/" + resultsFolder + f + fileExtension, fileNames))
haskellResultFileLocations = list(map(lambda f: "haskell/" + resultsFolder + f + fileExtension, fileNames))

resultsNames = []
resultsUnsafe = []
resultsSafe = []
resultsHaskell = []

sameGraphCounter = 0
safe = []
unsafe = []
print("Lean:")
for (fileName, fileLocation) in zip(fileNames, leanResultFileLocations):
    print(fileName)
    res = parseResults(fileLocation)
    for i in range(len(res)):
        if i % 2 == 0: unsafe.append(res[i])
        else: safe.append(res[i])

    sameGraphCounter += 1
    if sameGraphCounter % graphMulitiplicationCount == 0:
        resultsNames.append(fileName[:-1])
        resultsUnsafe.append(statistics.mean(unsafe))
        resultsSafe.append(statistics.mean(safe))
        safe = []
        unsafe = []


res = []
sameGraphCounter = 0
print("\nHaskell:")
for (fileName, fileLocation) in zip(fileNames, haskellResultFileLocations):
    print(fileName)
    res = res + parseResults(fileLocation)

    sameGraphCounter += 1
    if sameGraphCounter % graphMulitiplicationCount == 0:
        resultsHaskell.append(statistics.mean(res))
        res = []

# 
# Save to CVS file
# 

printControlMessage("Saving means to results.csv")

fileHandle = open("results.csv", 'w')

for name in resultsNames: fileHandle.write(',' + name)
fileHandle.write("\nLean unsafe,")
fileHandle.write(arrayToCSVLine(resultsUnsafe))
fileHandle.write("\nLean safe,")
fileHandle.write(arrayToCSVLine(resultsSafe))
fileHandle.write("\nHaskell,")
fileHandle.write(arrayToCSVLine(resultsHaskell))
fileHandle.write("\n")

fileHandle.close()

printControlMessage("Done")
