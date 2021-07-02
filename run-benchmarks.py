#!/usr/bin/python3

import random, subprocess, statistics

graphDirectory = "generated-graphs/"
fileExtension = ".txt"

graphParameters = [
    (2000, 1000000, "2K-1M"),
    (2000, 4000000, "2K-4M"),
    (20000, 1000000, "20K-1M"),
    (20000, 4000000, "20K-4M"),
    (20000, 7000000, "20K-7M"),
    (30000, 7000000, "30K-7M"),
]
stanfordFileNames = [
    "email-EuAll",
    "web-NotreDame",
    "web-Stanford",
    "web-BerkStan",
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

    # This part of the code saves the topological ordering that the graph is based on
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
printControlMessage("Running Benchmarks: ")
print("This might take a while...\n")

resultsFolder = "res/"

def runBenchmarks(fileNames, relativeFileNames, benchmark):

    subprocess.run("cd lean/res; rm -f *." + benchmark + ".txt; touch .keep ", shell=True)
    subprocess.run("cd haskell/res; rm -f *." + benchmark + ".txt; touch .keep ", shell=True)

    print(benchmark + ":\n")
    for (fileName, relativeFileName) in zip(fileNames, relativeFileNames):
        print(fileName)
        for i in range(iterationCount):
            subprocess.run("cd lean; ./build/bin/Benchmark " + relativeFileName + " " + benchmark + " >> " + resultsFolder + fileName + "." + benchmark + fileExtension, shell=True)
            subprocess.run("cd haskell; stack run " + relativeFileName + " " + benchmark + " >> " + resultsFolder + fileName + "." + benchmark + fileExtension, shell=True)

relativeFileNames = list(map(lambda f: "../" + graphDirectory + f + fileExtension, fileNames))
stanfordrelativeFileNames = list(map(lambda f: "../" + "stanford-networks/" + f + ".txt", stanfordFileNames))

runBenchmarks(fileNames, relativeFileNames, "topsort")
print()
runBenchmarks(stanfordFileNames, stanfordrelativeFileNames, "reachable")

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
            asInt = int(float(temp))
            if asInt < 15: raise Exception("Result is too small: '" + line.strip('\n') + "' in " + fileLocation)
            fileLines.append(asInt)
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


printControlMessage("Compiling results")

# Topsort

leanTopSortResultFileLocations = list(map(lambda f: "lean/" + resultsFolder + f + "." + "topsort" + fileExtension, fileNames))
haskellTopSortResultFileLocations = list(map(lambda f: "haskell/" + resultsFolder + f + "." + "topsort" + fileExtension, fileNames))

resultsUnsafe = []
resultsSafe = []
resultsHaskellTopSort = []

sameGraphCounter = 0
safe = []
unsafe = []
# print("Topsort\nLean:")
for fileLocation in leanTopSortResultFileLocations:
    # print(fileName)
    res = parseResults(fileLocation)
    for i in range(len(res)):
        if i % 2 == 0: unsafe.append(res[i])
        else: safe.append(res[i])

    sameGraphCounter += 1
    if sameGraphCounter % graphMulitiplicationCount == 0:
        resultsUnsafe.append(statistics.mean(unsafe))
        resultsSafe.append(statistics.mean(safe))
        safe = []
        unsafe = []


res = []
sameGraphCounter = 0
# print("\nHaskell:")
for fileLocation in haskellTopSortResultFileLocations:
    # print(fileName)
    res = res + parseResults(fileLocation)

    sameGraphCounter += 1
    if sameGraphCounter % graphMulitiplicationCount == 0:
        resultsHaskellTopSort.append(statistics.mean(res))
        res = []

# Reachable

# print("\n\nReachable\n")

leanReachableResultFileLocations = list(map(lambda f: "lean/" + resultsFolder + f + "." + "reachable" + fileExtension, stanfordFileNames))
haskellReachableResultFileLocations = list(map(lambda f: "haskell/" + resultsFolder + f + "." + "reachable" + fileExtension, stanfordFileNames))

resultsDepthFirst = []
resultsBreadthFirst = []
resultsHaskellReachable = []

# print("Lean:")
for fileLocation in leanReachableResultFileLocations:
    res = parseResults(fileLocation)
    depthFirst = []
    breadthFirst = []
    for i in range(len(res)):
        if i % 2 == 0: depthFirst.append(res[i])
        else: breadthFirst.append(res[i])

    resultsDepthFirst.append(statistics.mean(depthFirst))
    resultsBreadthFirst.append(statistics.mean(breadthFirst))

# print("Haskell:")
for fileLocation in haskellReachableResultFileLocations:
    res = parseResults(fileLocation)

    resultsHaskellReachable.append(statistics.mean(res))


# 
# Save to CSV file
# 

printControlMessage("Saving means to csv files")

# Topsort

fileHandle = open("results-topsort.csv", 'w')

for name in list(map(lambda f: f[2], graphParameters)): fileHandle.write(',' + name)
fileHandle.write("\nLean unsafe,")
fileHandle.write(arrayToCSVLine(resultsUnsafe))
fileHandle.write("\nLean safe,")
fileHandle.write(arrayToCSVLine(resultsSafe))
fileHandle.write("\nHaskell,")
fileHandle.write(arrayToCSVLine(resultsHaskellTopSort))
fileHandle.write("\n")

fileHandle.close()

# Reachable

fileHandle = open("results-reachable.csv", 'w')

for name in stanfordFileNames: fileHandle.write(',' + name)
fileHandle.write("\nLean depth-first,")
fileHandle.write(arrayToCSVLine(resultsDepthFirst))
fileHandle.write("\nLean breadth-first,")
fileHandle.write(arrayToCSVLine(resultsBreadthFirst))
fileHandle.write("\nHaskell,")
fileHandle.write(arrayToCSVLine(resultsHaskellReachable))
fileHandle.write("\n")

fileHandle.close()

printControlMessage("Done")
