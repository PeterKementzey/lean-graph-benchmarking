#!/bin/sh

# rm -rf build # FIXME reenable this

git clone https://github.com/PeterKementzey/graph-library-for-lean4 build/deps/Graph

(cd build/deps/Graph; git checkout --detach e73f70c848de80cce8cbff50aa8586496bc47c1d; leanpkg build lib)

leanpkg build bin LINK_OPTS=build/deps/Graph/build/lib/libGraph.a
