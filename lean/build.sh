#!/bin/sh

rm -rf build

git clone https://github.com/PeterKementzey/graph-library-for-lean4 build/deps/Graph

(cd build/deps/Graph; git checkout --detach b54a57d94e7e132a6806e7197a6360b938bd27ba; leanpkg build lib)

leanpkg build bin LINK_OPTS=build/deps/Graph/build/lib/libGraph.a
