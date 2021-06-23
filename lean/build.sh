#!/bin/sh

rm -rf build

git clone https://github.com/PeterKementzey/graph-library-for-lean4 build/deps/Graph

(cd build/deps/Graph; git checkout --detach 98863f1c5e1b64fe933e26c54402ad437db20473; leanpkg build lib)

leanpkg build bin LINK_OPTS=build/deps/Graph/build/lib/libGraph.a
