#!/bin/sh

# rm -rf build # FIXME reenable this

git clone https://github.com/PeterKementzey/graph-library-for-lean4 build/deps/Graph

(cd build/deps/Graph; git checkout --detach aa0c39b12e04f63a7cc6da475c592b522876618a; leanpkg build lib)

leanpkg build bin LINK_OPTS=build/deps/Graph/build/lib/libGraph.a
