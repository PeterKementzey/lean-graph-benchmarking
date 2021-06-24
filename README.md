# Benchmarking my Lean 4 [Graph library](https://github.com/PeterKementzey/graph-library-for-lean4)

## The Library

Find the library [here](https://github.com/PeterKementzey/graph-library-for-lean4).

## Prerequisites

### Python

The script requires a Python 3 installation, it has been tested on Python 3.8.5.

### Lean

Follow the basic setup of Lean 4 in [this guide](https://leanprover.github.io/lean4/doc/setup.html#basic-setup).

### Haskell

Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/). Building for the first time might take a while.

## How to reproduce

`chmod +x run-benchmarks.py; ./run-benchmarks.py` will

- delete any previous results
- generate some random directed acyclic graphs (predictable with the fixed seed)
- compile both binaries
- run the binaries on the generated graphs and save the running times
- gather the saved running times in the `./haskell/res/` and `./lean/res/` folders
- save the mean running times to `results.cvs`

alternatively you can use: `python3 run-benchmarks.py`

Overall this should take around 10 - 30 minutes if you have built the Haskell library at least once.
