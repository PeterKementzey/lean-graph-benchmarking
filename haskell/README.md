# Haskell Benchmark

## Prerequisites

Install the Haskell build tool
[Stack](https://docs.haskellstack.org/en/stable/README/).

## Building

Run `stack build`. This will download the right version of GHC, build the
benchmark's dependencies and finally build the benchmark executable.

## Running the Benchmark

Run `stack run`. If you need to give arguments, `stack run -- args`.
