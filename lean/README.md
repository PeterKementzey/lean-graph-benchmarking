# Lean Benchmark

## Prerequisites

Install the basic install of Lean 4 using elan following [this guide](https://leanprover.github.io/lean4/doc/setup.html#basic-setup).

*Note: you may have to run `chmod +x *.sh` to be able to run these scripts after having cloned the repo.*

## Building

To get the dependency and build the library run `./build.sh`. You may want to change which version of the library you want to use on line 5 of `build.sh` by changing the commit hash to the desired version.

## Running

TODO: mention script that runs tests here.
To run the built binary run `./build/bin/Benchmark`.

## Visual Studio Code

If you want to use the Visual Studio Code Lean 4 extension make sure to open the `/lean/` directory of this project as a folder and run the build script at least once for the dependency to be correctly recognized. After that you can run the `Lean 4: Restart Server` VS Code command to load the dependency.
