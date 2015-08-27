# ghcjs-examples

## Installation

To install or build this you need ghcjs, the proper cabal version, and the submodule dependencies. Then run the following:

```bash
git submodule update --init --recursive
bash ./rebuildSandbox.sh
cabal build
```