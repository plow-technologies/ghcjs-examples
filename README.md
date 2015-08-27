# ghcjs-examples
Keep examples small if possible, they are intended to be tutorials and references for people new to Shakespeare Dynamic and Live VDOM.

## Installation

To install or build this you need ghcjs, the proper cabal version, and the submodule dependencies. Then run the following:

```bash
git submodule update --init --recursive
bash ./rebuildSandbox.sh
cabal build
```

## Issues
  * Test out the textbox project. Update speed from the textbox to the div is slow.
  * Not sure how to two way bind data. E.g. typing in a form updates a data structure, clicking a clear button sets all elements in the data structure to empty string, how to automatically update the form?