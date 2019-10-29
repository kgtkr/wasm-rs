#/bin/bash -eu
cd example
for file in `ls ../wat`; do
    wat2wasm ../wat/$file
done