#!/bin/bash -eu
rm -rf wast-dist
mkdir wast-dist
cp ./WebAssembly/spec/test/core/*.wast ./wast-dist

for file in $(ls wast-dist); do
  wast2json wast-dist/$file -o wast-dist/$file.json
done