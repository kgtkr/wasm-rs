#!/bin/bash -eu
rm -rf spec
mkdir spec
cp ./wasm-spec/test/core/*.wast ./spec

for file in $(ls spec); do
  wast2json spec/$file
done