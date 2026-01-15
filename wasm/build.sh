#!/usr/bin/env bash

set -e

out_path=$(wasm32-wasi-cabal list-bin 0315-wasmlib | tail -n1)

if [[ $PWD == */wasm ]]; then
	cd ..
fi

rm -rf wasm/dist
mkdir wasm/dist

out_path=$(wasm32-wasi-cabal list-bin 0315-wasmlib | tail -n1)

wasm32-wasi-cabal build 0315-wasmlib
cp $out_path wasm/dist/0315.wasm
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$out_path" --output wasm/dist/ghc_wasm_jsffi.js

tsc ./wasm/lib.ts --module esnext --target esnext --strict -d --outDir ./wasm/dist

cp -r wasm/dist site
