#! /bin/bash

(cd spec && wat2wasm spectest.wat)
(python3 src/py/twinter.py "$@" --instantiate=spectest:spec/spectest.wasm)
