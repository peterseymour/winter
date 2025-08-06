#! /bin/bash

(cd spec && python3 build.py > WebAssembly.json)
(cd spec && python3 emit.py > WebAssembly.txt)
(cd spec && python3 gen.py ../src/cpp/wasm/gen/)

if [ -z "$1" ]
then
      target_dir="$(pwd)/bin"
else
      target_dir=$1
fi

mkdir -p $target_dir

cd src/cpp

g++ -I. -Werror -Wswitch -Wfatal-errors -masm=intel --std=gnu++20 -DTRACE_SWITCH -DRUNTIME_VALIDATION common.cpp os.cpp wasm/binary.cpp winter/runtime.cpp winter/winter.cpp winter/interpreter.cpp interfaces/interface.cpp interfaces/default.cpp interfaces/wasi.cpp interfaces/wave.cpp -o $target_dir/winterd
g++ -I. -Werror -Wswitch -Wfatal-errors -masm=intel --std=gnu++20                                     common.cpp os.cpp wasm/binary.cpp winter/runtime.cpp winter/winter.cpp winter/interpreter.cpp interfaces/interface.cpp interfaces/default.cpp interfaces/wasi.cpp interfaces/wave.cpp -o $target_dir/winter
