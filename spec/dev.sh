#! /bin/bash

#python3 gen.py ../src/cpp/wasm/gen
(cd ../src/cpp/ && ./build.sh                                                               \
    && echo && echo "NestedIf"                                                              \
    && ./bin/winter ../../archive/examples/nestedif.wasm --invoke=main 2                    \
    && echo && echo "Fac"                                                                   \
    && ./bin/winter /home/peter/Dev/wizard-engine/examples/fac.wasm --invoke=fac 4          \
    && echo && echo "RecSub"                                                                \
    && ./bin/winter ../../archive/examples/recsub.wasm --invoke=main 10 2                   \
    && echo && echo "Echo"                                                                  \
    && ./bin/winter /home/peter/Dev/wizard-engine/test/wizeng/echo.wasm -- hello  world     \
    && echo && echo "Helloworld"                                                            \
#    && ./bin/winter /home/peter/Dev/wasi-sdk-14.0/helloworld.wasm --trace                   \
)
