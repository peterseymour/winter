## Winter is a relatively fast in-place interpreter for WebAssembly

Designed as a target for compiler testing it performs enough validation to pass the [WebAssembly Spec Core Tests][core] but not to run untrusted modules. The core interpreter loop is kept as simple as possible by reading the bytecode directly without the need for an intermediate representation. A small amount of auxillary information is created during a partial validation step on a per-function just-in-time basis (similar in nature to [A fast in-place interpreter for WebAssembly](https://arxiv.org/abs/2205.01183)). Many validation checks are skipped to keep the implementation small and meet the use case of a flexible development target. All core (including SIMD) test cases pass with the built-in tester.


 *  **Winter is relatively simple.**
 The interpreter is around [4,000 semicolons][src].
 The main interpreter loop is implemented as a giant switch/case statement.
 As much information is pre-computed to reduce instruction overhead.
 For instance functions calls that cross module boundaries are detected ahead of time and handled separately from the main call instruction.
 Given the overhead of decoding each instruction on every execution extra optimizations such as threaded code are unlikely to have a significant impact.
 The main loop could be swapped out for an assembly version with minimal changes.

 *  **Winter has a REPL.**
 Using the command line option `--repl` or `--repl=<script-path>` allows modules to be instantiated, functions to be invoked and globals introspected.

 *  **Winter can trace execution paths.**
 Using the command line option `--trace` on the debug build causes the auxillary information to be displayed when functions are first invoked.
 Each instruction executed is then traced for debugging.

 *  **Winter's module reader uses a novel form of the specificaton.**
 The specificaton for the binary representation of modules is given by [WebAssembly.json][json] and in human readabale form [WebAssembly.txt][txt].
 A Python tool then generates a C++ reader automagically.
 This form of the specification was derived from online sources and hand crafted from the offical specificaton which unfortunately does not lend itself to programmatic generation.

 *  **Winter can be fully tested using command line arguments.**
 The specificaton tests are stateful when they run meaning most implementations cannot be tested without linking directly to the source.
 Winter uses its REPL to invoke methods, read globals and register and instantiate modules in a single (stateful) session.
 The implicit `spectest` module is provided externally as a compiled `.wat` file.
 The tester is written in Python and runs externally to the interpreter.

## Build Instructions
```
./build.sh
```

Runs GCC to compile the main source to `bin`.

## Test Instructions
```
./test.sh --winter=bin/winterd <path-to-webassembly-spec>/test/core/ --exclude-dir=simd
```

Run the Python tester `Twinter` on all `.wast` files in the target directory.
Each file is compiled as needed using `wat2wasm` and run in a single `Winter` session as an external process. If tests fail then the additional command line option `--trace` will show the trace of the last test.

## Using the REPL

### List all exports
```
./bin/winterd <path-to-webassembly-spec>/test/core/output/names.2.wasm --repl --trace
> ?
...
Ꙗ: () -> (i32)
: () -> (i32)
: () -> (i32)
Ɐ: () -> (i32)
🅐: () -> (i32)
🅰: () -> (i32)
Ⱝ: () -> (i32)
𐐂: () -> (i32)
...
```

### Invoke a function
```
> Ɐ
Entering: __main__

-- Tracing --
eval[   0     ]: trap guard

-- Building 174 --
function(() -> (i32))[0]
in[   0     ]:   41 {+1 @1}
in[   3     ]:   end {@1}

function(() -> (i32))[0]
out[   0     ]: 41 {+1 @1}
out[   3/   0]: return {@1} {move 1/0 jmp 3/0}

eval[   0     ]: i32.const 174
eval[   3/   0]: return {move 1/0 jmp 3/0}
eval[   0     ]: trap exit
174
```

or

```
./bin/winterd <path-to-webassembly-spec>/test/core/output/names.2.wasm --trace --invoke="Ɐ"
```

### Instantiate a module and register it with a new name
```
./bin/winterd --repl --trace
> instantiate <path-to-webassembly-spec>/test/core/output/names.2.wasm
> ""
Entering: __main__

-- Tracing --
eval[   0     ]: trap guard

-- Building 0 --
function(() -> (i32))[0]
in[   0     ]:   41 {+1 @1}
in[   2     ]:   end {@1}

function(() -> (i32))[0]
out[   0     ]: 41 {+1 @1}
out[   2/   0]: return {@1} {move 1/0 jmp 2/0}

eval[   0     ]: i32.const 0
eval[   2/   0]: return {move 1/0 jmp 2/0}
eval[   0     ]: trap exit
0
> register "some long alias"
> "some long alias":""
Entering: __main__

-- Tracing --
eval[   0     ]: i32.const 0
eval[   2/   0]: return {move 1/0 jmp 2/0}
eval[   0     ]: trap exit
0
>
```

### Instantiate external modules and invoke
```
./bin/winterd <path-to-webassembly-spec>/test/core/output/imports.2.wasm --instantiate=spectest:spec/spectest.wasm --repl --trace
> ?
 -- Globals --
spectest:global_i32: 666
spectest:global_i64: 666
spectest:global_f32: 0x1.4dp+9
spectest:global_f64: 0x1.4dp+9

 -- Functions --
print_i32: (i32) -> ()
spectest:print: () -> ()
spectest:print_i32: (i32) -> ()
spectest:print_i64: (i64) -> ()
spectest:print_f32: (f32) -> ()
spectest:print_f64: (f64) -> ()
spectest:print_i32_f32: (i32, f32) -> ()
spectest:print_f64_f64: (f64, f64) -> ()

> spectest:print_i32 21
Entering: spectest

-- Tracing --
eval[   0     ]: trap guard

-- Building 1 --
function((i32) -> ())[0]
in[   0     ]:   end {@0}

function((i32) -> ())[0]
out[   0/   0]: return {@0} {move 0/1 jmp 0/0}

eval[   0/   0]: return {move 0/1 jmp 0/0}
eval[   0     ]: trap exit
> get spectest:global_i32
666
>
```

[core]: https://github.com/WebAssembly/spec/tree/main/test/core
[src]: https://github.com/peterseymour/winter/blob/main/
[json]: https://github.com/peterseymour/winter/blob/main/spec/WebAssembly.json
[txt]: https://github.com/peterseymour/winter/blob/main/spec/WebAssembly.txt
