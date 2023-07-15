#include "wasm/binary.hpp"
#include "runtime.hpp"


struct  Interpreter : Invoker {
    ThreadPool threads;
    Registry registry;

    Interpreter();

    Instance* instantiate(const identifier_t& name, const Module* module, bool overwrite=false);

    virtual const char* invoke(const WASMFunction* fn) override;
    virtual const char* invoke(const HostFunction* fn) override;

    static const char* interpreter_loop(const WASMFunction* fn, Thread* thread);
};
