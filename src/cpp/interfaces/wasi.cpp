#include "wasi.hpp"
#include <cstring>


const identifier_t Wasi::NAME = str("wasi_snapshot_preview1");


void Wasi::init_main(Thread& thread, const func_sig_t& signature) {
    check(signature == sig({}, {}), "Wasi interface requires main function to have signature () -> ()");
}

int Wasi::exit_main(Thread& thread, const func_sig_t& signature) const {
    auto& stack = thread.stack;

    if (stack.size() != 0)
        warn("Wasi interface left values on stack exiting main");

    return 0;
}

const Interface::HostFns& Wasi::host_functions() const {
    static const HostFns fns = {
        {str("args_get"),       {Wasi::args_get,        sig({I32, I32},             {I32})}},
        {str("args_sizes_get"), {Wasi::args_sizes_get,  sig({I32, I32},             {I32})}},
        {str("fd_close"),       {Wasi::fd_close,        sig({I32},                  {I32})}},
        {str("fd_fdstat_get"),  {Wasi::fd_fdstat_get,   sig({I32, I32},             {I32})}},
        {str("fd_seek"),        {Wasi::fd_seek,         sig({I32, I64, I32, I32},   {I32})}},
        {str("fd_write"),       {Wasi::fd_write,        sig({I32, I32, I32, I32},   {I32})}},
        {str("proc_exit"),      {Wasi::proc_exit,       sig({I32},                  {}   )}},
    };

    return fns;
}

void Wasi::args_get(Interface* self, Instance& instance, Thread& thread) {
    auto wasi = self->as<Wasi>();
    auto& stack = thread.stack;

    auto argv_buf = stack.pop<uint32_t>();
    auto argv = stack.pop<uint32_t>();

    auto& mem = *instance.memory_space[0];

    std::cout << " argv=" << argv << " argv_buf=" << argv_buf << std::endl;

    for (auto arg : range_t(wasi->args, wasi->args + wasi->nargs)) {
        mem.deref<uint32_t>(argv) = argv_buf;
        argv += 4;

        const auto dest = &mem.deref<char>(argv_buf);
        argv_buf += strcpy(dest, arg) - dest;
    }

    return stack.push<uint32_t>(0);
}

void Wasi::args_sizes_get(Interface* self, Instance& instance, Thread& thread) {
    auto wasi = self->as<Wasi>();
    auto& stack = thread.stack;

    const auto size_ptr = stack.pop<uint32_t>();
    const auto nargs_ptr = stack.pop<uint32_t>();

    std::cout << " nargs_ptr=" << nargs_ptr << " size_ptr=" << size_ptr << std::endl;

    auto buf_size = 0;
    for (auto arg : range_t(wasi->args, wasi->args + wasi->nargs))
        buf_size += strlen(arg) + 1;

    std::cout << " #args=" << wasi->nargs << " buf_size=" << buf_size << std::endl;

    auto& mem = *instance.memory_space[0];

    mem.deref<uint32_t>(nargs_ptr) = wasi->nargs;
    mem.deref<uint32_t>(size_ptr) = buf_size;

    return stack.push<uint32_t>(0);
}

void Wasi::fd_close(Interface* self, Instance& instance, Thread& thread) {
    error("Not implemented fd_close");
}

void Wasi::fd_fdstat_get(Interface* self, Instance& instance, Thread& thread) {
    error("Not implemented fd_fdstat_get");
}

void Wasi::fd_seek(Interface* self, Instance& instance, Thread& thread) {
    error("Not implemented fd_seek");
}

void Wasi::fd_write(Interface* self, Instance& instance, Thread& thread) {
    error("Not implemented fd_write");
}

void Wasi::proc_exit(Interface* self, Instance& instance, Thread& thread) {
    auto& stack = thread.stack;

    const auto retval = stack.pop<uint32_t>();

    exit(retval);
}