#include "wave.hpp"
#include <cstring>
#include <unistd.h>


const identifier_t Wave::NAME = str("wave");


void Wave::init_main(Thread& thread, const func_sig_t& signature) {
    auto& stack = thread.stack;

    check(signature == sig({I32}, {I32}), "Wave interface requires main function to have signature (nargs:i32) -> (exit_code:i32)");

    stack.push<uint32_t>(nargs);
}

int Wave::exit_main(Thread& thread, const func_sig_t& signature) const {
    auto& stack = thread.stack;

    if (stack.size() != 1)
        warn("Invalid stack state exiting main");

    return stack.empty() ? 1 : stack.pop<int32_t>();
}

const Interface::HostFns& Wave::host_functions() const {
    static const HostFns fns = {
        {str("arg_len"),    {Wave::arg_len,     sig({I32},           {I32})}},
        {str("arg_copy"),   {Wave::arg_copy,    sig({I32, I32, I32}, {I32})}},
        {str("fs_write"),   {Wave::fs_write,    sig({I32, I32, I32}, {I32})}},
    };

    return fns;
}

void Wave::arg_len(Interface* self, Instance& instance, Thread& thread) {
    auto wave = self->as<Wave>();
    auto& stack = thread.stack;

    const auto arg = stack.pop<uint32_t>();

    if (arg >= wave->nargs)
        return stack.push<int32_t>(-1);

    return stack.push<uint32_t>(strlen(wave->args[arg]));
}

void Wave::arg_copy(Interface* self, Instance& instance, Thread& thread) {
    auto wave = self->as<Wave>();
    auto& stack = thread.stack;

    auto len = stack.pop<uint32_t>();
    auto ptr = stack.pop<uint32_t>();
    auto arg = stack.pop<uint32_t>();

    if (arg >= wave->nargs)
        return stack.push<int32_t>(-1);

    auto data = wave->args[arg];
    len = std::min<uint32_t>(len, strlen(data));

    auto& mem = *instance.memory_space[0];

    auto dst = mem.begin() + ptr;
    for (auto i = 0; i < len; ++i)
        *dst++ = *data++;

    return stack.push<uint32_t>(len);
}

void Wave::fs_write(Interface* self, Instance& instance, Thread& thread) {
    auto& stack = thread.stack;

    auto len = stack.pop<uint32_t>();
    auto buf = stack.pop<uint32_t>();
    auto fd = stack.pop<int32_t>();

    if (fd < 0 or fd > 2)
        return stack.push<int32_t>(-1);

    auto& mem = *instance.memory_space[0];

    write(fd, mem.begin() + buf, len);

    return stack.push<uint32_t>(len);
}
