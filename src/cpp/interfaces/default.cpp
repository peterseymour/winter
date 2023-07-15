#include "default.hpp"
#include <cstring>


const identifier_t Default::NAME = str("default");


bool Default::push_arg(ValueStack& stack, value_type_t type, const char* arg) {
    switch (type) {
        case value_type_t::I32: {
            uint32_t v;
            if (parse_integer<uint32_t>(arg, v)) {stack.push(v); return true;}
            int32_t w;
            if (parse_integer<int32_t>(arg, w)) {stack.push(w); return true;}
            break;
        }
        case value_type_t::I64: {
            uint64_t v;
            if (parse_integer<uint64_t>(arg, v)) {stack.push(v); return true;}
            int64_t w;
            if (parse_integer<int64_t>(arg, w)) {stack.push(w); return true;}
            break;
        }
        case value_type_t::F32: {
            float32_t v;
            if (parse_decimal<float32_t>(arg, v)) {stack.push(v); return true;}
            break;
        }
        case value_type_t::F64: {
            float64_t v;
            if (parse_decimal<float64_t>(arg, v)) {stack.push(v); return true;}
            break;
        }
        case value_type_t::FUNCREF: {
            if (strcmp(arg, "null") == 0) {stack.push(type_traits<reference_type_t>::null(reference_type_t::FUNCREF)); return true;}
            break;
        }
        case value_type_t::EXTERNREF: {
            if (strcmp(arg, "null") == 0) {stack.push(type_traits<reference_type_t>::null(reference_type_t::EXTERNREF)); return true;}
            uint64_t v;
            if (parse_integer<uint64_t>(arg, v)) {stack.push(new ExternValue(value_type_t::I64, v)); return true;}
            break;
        }
        default:
            error(fstr("Unhandled parsing argument type", type));
    }

    return false;
}

bool Default::push_args(ValueStack& stack, const func_sig_t& signature, const char* args[], int nargs) {
    int t = 0;

    for (auto arg : range_t(args, args + nargs))
        if (not push_arg(stack, signature.params.elems[t++], arg))
            return false;

    return true;
}

void Default::init_main(Thread& thread, const func_sig_t& signature) {
    auto& stack = thread.stack;

    check(signature.params.size == nargs, "Wrong number of arguments for main");

    check(Default::push_args(stack, signature, args, nargs), "Invalid arguments for main");
}

int Default::exit_main(Thread& thread, const func_sig_t& signature) const {
    auto& stack = thread.stack;

    if (stack.size() != signature.returns.size) {
        stack.debug();
        warn("Invalid stack state exiting main");
        return 1;
    }

    auto fp = stack.frame(stack.size());

    auto i = 0;
    for (auto value_type : signature.returns)
        std::cout << TypedValue(value_type, ValueStack::frame_elem(fp, i++)) << std::endl;

    return 0;
}

const Interface::HostFns& Default::host_functions() const {
    static const HostFns fns = {
    };

    return fns;
}
