#include "interpreter.hpp"
#include "os.hpp"
#include "arch.hpp"
#include <algorithm>
#include "interfaces/interface.hpp"


Interpreter::Interpreter() :
    threads(1)
{
    os::guard(segmentation_fault);
}

Instance* Interpreter::instantiate(const identifier_t& name, const Module* module, bool overwrite) {
    auto instance = new Instance(name, module);

    const auto start = instance->init(registry);

    registry.add(name, instance, overwrite);

    if (start != nullptr) {
        check(*start->signature == sig({}, {}), "Start function expected to have signature () -> ()");

        auto trap_message = start->invoke(this);

        if (trap_message != nullptr)
            throw Trap{.message=trap_message};
    }

    return instance;
}

const char* Interpreter::invoke(const WASMFunction* fn) {
    static const byte_t exit_code[] = {TRAP, TRAP_EXIT};

    threads[0]->call_stack.push(CallContext{
        IF_TRACE_SWITCH(.base_ip = exit_code,)
        IF_TRACE_SWITCH(.base_xp = nullptr,)
        .ip = exit_code,
        .xp = nullptr,
        .fp = nullptr,
    });

    return interpreter_loop(fn, threads[0]);
}

const char* Interpreter::invoke(const HostFunction* fn) {
    return "Not invoking host functions directly";
}


#define ileb(v)                 auto v##_ = sdecode(ip); ip += v##_.size; auto v = v##_.value;
#define uleb(v)                 auto v##_ = udecode(ip); ip += v##_.size; auto v = v##_.value;
#define reserved()              ip++
#define read(typ, v)            auto v = *reinterpret_cast<const typ*>(ip); ip += sizeof(typ) / sizeof(byte_t)
#define jmp(x)                  {const auto _xp = (x); ip += _xp->ip_offset; xp += _xp->xp_offset;}
#define brk(x)                  {const auto __xp = (x); stack.move(__xp->nkeep, __xp->ndrop); jmp(x);}
#define trap(msg)               return msg
#define trap_if(cond, msg)      IF_RUNTIME_VALIDATING(if (cond) trap(msg);)
#define trap_check(cond, msg)   trap_if(not (cond), msg);


#ifdef TRACE_SWITCH

#define trace(instr) IF_TRACING( \
    if (trace_limit != 0 and trace_limit-- == 0) error("Trace limit reached"); \
    std::cout << "eval[" << std::setw(4) << (instr_ip-base_ip) << "     ]: "; \
    std::cout << instr; \
    std::cout << std::endl; \
)

#define tracex(instr, x) IF_TRACING( \
    if (trace_limit != 0 and trace_limit-- == 0) error("Trace limit reached"); \
    std::cout << "eval[" << std::setw(4) << (instr_ip-base_ip) << "/" << std::setw(4) << ((x)-base_xp) << "]: "; \
    std::cout << instr; \
    std::cout << " {move " << (x)->nkeep << "/" << (x)->ndrop << " jmp " << (ip+(x)->ip_offset-base_ip) << "/" << ((x)+(x)->xp_offset-base_xp) << "}"; \
    std::cout << std::endl; \
)

#else

#define trace(instr)
#define tracex(instr, x)

#endif


#define load(name, stk_t, mem_t) \
    uleb(f) uleb(n) \
    trace(name << ' ' << f << ' ' << n) \
    auto mem = memory_space[0]; \
    const addr33_t base = stack.pop<uint32_t>(); \
    const auto value = mem->deref<mem_t>(base + n); \
    stack.push<stk_t>(value)

#define store(name, stk_t, mem_t) \
    uleb(f) uleb(n) \
    trace(name); \
    auto mem = memory_space[0]; \
    const auto value = stack.pop<stk_t>(); \
    const addr33_t base = stack.pop<uint32_t>(); \
    mem->deref<mem_t>(base + n) = value

#define args1(arg_t, v)                 const auto v = stack.pop<arg_t>()
#define args2(arg_t, v, w)              args1(arg_t, w); args1(arg_t, v)
#define ret(val_t, v)                   stack.push<val_t>(v)

#define cmpk(name, arg_t, op, k)        trace(name); args1(arg_t, v);    ret(uint32_t, v op k ? 1 : 0)
#define cmp(name, arg_t, op)            trace(name); args2(arg_t, v, w); ret(uint32_t, v op w ? 1 : 0)
#define binop(name, arg_t, op)          trace(name); args2(arg_t, v, w); ret(arg_t, v op w)
#define fbinop(name, arg_t, op)         trace(name); args2(arg_t, v, w); const auto r = v op w; ret(arg_t, is_nan(r) ? choose_nan(v, w) : r)
#define lop(name, arg_t, op)            trace(name); args1(arg_t, v);    ret(arg_t, op v)
#define apply(name, arg_t, f)           trace(name); args1(arg_t, v);    ret(arg_t, f(v))
#define apply2(name, arg_t, f)          trace(name); args2(arg_t, v, w); ret(arg_t, f(v, w))
#define fapply2(name, arg_t, f)         trace(name); args2(arg_t, v, w); const auto r = f(v, w); ret(arg_t, is_nan(r) ? choose_nan(v, w) : r)
#define call(name, ret_t, arg_t, f)     trace(name); args1(arg_t, v);    ret(ret_t, f(v))
#define trunc(name, ret_t, arg_t)       trace(name); args1(arg_t, v);    trap_if(std::isnan(v), "invalid conversion to integer"); ret_t r; trap_check(ftrunc(v, r), "integer overflow"); ret(ret_t, r)


#define ENTER(target_instance) \
    IF_TRACING(std::cout << "Entering: " << (target_instance)->name() << std::endl;) \
    instance = target_instance; \
    module = instance->module; \
    function_space = instance->function_space; \
    fast_call = instance->fast_call; \
    memory_space = instance->memory_space; \
    table_space = instance->table_space; \
    elements = instance->elements; \
    global_space = instance->global_space; \


const char* Interpreter::interpreter_loop(const WASMFunction* fn, Thread* thread) {
    static constexpr byte_t return_far_code[2] = {TRAP, TRAP_RETURN_FAR};

    const auto& fcall = fn->owning.instance->fast_call[fn->funcidx];

    //per thread stack
    auto& stack = thread->stack;
    auto& call_stack = thread->call_stack;
    Value* fp = stack.frame(fcall.nargs, fcall.nlocals);

    //per thread code control
    const byte_t* ip = fcall.code;
    const aux_t* xp = fcall.auxs;

    IF_TRACE_SWITCH(
        const byte_t* base_ip = ip;
        const aux_t* base_xp = xp;
        const byte_t* instr_ip = nullptr;
        uint32_t trace_limit = 0;
    )

    //current instance
    Instance* instance;
    const Module* module;
    range_t<Function*> function_space;
    range_t<FastCall> fast_call;
    range_t<Memory*> memory_space;
    range_t<Table*> table_space;
    range_t<range_t<const ref_t>> elements;
    range_t<Global*> global_space;

    ENTER(fn->owning.instance);

    IF_TRACING(std::cout << "\n-- Tracing --" << std::endl;)

    while (true) {
        IF_TRACE_SWITCH(instr_ip = ip;)

        const auto opcode = *ip++;

        switch (opcode) {
            case 0x00: {
                trace("unreachable");

                trap("unreachable instruction reached");
            }
            case 0x01: {
                trace("nop");

                IF_TRACING(stack.debug(fp);)
                break;
            }
            case 0x02: {
                ileb(bt);
                trace("block " << bt);

                break;
            }
            case 0x03: {
                ileb(bt);
                trace("loop " << bt);

                break;
            }
            case 0x04: {
                ileb(bt);
                tracex("if " << bt, xp);

                auto cond = stack.pop<uint32_t>();

                if (cond != 0) {
                    ++xp;
                } else {
                    jmp(xp);
                }
                break;
            }
            case 0x05: {
                tracex("else", xp)

                jmp(xp);
                break;
            }
            case 0x0b: {
                trace("end");

                break;
            }
            case 0x0c: {
                uleb(d);

                tracex("br " << d, xp);

                brk(xp);
                break;
            }
            case 0x0d: {
                uleb(d);
                tracex("br_if " << d, xp);

                auto cond = stack.pop<uint32_t>();

                if (cond != 0) {
                    brk(xp);
                } else {
                    ++xp;
                }

                break;
            }
            case 0x0e: {
                uleb(l);
                auto n = stack.pop<uint32_t>();

                if (n > l)
                    n = l;

                tracex("br_table " << l << " [+" << n << "]", xp + n)

                brk(xp + n);

                break;
            }
            case 0x0f: {
                tracex("return", xp);

                stack.move(xp->nkeep, xp->ndrop);

                const auto& ctxt = call_stack.top(); call_stack.pop();
                IF_TRACE_SWITCH(base_ip = ctxt.base_ip;)
                IF_TRACE_SWITCH(base_xp = ctxt.base_xp;)
                ip = ctxt.ip;
                xp = ctxt.xp;
                fp = ctxt.fp;

                break;
            }
            case 0x10: {
                uleb(f);
                trace("call " << f);

                call_stack.push(CallContext{
                    IF_TRACE_SWITCH(.base_ip = base_ip,)
                    IF_TRACE_SWITCH(.base_xp = base_xp,)
                    .ip = ip,
                    .xp = xp,
                    .fp = fp,
                });

                const auto& fcall = fast_call[f];

                IF_TRACE_SWITCH(base_ip =)
                ip = fcall.code;

                IF_TRACE_SWITCH(base_xp = )
                xp = fcall.auxs;

                fp = stack.frame(fcall.nargs, fcall.nlocals);
                break;
            }
            case 0x11: {
                uleb(s); uleb(t);
                trace("call_indirect " << s << " " << t);

                auto& tab = *table_space[t];

                const auto r = stack.pop<uint32_t>();
                trap_check(r < tab.size(), "undefined element");

                const auto& fcall = *tab.view<funcref_t>()[r];

                call_stack.push(CallContext{
                    IF_TRACE_SWITCH(.base_ip = base_ip,)
                    IF_TRACE_SWITCH(.base_xp = base_xp,)
                    .ip = ip,
                    .xp = xp,
                    .fp = fp,
                });

                //make this faster by pointer compare on canonical types
                trap_check(fcall.signature == nullptr or *fcall.signature == module->type_section->_.elems[s]->func, "indirect call type mismatch");

                IF_TRACE_SWITCH(base_ip =)
                ip = fcall.code;

                IF_TRACE_SWITCH(base_xp = )
                xp = fcall.auxs;

                fp = stack.frame(fcall.nargs, fcall.nlocals);
                break;
            }
            case 0x12: {
                uleb(f);
                tracex("return_call " << f, xp);

                stack.move(xp->nkeep, xp->ndrop);

                const auto& fcall = fast_call[f];

                IF_TRACE_SWITCH(base_ip =)
                ip = fcall.code;

                IF_TRACE_SWITCH(base_xp = )
                xp = fcall.auxs;

                fp = stack.frame(fcall.nargs, fcall.nlocals);
                break;
            }
            case 0x1a: {
                trace("drop");

                stack.drop();
                break;
            }
            case 0x1b: {
                trace("select");

                const auto c = stack.pop<uint32_t>();
                const auto w = stack.pop();
                const auto v = stack.pop();

                if (c != 0)
                    ret(Value, v);
                else
                    ret(Value, w);
                break;
            }
            case 0x1c: {
                uleb(l); check(l == 1, "Unexpected select t* length"); ileb(t);
                trace("select t*");

                const auto c = stack.pop<uint32_t>();
                const auto w = stack.pop();
                const auto v = stack.pop();

                if (c != 0)
                    ret(Value, v);
                else
                    ret(Value, w);
                break;
            }
            case 0x20: {
                uleb(n)
                trace("local.get " << n);

                ret(Value, ValueStack::frame_elem(fp, n));
                break;
            }
            case 0x21: {
                uleb(n)
                trace("local.set " << n);

                ValueStack::frame_elem(fp, n) = stack.pop();
                break;
            }
            case 0x22: {
                uleb(n)
                trace("local.tee " << n);

                ValueStack::frame_elem(fp, n) = stack.top();
                break;
            }
            case 0x23: {
                uleb(n)
                trace("global.get " << n);

                ret(Value, global_space[n]->typed_value().value());
                break;
            }
            case 0x24: {
                uleb(n)
                trace("global.set " << n);

                global_space[n]->set(stack.pop());
                break;
            }
            case 0x25: {
                uleb(t)
                trace("table.get " << t);

                const auto& tab = *table_space[t];

                const auto i = stack.pop<uint32_t>();

                trap_check(i < tab.size(), TABLE_OUT_OF_BOUNDS);

                ret(ref_t, tab[i]);
                break;
            }
            case 0x26: {
                uleb(t)
                trace("table.set " << t);

                auto& tab = *table_space[t];

                const auto v = stack.pop<ref_t>();
                const auto i = stack.pop<uint32_t>();

                trap_check(i < tab.size(), TABLE_OUT_OF_BOUNDS);

                tab[i] = v;
                break;
            }
            case 0x28: {
                load("i32.load", uint32_t, uint32_t);
                break;
            }
            case 0x29: {
                load("i64.load", uint64_t, uint64_t);
                break;
            }
            case 0x2a: {
                load("f32.load", float32_t, float32_t);
                break;
            }
            case 0x2b: {
                load("f64.load", float64_t, float64_t);
                break;
            }
            case 0x2c: {
                load("i32.load8_s", uint32_t, int8_t);
                break;
            }
            case 0x2d: {
                load("i32.load8_u", uint32_t, uint8_t);
                break;
            }
            case 0x2e: {
                load("i32.load16_s", uint32_t, int16_t);
                break;
            }
            case 0x2f: {
                load("i32.load16_u", uint32_t, uint16_t);
                break;
            }
            case 0x30: {
                load("i64.load8_s", uint64_t, int8_t);
                break;
            }
            case 0x31: {
                load("i64.load8_u", uint64_t, uint8_t);
                break;
            }
            case 0x32: {
                load("i64.load16_s", uint64_t, int16_t);
                break;
            }
            case 0x33: {
                load("i64.load16_u", uint64_t, uint16_t);
                break;
            }
            case 0x34: {
                load("i64.load32_s", uint64_t, int32_t);
                break;
            }
            case 0x35: {
                load("i64.load32_u", uint64_t, uint32_t);
                break;
            }
            case 0x36: {
                store("i32.store", uint32_t, uint32_t);
                break;
            }
            case 0x37: {
                store("i64.store", uint64_t, uint64_t);
                break;
            }
            case 0x38: {
                store("f32.store", float32_t, float32_t);
                break;
            }
            case 0x39: {
                store("f64.store", float64_t, float64_t);
                break;
            }
            case 0x3a: {
                store("i32.store8", uint32_t, uint8_t);
                break;
            }
            case 0x3b: {
                store("i32.store16", uint32_t, uint16_t);
                break;
            }
            case 0x3c: {
                store("i64.store8", uint64_t, uint8_t);
                break;
            }
            case 0x3d: {
                store("i64.store16", uint64_t, uint16_t);
                break;
            }
            case 0x3e: {
                store("i64.store32", uint64_t, uint32_t);
                break;
            }
            case 0x3f: {
                uleb(r);
                trace("memory.size");

                auto& mem = *memory_space[0];

                ret(uint32_t, mem.size() / WASM_PAGE_SIZE);
                break;
            }
            case 0x40: {
                uleb(r);
                trace("memory.grow");

                auto& mem = *memory_space[0];

                const auto delta = stack.pop<uint32_t>();

                const auto oldsize = mem.size_in_pages();

                if (mem.grow(delta, memory_out_of_bounds))
                    ret(uint32_t, oldsize);
                else
                    ret(int32_t, -1);

                break;
            }
            case 0x41: {
                ileb(n)
                trace("i32.const " << n);

                ret(uint32_t, n);
                break;
            }
            case 0x42: {
                ileb(n)
                trace("i64.const " << n);

                ret(uint64_t, n);
                break;
            }
            case 0x43: {
                read(float32_t, f);
                trace("f32.const " << f);

                ret(float32_t, f);
                break;
            }
            case 0x44: {
                read(float64_t, f);
                trace("f64.const " << f);

                ret(float64_t, f);
                break;
            }
            case 0x45: {
                cmpk("i32.eqz", uint32_t, ==, 0);
                break;
            }
            case 0x46: {
                cmp("i32.eq", uint32_t, ==);
                break;
            }
            case 0x47: {
                cmp("i32.ne", uint32_t, !=);
                break;
            }
            case 0x48: {
                cmp("i32.lt_s", int32_t, <);
                break;
            }
            case 0x49: {
                cmp("i32.lt_u", uint32_t, <);
                break;
            }
            case 0x4a: {
                cmp("i32.gt_s", int32_t, >);
                break;
            }
            case 0x4b: {
                cmp("i32.gt_u", uint32_t, >);
                break;
            }
            case 0x4c: {
                cmp("i32.le_s", int32_t, <=);
                break;
            }
            case 0x4d: {
                cmp("i32.le_u", uint32_t, <=);
                break;
            }
            case 0x4e: {
                cmp("i32.ge_s", int32_t, >=);
                break;
            }
            case 0x4f: {
                cmp("i32.ge_u", uint32_t, >=);
                break;
            }
            case 0x50: {
                cmpk("i64.eqz", uint64_t, ==, 0);
                break;
            }
            case 0x51: {
                cmp("i64.eq", uint64_t, ==);
                break;
            }
            case 0x52: {
                cmp("i64.ne", uint64_t, !=);
                break;
            }
            case 0x53: {
                cmp("i64.lt_s", int64_t, <);
                break;
            }
            case 0x54: {
                cmp("i64.lt_u", uint64_t, <);
                break;
            }
            case 0x55: {
                cmp("i64.gt_s", int64_t, >);
                break;
            }
            case 0x56: {
                cmp("i64.gt_u", uint64_t, >);
                break;
            }
            case 0x57: {
                cmp("i64.le_s", int64_t, <=);
                break;
            }
            case 0x58: {
                cmp("i64.le_u", uint64_t, <=);
                break;
            }
            case 0x59: {
                cmp("i64.ge_s", int64_t, >=);
                break;
            }
            case 0x5a: {
                cmp("i64.ge_u", uint64_t, >=);
                break;
            }
            case 0x5b: {
                cmp("f32.eq", float32_t, ==);
                break;
            }
            case 0x5c: {
                cmp("f32.ne", float32_t, !=);
                break;
            }
            case 0x5d: {
                cmp("f32.lt", float32_t, <);
                break;
            }
            case 0x5e: {
                cmp("f32.gt", float32_t, >);
                break;
            }
            case 0x5f: {
                cmp("f32.le", float32_t, <=);
                break;
            }
            case 0x60: {
                cmp("f32.ge", float32_t, >=);
                break;
            }
            case 0x61: {
                cmp("f64.eq", float64_t, ==);
                break;
            }
            case 0x62: {
                cmp("f64.ne", float64_t, !=);
                break;
            }
            case 0x63: {
                cmp("f64.lt", float64_t, <);
                break;
            }
            case 0x64: {
                cmp("f64.gt", float64_t, >);
                break;
            }
            case 0x65: {
                cmp("f64.le", float64_t, <=);
                break;
            }
            case 0x66: {
                cmp("f64.ge", float64_t, >=);
                break;
            }
            case 0x67: {
                apply("i32.clz", uint32_t, clz<uint32_t>);
                break;
            }
            case 0x68: {
                apply("i32.ctz", uint32_t, ctz<uint32_t>);
                break;
            }
            case 0x69: {
                apply("i32.popcnt", uint32_t, popcnt<uint32_t>);
                break;
            }
            case 0x6a: {
                binop("i32.add", uint32_t, +);
                break;
            }
            case 0x6b: {
                binop("i32.sub", uint32_t, -);
                break;
            }
            case 0x6c: {
                binop("i32.mul", uint32_t, *);
                break;
            }
            case 0x6d: {
                trace("i32.div_s");
                args2(int32_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                trap_if(v == std::numeric_limits<int32_t>::min() and w == -1, INTEGER_OVERFLOW);
                ret(int32_t, v / w);
                break;
            }
            case 0x6e: {
                trace("i32.div_u");
                args2(uint32_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                ret(uint32_t, v / w);
                break;
            }
            case 0x6f: {
                trace("i32.rem_s");
                args2(int32_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                if (v == std::numeric_limits<int32_t>::min() and w == -1)
                    ret(int32_t, 0);
                else
                    ret(int32_t, v % w);
                break;
            }
            case 0x70: {
                trace("i32.rem_u");
                args2(uint32_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                ret(uint32_t, v % w);
                break;
            }
            case 0x71: {
                binop("i32.and", uint32_t, &);
                break;
            }
            case 0x72: {
                binop("i32.or", uint32_t, |);
                break;
            }
            case 0x73: {
                binop("i32.xor", uint32_t, ^);
                break;
            }
            case 0x74: {
                binop("i32.shl", uint32_t, <<);
                break;
            }
            case 0x75: {
                binop("i32.shr_s", int32_t, >>);
                break;
            }
            case 0x76: {
                binop("i32.shr_u", uint32_t, >>);
                break;
            }
            case 0x77: {
                apply2("i32.rotl", uint32_t, rotl<uint32_t>);
                break;
            }
            case 0x78: {
                apply2("i32.rotr", uint32_t, rotr<uint32_t>);
                break;
            }
            case 0x79: {
                apply("i64.clz", uint64_t, clz<uint64_t>);
                break;
            }
            case 0x7a: {
                apply("i64.ctz", uint64_t, ctz<uint64_t>);
                break;
            }
            case 0x7b: {
                apply("i64.popcnt", uint64_t, popcnt<uint64_t>);
                break;
            }
            case 0x7c: {
                binop("i64.add", uint64_t, +);
                break;
            }
            case 0x7d: {
                binop("i64.sub", uint64_t, -);
                break;
            }
            case 0x7e: {
                binop("i64.mul", uint64_t, *);
                break;
            }
            case 0x7f: {
                trace("i64.div_s");
                args2(int64_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                trap_if(v == std::numeric_limits<int64_t>::min() and w == -1, INTEGER_OVERFLOW);
                ret(int64_t, v / w);
                break;
            }
            case 0x80: {
                trace("i64.div_u");
                args2(uint64_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                ret(uint64_t, v / w);
                break;
            }
            case 0x81: {
                trace("i64.rem_s");
                args2(int64_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                if (v == std::numeric_limits<int64_t>::min() and w == -1)
                    ret(int64_t, 0);
                else
                    ret(int64_t, v % w);
                break;
            }
            case 0x82: {
                trace("i64.rem_u");
                args2(uint64_t, v, w);
                trap_check(w != 0, INTEGER_DIVIDE_BY_ZERO);
                ret(uint64_t, v % w);
                break;
            }
            case 0x83: {
                binop("i64.and", uint64_t, &);
                break;
            }
            case 0x84: {
                binop("i64.or", uint64_t, |);
                break;
            }
            case 0x85: {
                binop("i64.xor", uint64_t, ^);
                break;
            }
            case 0x86: {
                binop("i64.shl", uint64_t, <<);
                break;
            }
            case 0x87: {
                binop("i64.shr_s", int64_t, >>);
                break;
            }
            case 0x88: {
                binop("i64.shr_u", uint64_t, >>);
                break;
            }
            case 0x89: {
                apply2("i64.rotl", uint64_t, rotl<uint64_t>);
                break;
            }
            case 0x8a: {
                apply2("i64.rotr", uint64_t, rotr<uint64_t>);
                break;
            }
            case 0x8b: {
                apply("f32.abs", float32_t, fabs<float32_t>);
                break;
            }
            case 0x8c: {
                apply("f32.neg", float32_t, fneg<float32_t>);
                break;
            }
            case 0x8d: {
                apply("f32.ceil", float32_t, fceil<float32_t>);
                break;
            }
            case 0x8e: {
                apply("f32.floor", float32_t, ffloor<float32_t>);
                break;
            }
            case 0x8f: {
                apply("f32.trunc", float32_t, ftrunc<float32_t>);
                break;
            }
            case 0x90: {
                apply("f32.nearest", float32_t, fnearest<float32_t>);
                break;
            }
            case 0x91: {
                apply("f32.sqrt", float32_t, fsqrt<float32_t>);
                break;
            }
            case 0x92: {
                fbinop("f32.add", float32_t, +);
                break;
            }
            case 0x93: {
                fbinop("f32.sub", float32_t, -);
                break;
            }
            case 0x94: {
                fbinop("f32.mul", float32_t, *);
                break;
            }
            case 0x95: {
                fbinop("f32.div", float32_t, /);
                break;
            }
            case 0x96: {
                fapply2("f32.min", float32_t, fmin<float32_t>);
                break;
            }
            case 0x97: {
                fapply2("f32.max", float32_t, fmax<float32_t>);
                break;
            }
            case 0x98: {
                fapply2("f32.copysign", float32_t, fcopysign<float32_t>);
                break;
            }
            case 0x99: {
                apply("f64.abs", float64_t, fabs<float64_t>);
                break;
            }
            case 0x9a: {
                apply("f64.neg", float64_t, fneg<float64_t>);
                break;
            }
            case 0x9b: {
                apply("f64.ceil", float64_t, fceil<float64_t>);
                break;
            }
            case 0x9c: {
                apply("f64.floor", float64_t, ffloor<float64_t>);
                break;
            }
            case 0x9d: {
                apply("f64.trunc", float64_t, ftrunc<float64_t>);
                break;
            }
            case 0x9e: {
                apply("f64.nearest", float64_t, fnearest<float64_t>);
                break;
            }
            case 0x9f: {
                apply("f64.sqrt", float64_t, fsqrt<float64_t>);
                break;
            }
            case 0xa0: {
                fbinop("f64.add", float64_t, +);
                break;
            }
            case 0xa1: {
                fbinop("f64.sub", float64_t, -);
                break;
            }
            case 0xa2: {
                fbinop("f64.mul", float64_t, *);
                break;
            }
            case 0xa3: {
                fbinop("f64.div", float64_t, /);
                break;
            }
            case 0xa4: {
                fapply2("f64.min", float64_t, fmin<float64_t>);
                break;
            }
            case 0xa5: {
                fapply2("f64.max", float64_t, fmax<float64_t>);
                break;
            }
            case 0xa6: {
                fapply2("f64.copysign", float64_t, fcopysign<float64_t>);
                break;
            }
            case 0xa7: {
                trace("i32.wrap_i64");
                args1(uint64_t, v);
                ret(uint32_t, v);
                break;
            }
            case 0xa8: {
                trunc("i32.trunc_f32_s", int32_t, float32_t);
                break;
            }
            case 0xa9: {
                trunc("i32.trunc_f32_u", uint32_t, float32_t);
                break;
            }
            case 0xaa: {
                trunc("i32.trunc_f64_s", int32_t, float64_t);
                break;
            }
            case 0xab: {
                trunc("i32.trunc_f64_u", uint32_t, float64_t);
                break;
            }
            case 0xac: {
                trace("i64.extend_i32_s");
                args1(int32_t, v);
                ret(int64_t, v);
                break;
            }
            case 0xad: {
                trace("i64.extend_i32_u");
                args1(uint32_t, v);
                ret(uint64_t, v);
                break;
            }
            case 0xae: {
                trunc("i64.trunc_f32_s", int64_t, float32_t);
                break;
            }
            case 0xaf: {
                trunc("i64.trunc_f32_u", uint64_t, float32_t);
                break;
            }
            case 0xb0: {
                trunc("i64.trunc_f64_s", int64_t, float64_t);
                break;
            }
            case 0xb1: {
                trunc("i64.trunc_f64_u", uint64_t, float64_t);
                break;
            }
            case 0xb2: {
                trace("f32.convert_i32_s");
                args1(int32_t, v);
                ret(float32_t, v);
                break;
            }
            case 0xb3: {
                trace("f32.convert_i32_u");
                args1(uint32_t, v);
                ret(float32_t, v);
                break;
            }
            case 0xb4: {
                trace("f32.convert_i64_s");
                args1(int64_t, v);
                ret(float32_t, v);
                break;
            }
            case 0xb5: {
                trace("f32.convert_i64_u");
                args1(uint64_t, v);
                ret(float32_t, v);
                break;
            }
            case 0xb6: {
                trace("f32.demote_f64");
                args1(float64_t, v);
                ret(float32_t, v);
                break;
            }
            case 0xb7: {
                trace("f64.convert_i32_s");
                args1(int32_t, v);
                ret(float64_t, v);
                break;
            }
            case 0xb8: {
                trace("f64.convert_i32_u");
                args1(uint32_t, v);
                ret(float64_t, v);
                break;
            }
            case 0xb9: {
                trace("f64.convert_i64_s");
                args1(int64_t, v);
                ret(float64_t, v);
                break;
            }
            case 0xba: {
                trace("f64.convert_i64_u");
                args1(uint64_t, v);
                ret(float64_t, v);
                break;
            }
            case 0xbb: {
                trace("f64.promote_f32");
                args1(float32_t, v);
                ret(float64_t, v);
                break;
            }
            case 0xbc: {
                trace("i32.reinterpret_f32");
                args1(float32_t, v);
                ret(uint32_t, reinterpret<uint32_t>(v));
                break;
            }
            case 0xbd: {
                trace("i64.reinterpret_f64");
                args1(float64_t, v);
                ret(int64_t, reinterpret<int64_t>(v));
                break;
            }
            case 0xbe: {
                trace("f32.reinterpret_i32");
                args1(uint32_t, v);
                ret(float32_t, reinterpret<float32_t>(v));
                break;
            }
            case 0xbf: {
                trace("f64.reinterpret_i64");
                args1(uint64_t, v);
                ret(float64_t, reinterpret<float64_t>(v));
                break;
            }
            case 0xc0: {
                trace("i32.extend8_s");
                args1(int8_t, v);
                ret(int32_t, v);
                break;
            }
            case 0xc1: {
                trace("i32.extend16_s");
                args1(int16_t, v);
                ret(int32_t, v);
                break;
            }
            case 0xc2: {
                trace("i64.extend8_s");
                args1(int8_t, v);
                ret(int64_t, v);
                break;
            }
            case 0xc3: {
                trace("i64.extend16_s");
                args1(int16_t, v);
                ret(int64_t, v);
                break;
            }
            case 0xc4: {
                trace("i64.extend32_s");
                args1(int32_t, v);
                ret(int64_t, v);
                break;
            }
            case 0xd0: {
                ileb(t);
                trace("ref.null " << t);
                ret(ref_t, type_traits<reference_type_t>::null(static_cast<reference_type_t>(t)));
                break;
            }
            case 0xd1: {
                trace("ref.is_null");
                args1(ref_t, r);
                ret(uint32_t, type_traits<reference_type_t>::isnull(r) ? 1 : 0);
                break;
            }
            case 0xd2: {
                uleb(f);
                trace("ref.func");
                ret(funcref_t, function_space[f]->ref());
                break;
            }
            case 0xfc: {
                auto opcode = *ip++;

                switch (opcode) {
                    case 0x00: {
                        call("i32.trunc_sat_f32_s", int32_t, float32_t, ftrunc_sat<int32_t>);
                        break;
                    }
                    case 0x01: {
                        call("i32.trunc_sat_f32_u", uint32_t, float32_t, ftrunc_sat<uint32_t>);
                        break;
                    }
                    case 0x02: {
                        call("i32.trunc_sat_f64_s", int32_t, float64_t, ftrunc_sat<int32_t>);
                        break;
                    }
                    case 0x03: {
                        call("i32.trunc_sat_f64_u", uint32_t, float64_t, ftrunc_sat<uint32_t>);
                        break;
                    }
                    case 0x04: {
                        call("i64.trunc_sat_f32_s", int64_t, float32_t, ftrunc_sat<int64_t>);
                        break;
                    }
                    case 0x05: {
                        call("i64.trunc_sat_f32_u", uint64_t, float32_t, ftrunc_sat<uint64_t>);
                        break;
                    }
                    case 0x06: {
                        call("i64.trunc_sat_f64_s", int64_t, float64_t, ftrunc_sat<int64_t>);
                        break;
                    }
                    case 0x07: {
                        call("i64.trunc_sat_f64_u", uint64_t, float64_t, ftrunc_sat<uint64_t>);
                        break;
                    }
                    case 0x08: {
                        uleb(x); reserved();
                        trace("memory.init " << x);

                        auto& mem = *memory_space[0];
                        const auto& data = instance->datas[x];

                        auto n = stack.pop<uint32_t>();
                        addr33_t s = stack.pop<uint32_t>();
                        addr33_t d = stack.pop<uint32_t>();

                        trap_check(s + n <= data.size() and d + n <= mem.size(), MEMORY_OUT_OF_BOUNDS);

                        while (n-- != 0)
                            mem.deref<uint8_t>(d++) = data[s++];

                        break;
                    }
                    case 0x09: {
                        uleb(x);
                        trace("data.drop " << x);

                        instance->datas[x] = range_t<const byte_t>();
                        break;
                    }
                    case 0x0a: {
                        reserved(); reserved();
                        trace("memory.copy");

                        auto& mem_src = *memory_space[0];
                        auto& mem_dst = *memory_space[0];

                        auto n = stack.pop<uint32_t>();
                        addr33_t s = stack.pop<uint32_t>();
                        addr33_t d = stack.pop<uint32_t>();

                        trap_check(s + n <= mem_src.size() and d + n <= mem_dst.size(), MEMORY_OUT_OF_BOUNDS);

                        if (d <= s) {
                            while (n-- != 0)
                                mem_dst.deref<uint8_t>(d++) = mem_src.deref<uint8_t>(s++);
                        }
                        else {
                            s += n;
                            d += n;
                            while (n-- != 0)
                                mem_dst.deref<uint8_t>(--d) = mem_src.deref<uint8_t>(--s);
                        }

                        break;
                    }
                    case 0x0b: {
                        reserved();
                        trace("memory.fill");

                        auto& mem = *memory_space[0];

                        auto n = stack.pop<uint32_t>();
                        const auto val = stack.pop<uint32_t>();
                        addr33_t d = stack.pop<uint32_t>();

                        trap_check(d + n <= mem.size(), MEMORY_OUT_OF_BOUNDS);

                        while (n-- != 0)
                            mem.deref<uint8_t>(d++) = val;

                        break;
                    }
                    case 0x0c: {
                        uleb(e); uleb(t);
                        trace("table.init " << e << " " << t);

                        const auto& elems = elements[e];
                        auto& tab = *table_space[t];

                        auto n = stack.pop<uint32_t>();
                        addr33_t s = stack.pop<uint32_t>();
                        addr33_t d = stack.pop<uint32_t>();

                        trap_check(s + n <= elements[e].size() and d + n <= tab.size(), TABLE_OUT_OF_BOUNDS);

                        auto p = &tab[d];
                        for (auto q = &elems[s]; q != &elems[s + n]; ++q)
                            *p++ = *q;

                        break;
                    }
                    case 0x0d: {
                        uleb(x);
                        trace("elem.drop " << x);

                        elements[x] = range_t<const ref_t>();
                        break;
                    }
                    case 0x0e: {
                        uleb(dt); uleb(st);
                        trace("table.copy " << dt << " " << st);

                        const auto& src = *table_space[st];
                        auto& dst = *table_space[dt];

                        auto n = stack.pop<uint32_t>();
                        addr33_t s = stack.pop<uint32_t>();
                        addr33_t d = stack.pop<uint32_t>();

                        trap_check(s + n <= src.size() and d + n <= dst.size(), TABLE_OUT_OF_BOUNDS);

                        std::copy(&src[s], &src[s + n], &dst[d]);
                        break;
                    }
                    case 0x0f: {
                        uleb(x);
                        trace("table.grow " << x);

                        auto& tab = *table_space[x];

                        const auto delta = stack.pop<uint32_t>();
                        const auto initial = stack.pop<ref_t>();

                        const auto oldsize = tab.size();

                        if (tab.grow(delta, memory_out_of_bounds, initial))
                            ret(uint32_t, oldsize);
                        else
                            ret(int32_t, -1);

                        break;
                    }
                    case 0x10: {
                        uleb(x);
                        trace("table.size " << x);

                        const auto& tab = *table_space[x];

                        ret(uint32_t, tab.size());

                        break;
                    }
                    case 0x11: {
                        uleb(x);
                        trace("table.fill " << x);

                        auto &tab = *table_space[x];

                        auto n = stack.pop<uint32_t>();
                        auto r = stack.pop<ref_t>();
                        auto i = stack.pop<uint32_t>();

                        trap_check(i + n <= tab.size(), TABLE_OUT_OF_BOUNDS);

                        std::fill(&tab[i], &tab[i+n], r);
                        break;
                    }
                    default:
                        return fstr("Trap: Unhandled opcode in expression 0xfc 0x%02x", opcode);
                }

                break;
            }
            case TRAP: {
                uleb(trapcode);

                switch (trapcode) {
                    case TRAP_UNINIT_ELEM:
                        trace("trap uninit elem");

                        trap("uninitialized element");
                        break;
                    case TRAP_EXIT: {
                        trace("trap exit");

                        return nullptr;
                    }
                    case TRAP_CALL_HOST: {
                        auto funcidx = fast_call.indexof(ip - sizeof(TRAP) - sizeof(TRAP_CALL_HOST) - offsetof(FastCall, inline_code));

                        const auto func = function_space[funcidx];
                        trace("trap call_host " << func->owning.interface->name() << ":" << *func->name);

                        const auto& fcall = fast_call[funcidx];

                        const auto host_fn = (HostFn) (((*(const uint64_t*)(ip-2))) >> 16);

                        (*host_fn)(func->owning.interface, *instance, *thread);

                        const auto& ctxt = call_stack.top(); call_stack.pop();
                        IF_TRACE_SWITCH(base_ip = ctxt.base_ip;)
                        IF_TRACE_SWITCH(base_xp = ctxt.base_xp;)
                        ip = ctxt.ip;
                        xp = ctxt.xp;
                        fp = ctxt.fp;
                        break;
                    }
                    case TRAP_GUARD: {
                        trace("trap guard");

                        auto funcidx = fast_call.indexof(ip - sizeof(TRAP) - sizeof(TRAP_GUARD) - offsetof(FastCall, inline_code));

                        auto fn = dynamic_cast<WASMFunction*>(function_space[funcidx]);

                        fn->build_auxillary();

                        auto& fcall = fast_call[funcidx];

                        fn->build_fast_call(instance, fcall);

                        IF_TRACE_SWITCH(base_ip =)
                        ip = fcall.code;
                        IF_TRACE_SWITCH(base_xp =)
                        xp = fcall.auxs;

                        break;
                    }
                    case TRAP_CALL_FAR: {
                        const uint64_t* block = (const uint64_t*) (ip-2);

                        const auto& far_fcall = *((const FastCall*) (block[0] >> 16));
                        auto* far_instance = (Instance*) block[1];

                        trace("trap call_far " << far_instance->name() << ":" << far_instance->fast_call.indexof(far_fcall));

                        if (far_instance != instance) {
                            call_stack.push(CallContext{
                                IF_TRACE_SWITCH(.base_ip = return_far_code,)
                                IF_TRACE_SWITCH(.base_xp = nullptr,)
                                .ip = return_far_code,
                                .xp = nullptr,
                                .instance = instance,
                            });

                            ENTER(far_instance);
                        }

                        IF_TRACE_SWITCH(base_ip =)
                        ip = far_fcall.code;
                        IF_TRACE_SWITCH(base_xp =)
                        xp = far_fcall.auxs;

                        break;
                    }
                    case TRAP_RETURN_FAR: {
                        trace("trap return_far");

                        const auto& far_ctxt = call_stack.last();

                        check(far_ctxt.ip == return_far_code, "Invalid ip for return_far");

                        ENTER(far_ctxt.instance);

                        //regular near return
                        const auto& ctxt = call_stack.top(); call_stack.pop();
                        IF_TRACE_SWITCH(base_ip = ctxt.base_ip;)
                        IF_TRACE_SWITCH(base_xp = ctxt.base_xp;)
                        ip = ctxt.ip;
                        xp = ctxt.xp;
                        fp = ctxt.fp;

                        break;
                    }
                    default:
                        trace("trap " << trapcode);

                        trap("Unimplemented trap");
                        break;
                }

                break;
            }
            default:
                trap(fstr("Unhandled opcode in expression 0x%02x", opcode));
        }
    }

    return "left interpreter loop";
}

