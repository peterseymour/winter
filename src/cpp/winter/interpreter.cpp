#include "interpreter.hpp"
#include "os.hpp"
#include "arch.hpp"
#include <algorithm>
#include "interfaces/interface.hpp"
#include <bit>


template<typename lane_t, size_t N=sizeof(v128_t) / sizeof(lane_t)>
using vlanes_t = std::array<lane_t, N>;

template<typename lane_t>
using v64lanes_t = vlanes_t<lane_t, sizeof(uint64_t) / sizeof(lane_t)>;


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
    const addr33_t base = stack.pop<uint32_t>(); \
    const auto value = memory_space[0]->deref<mem_t>(base + n); \
    stack.push<stk_t>(value)

#define load_lane(name, lane_t) \
    uleb(f) uleb(n) read(byte_t, l); \
    trace(name << ' ' << f << ' ' << n << " lane=" << (int) l); \
    auto lanes = stack.pop<vlanes_t<lane_t>>(); \
    const addr33_t base = stack.pop<uint32_t>(); \
    const auto value = memory_space[0]->deref<lane_t>(base + n); \
    lanes[l % lanes.size()] = value; \
    stack.push<vlanes_t<lane_t>>(lanes)

#define store_impl(f, n, val_t, mem_t, valexpr) \
    const auto value = stack.pop<val_t>(); \
    const addr33_t base = stack.pop<uint32_t>(); \
    memory_space[0]->deref<mem_t>(base + n) = (valexpr)

#define store(name, stk_t, mem_t) \
    uleb(f) uleb(n) \
    trace(name << ' ' << f << ' ' << n); \
    store_impl(f, n, stk_t, mem_t, value)

#define store_lane(name, lane_t) \
    uleb(f) uleb(n) read(byte_t, l); \
    trace(name << ' ' << f << ' ' << n << " lane=" << (int) l); \
    store_impl(f, n, vlanes_t<lane_t>, lane_t, value[l])


#define concat(x, y)                    x##y
#define args1(arg_t, v)                 const auto v = stack.pop<arg_t>()
#define args2(arg_t, v, w)              args1(arg_t, w); args1(arg_t, v)
#define args3(arg_t, v, w, x)           args1(arg_t, x); args2(arg_t, v, w)
#define vargs1(lane_t, v)               args1(v128_t, concat(v, tmp)); auto v = std::bit_cast<vlanes_t<lane_t>>(concat(v, tmp))
#define vargs2(lane_t, v, w)            vargs1(lane_t, w); vargs1(lane_t, v)
#define ret(val_t, v)                   stack.push<val_t>(v)
#define vret(lane_t)                    stack.push<vlanes_t<lane_t>>

#define cmpk(name, arg_t, op, k)        if (name != nullptr) trace(name); args1(arg_t, v);    ret(uint32_t, v op k ? 1 : 0)
#define cmp(name, arg_t, op)            trace(name); args2(arg_t, v, w); ret(uint32_t, v op w ? 1 : 0)
#define unop(name, arg_t, op)           trace(name); args1(arg_t, v); ret(arg_t, op v)
#define binop(name, arg_t, op)          trace(name); args2(arg_t, v, w); ret(arg_t, v op w)
#define fbinop(name, arg_t, op)         trace(name); args2(arg_t, v, w); const auto r = v op w; ret(arg_t, is_nan(r) ? choose_nan(v, w) : r)
#define apply(name, arg_t, f)           trace(name); args1(arg_t, v);    ret(arg_t, f(v))
#define apply2(name, arg_t, f)          trace(name); args2(arg_t, v, w); ret(arg_t, f(v, w))
#define fapply2(name, arg_t, f)         trace(name); args2(arg_t, v, w); const auto r = f(v, w); ret(arg_t, is_nan(r) ? choose_nan(v, w) : r)
#define vcmpk(name, lane_t, op, k)      trace(name); vargs1(lane_t, lanes); vlanes_t<intn<sizeof(lane_t)>> olanes; for (auto i = 0; i < olanes.size(); ++i) olanes[i] = lanes[i] op k ? 1 : 0; vret(intn<sizeof(lane_t)>)(olanes)
#define vcmp(name, lane_t, op)          trace(name); vargs2(lane_t, lanes1, lanes2); vlanes_t<intn<sizeof(lane_t)>> olanes; for (auto i = 0; i < olanes.size(); ++i) olanes[i] = (lanes1[i] op lanes2[i] ? -1 : 0); vret(intn<sizeof(lane_t)>)(olanes)
#define vunop(name, lane_t, op)         trace(name); vargs1(lane_t, lanes); for (lane_t& vv : lanes) vv = (op vv); vret(lane_t)(lanes)
#define vbinop(name, lane_t, op)        trace(name); vargs2(lane_t, lanes1, lanes2); for (auto i = 0; i < lanes1.size(); ++i) lanes1[i] = lanes1[i] op lanes2[i]; vret(lane_t)(lanes1)
#define vfbinop(name, lane_t, op)       trace(name); vargs2(lane_t, lanes1, lanes2); for (auto i = 0; i < lanes1.size(); ++i) {const auto r = lanes1[i] op lanes2[i]; lanes1[i] = is_nan(r) ? choose_nan(lanes1[i], lanes2[i]) : r;} vret(lane_t)(lanes1)
#define vapply(name, lane_t, f)         trace(name); vargs1(lane_t, lanes); for (lane_t& vv : lanes) vv = f(vv); vret(lane_t)(lanes)
#define vapply2(name, lane_t, f)        trace(name); vargs2(lane_t, lanes1, lanes2); for (auto i = 0; i < lanes1.size(); ++i) lanes1[i] = f(lanes1[i], lanes2[i]); vret(lane_t)(lanes1)
#define vapply2a(name, lane_t, f, arg_t)trace(name); args1(arg_t, w); vargs1(lane_t, lanes); for (lane_t& vv : lanes) vv = f(vv, w); vret(lane_t)(lanes)
#define vmask(name, lane_t, vv, mask_t, expr)  trace(name); vargs1(lane_t, lanes); mask_t m = 0; for (auto vv : lanes) m = (m >> 1) | ((expr) << (sizeof(v128_t) / sizeof(lane_t) - 1)); ret(mask_t, m);
#define vmap(lane_t, f, ilanes_t)       args1(ilanes_t, ilanes); vlanes_t<lane_t> olanes; for (auto i = 0; i < olanes.size(); ++i) {auto& lhs = olanes[i]; const auto& rhs = ilanes[i]; lhs = f(rhs);} vret(lane_t)(olanes);
#define vmap2(name, lane_t, f, ilane_t) trace(name); vargs2(ilane_t, ilanes1, ilanes2); vlanes_t<lane_t> olanes; for (auto i = 0; i < olanes.size(); ++i) olanes[i] = f(i < ilanes1.size() ? ilanes1[i] : ilanes2[i - ilanes1.size()]); vret(lane_t)(olanes)

#define call(name, ret_t, arg_t, f)     trace(name); args1(arg_t, v); ret(ret_t, f(v))
#define trunc(name, ret_t, arg_t)       trace(name); args1(arg_t, v); trap_if(std::isnan(v), "invalid conversion to integer"); ret_t r; trap_check(ftrunc(v, r), "integer overflow"); ret(ret_t, r)


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
                load("i32.load8_s", int32_t, int8_t);
                break;
            }
            case 0x2d: {
                load("i32.load8_u", uint32_t, uint8_t);
                break;
            }
            case 0x2e: {
                load("i32.load16_s", int32_t, int16_t);
                break;
            }
            case 0x2f: {
                load("i32.load16_u", uint32_t, uint16_t);
                break;
            }
            case 0x30: {
                load("i64.load8_s", int64_t, int8_t);
                break;
            }
            case 0x31: {
                load("i64.load8_u", uint64_t, uint8_t);
                break;
            }
            case 0x32: {
                load("i64.load16_s", int64_t, int16_t);
                break;
            }
            case 0x33: {
                load("i64.load16_u", uint64_t, uint16_t);
                break;
            }
            case 0x34: {
                load("i64.load32_s", int64_t, int32_t);
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
                apply2("i32.shl", uint32_t, shl<uint32_t>);
                break;
            }
            case 0x75: {
                apply2("i32.shr_s", int32_t, shr<int32_t>);
                break;
            }
            case 0x76: {
                apply2("i32.shr_u", uint32_t, shr<uint32_t>);
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
                ret(int64_t, extend<int64_t>(v));
                break;
            }
            case 0xad: {
                trace("i64.extend_i32_u");
                args1(uint32_t, v);
                ret(uint64_t, extend<uint64_t>(v));
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
                ret(float32_t, fconvert<float32_t>(v));
                break;
            }
            case 0xb3: {
                trace("f32.convert_i32_u");
                args1(uint32_t, v);
                ret(float32_t, fconvert<float32_t>(v));
                break;
            }
            case 0xb4: {
                trace("f32.convert_i64_s");
                args1(int64_t, v);
                ret(float32_t, fconvert<float32_t>(v));
                break;
            }
            case 0xb5: {
                trace("f32.convert_i64_u");
                args1(uint64_t, v);
                ret(float32_t, fconvert<float32_t>(v));
                break;
            }
            case 0xb6: {
                trace("f32.demote_f64");
                args1(float64_t, v);
                ret(float32_t, fdemote<float32_t>(v));
                break;
            }
            case 0xb7: {
                trace("f64.convert_i32_s");
                args1(int32_t, v);
                ret(float64_t, fconvert<float64_t>(v));
                break;
            }
            case 0xb8: {
                trace("f64.convert_i32_u");
                args1(uint32_t, v);
                ret(float64_t, fconvert<float64_t>(v));
                break;
            }
            case 0xb9: {
                trace("f64.convert_i64_s");
                args1(int64_t, v);
                ret(float64_t, fconvert<float64_t>(v));
                break;
            }
            case 0xba: {
                trace("f64.convert_i64_u");
                args1(uint64_t, v);
                ret(float64_t, fconvert<float64_t>(v));
                break;
            }
            case 0xbb: {
                trace("f64.promote_f32");
                args1(float32_t, v);
                ret(float64_t, fpromote<float64_t>(v));
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
                ret(int32_t, extend<int32_t>(v));
                break;
            }
            case 0xc1: {
                trace("i32.extend16_s");
                args1(int16_t, v);
                ret(int32_t, extend<int32_t>(v));
                break;
            }
            case 0xc2: {
                trace("i64.extend8_s");
                args1(int8_t, v);
                ret(int64_t, extend<int64_t>(v));
                break;
            }
            case 0xc3: {
                trace("i64.extend16_s");
                args1(int16_t, v);
                ret(int64_t, extend<int64_t>(v));
                break;
            }
            case 0xc4: {
                trace("i64.extend32_s");
                args1(int32_t, v);
                ret(int64_t, extend<int64_t>(v));
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
                uleb(opcode);

                switch (opcode) {
                    case 0: {
                        call("i32.trunc_sat_f32_s", int32_t, float32_t, ftrunc_sat<int32_t>);
                        break;
                    }
                    case 1: {
                        call("i32.trunc_sat_f32_u", uint32_t, float32_t, ftrunc_sat<uint32_t>);
                        break;
                    }
                    case 2: {
                        call("i32.trunc_sat_f64_s", int32_t, float64_t, ftrunc_sat<int32_t>);
                        break;
                    }
                    case 3: {
                        call("i32.trunc_sat_f64_u", uint32_t, float64_t, ftrunc_sat<uint32_t>);
                        break;
                    }
                    case 4: {
                        call("i64.trunc_sat_f32_s", int64_t, float32_t, ftrunc_sat<int64_t>);
                        break;
                    }
                    case 5: {
                        call("i64.trunc_sat_f32_u", uint64_t, float32_t, ftrunc_sat<uint64_t>);
                        break;
                    }
                    case 6: {
                        call("i64.trunc_sat_f64_s", int64_t, float64_t, ftrunc_sat<int64_t>);
                        break;
                    }
                    case 7: {
                        call("i64.trunc_sat_f64_u", uint64_t, float64_t, ftrunc_sat<uint64_t>);
                        break;
                    }
                    case 8: {
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
                    case 9: {
                        uleb(x);
                        trace("data.drop " << x);

                        instance->datas[x] = range_t<const byte_t>();
                        break;
                    }
                    case 10: {
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
                    case 11: {
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
                    case 12: {
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
                    case 13: {
                        uleb(x);
                        trace("elem.drop " << x);

                        elements[x] = range_t<const ref_t>();
                        break;
                    }
                    case 14: {
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
                    case 15: {
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
                    case 16: {
                        uleb(x);
                        trace("table.size " << x);

                        const auto& tab = *table_space[x];

                        ret(uint32_t, tab.size());

                        break;
                    }
                    case 17: {
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
                        return fstr("Trap: Unhandled opcode in expression 0xfc %d", (int) opcode);
                }

                break;
            }
            case 0xfd: {
                uleb(opcode);

                switch (opcode) {
                    case 0: {
                        load("v128.load", v128_t, v128_t);
                        break;
                    }
                    case 1: {
                        load("v128.load8x8_s", uint64_t, uint64_t);
                        vmap(int16_t, extend<int16_t>, v64lanes_t<int8_t>);
                        break;
                    }
                    case 2: {
                        load("v128.load8x8_u", uint64_t, uint64_t);
                        vmap(uint16_t, extend<uint16_t>, v64lanes_t<uint8_t>);
                        break;
                    }
                    case 3: {
                        load("v128.load16x4_s", uint64_t, uint64_t);
                        vmap(int32_t, extend<int32_t>, v64lanes_t<int16_t>);
                        break;
                    }
                    case 4: {
                        load("v128.load16x4_u", uint64_t, uint64_t);
                        vmap(uint32_t, extend<uint32_t>, v64lanes_t<uint16_t>);
                        break;
                    }
                    case 5: {
                        load("v128.load32x2_s", uint64_t, uint64_t);
                        vmap(int64_t, extend<int64_t>, v64lanes_t<int32_t>);
                        break;
                    }
                    case 6: {
                        load("v128.load32x2_u", uint64_t, uint64_t);
                        vmap(uint64_t, extend<uint64_t>, v64lanes_t<uint32_t>);
                        break;
                    }
                    case 7: {
                        load("v128.load8_splat", int8_t, int8_t);
                        args1(int8_t, v);
                        vret(int8_t)({v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v});
                        break;
                    }
                    case 8: {
                        load("v128.load16_splat", int16_t, int16_t);
                        args1(int16_t, v);
                        vret(int16_t)({v, v, v, v, v, v, v, v});
                        break;
                    }
                    case 9: {
                        load("v128.load32_splat", int32_t, int32_t);
                        args1(int32_t, v);
                        vret(int32_t)({v, v, v, v});
                        break;
                    }
                    case 10: {
                        load("v128.load64_splat", int64_t, int64_t);
                        args1(int64_t, v);
                        vret(int64_t)({v, v});
                        break;
                    }
                    case 11: {
                        store("v128.store", v128_t, v128_t);
                        break;
                    }
                    case 12: {
                        read(v128_t, v);
                        trace("v128.const " << v);

                        ret(v128_t, v);
                        break;
                    }
                    case 13: {
                        read(vlanes_t<uint8_t>, perm);
                        trace("i8x16.shuffle " << std::bit_cast<v128_t>(perm));
                        vargs2(uint8_t, lanes1, lanes2);
                        vlanes_t<uint8_t> lanes;
                        for (auto i = 0; i < lanes.size(); ++i) lanes[i] = (perm[i] < lanes1.size() ? lanes1 : lanes2)[perm[i] % lanes1.size()];
                        vret(uint8_t)(lanes);
                        break;
                    }
                    case 14: {
                        trace("i8x16.swizzle");
                        vargs2(uint8_t, vals, perm);
                        vlanes_t<uint8_t> lanes;
                        for (auto i = 0; i < lanes.size(); ++i) lanes[i] = perm[i] < vals.size() ? vals[perm[i]] : 0;
                        vret(uint8_t)(lanes);
                        break;
                    }
                    case 15: {
                        trace("i8x16.splat");
                        args1(uint8_t, v);
                        vret(uint8_t)({v, v, v, v, v, v, v, v, v, v, v, v, v, v, v, v});
                        break;
                    }
                    case 16: {
                        trace("i16x8.splat");
                        args1(uint16_t, v);
                        vret(uint16_t)({v, v, v, v, v, v, v, v});
                        break;
                    }
                    case 17: {
                        trace("i32x4.splat");
                        args1(uint32_t, v);
                        vret(uint32_t)({v, v, v, v});
                        break;
                    }
                    case 18: {
                        trace("i64x2.splat");
                        args1(uint64_t, v);
                        vret(uint64_t)({v, v});
                        break;
                    }
                    case 19: {
                        trace("f32x4.splat");
                        args1(float32_t, v);
                        vret(float32_t)({v, v, v, v});
                        break;
                    }
                    case 20: {
                        trace("f64x2.splat");
                        args1(float64_t, v);
                        vret(float64_t)({v, v});
                        break;
                    }
                    case 21: {
                        read(byte_t, l);
                        trace("i8x16.extract_lane_s " << (int) l);
                        vargs1(int8_t, lanes);
                        ret(int32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 22: {
                        read(byte_t, l);
                        trace("i8x16.extract_lane_u " << (int) l);
                        vargs1(uint8_t, lanes);
                        ret(uint32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 23: {
                        read(byte_t, l);
                        trace("i8x16.replace_lane " << (int) l);
                        args1(uint8_t, v); vargs1(uint8_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(uint8_t)(lanes);
                        break;
                    }
                    case 24: {
                        read(byte_t, l);
                        trace("i16x8.extract_lane_s " << (int) l);
                        vargs1(int16_t, lanes);
                        ret(int32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 25: {
                        read(byte_t, l);
                        trace("i16x8.extract_lane_u " << (int) l);
                        vargs1(uint16_t, lanes);
                        ret(uint32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 26: {
                        read(byte_t, l);
                        trace("i16x8.replace_lane " << (int) l);
                        args1(uint16_t, v); vargs1(uint16_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(uint16_t)(lanes);
                        break;
                    }
                    case 27: {
                        read(byte_t, l);
                        trace("i32x4.extract_lane " << (int) l);
                        vargs1(int32_t, lanes);
                        ret(int32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 28: {
                        read(byte_t, l);
                        trace("i32x4.replace_lane " << (int) l);
                        args1(uint32_t, v); vargs1(uint32_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(uint32_t)(lanes);
                        break;
                    }
                    case 29: {
                        read(byte_t, l);
                        trace("i64x2.extract_lane " << (int) l);
                        vargs1(int64_t, lanes);
                        ret(int64_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 30: {
                        read(byte_t, l);
                        trace("i64x2.replace_lane " << (int) l);
                        args1(uint64_t, v); vargs1(uint64_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(uint64_t)(lanes);
                        break;
                    }
                    case 31: {
                        read(byte_t, l);
                        trace("f32x2.extract_lane " << (int) l);
                        vargs1(float32_t, lanes);
                        ret(float32_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 32: {
                        read(byte_t, l);
                        trace("f32x2.replace_lane " << (int) l);
                        args1(float32_t, v); vargs1(float32_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(float32_t)(lanes);
                        break;
                    }
                    case 33: {
                        read(byte_t, l);
                        trace("f64x2.extract_lane " << (int) l);
                        vargs1(float64_t, lanes);
                        ret(float64_t, lanes[l % lanes.size()]);
                        break;
                    }
                    case 34: {
                        read(byte_t, l);
                        trace("f64x2.replace_lane " << (int) l);
                        args1(float64_t, v); vargs1(float64_t, lanes);
                        lanes[l % lanes.size()] = v;
                        vret(float64_t)(lanes);
                        break;
                    }
                    case 35: {
                        vcmp("i8x16.eq", uint8_t, ==);
                        break;
                    }
                    case 36: {
                        vcmp("i8x16.ne", uint8_t, !=);
                        break;
                    }
                    case 37: {
                        vcmp("i8x16.lt_s", int8_t, <);
                        break;
                    }
                    case 38: {
                        vcmp("i8x16.lt_u", uint8_t, <);
                        break;
                    }
                    case 39: {
                        vcmp("i8x16.gt_s", int8_t, >);
                        break;
                    }
                    case 40: {
                        vcmp("i8x16.gt_u", uint8_t, >);
                        break;
                    }
                    case 41: {
                        vcmp("i8x16.le_s", int8_t, <=);
                        break;
                    }
                    case 42: {
                        vcmp("i8x16.le_u", uint8_t, <=);
                        break;
                    }
                    case 43: {
                        vcmp("i8x16.ge_s", int8_t, >=);
                        break;
                    }
                    case 44: {
                        vcmp("i8x16.ge_u", uint8_t, >=);
                        break;
                    }
                    case 45: {
                        vcmp("i16x8.eq", uint16_t, ==);
                        break;
                    }
                    case 46: {
                        vcmp("i16x8.ne", uint16_t, !=);
                        break;
                    }
                    case 47: {
                        vcmp("i16x8.lt_s", int16_t, <);
                        break;
                    }
                    case 48: {
                        vcmp("i16x8.lt_u", uint16_t, <);
                        break;
                    }
                    case 49: {
                        vcmp("i16x8.gt_s", int16_t, >);
                        break;
                    }
                    case 50: {
                        vcmp("i16x8.gt_u", uint16_t, >);
                        break;
                    }
                    case 51: {
                        vcmp("i16x8.le_s", int16_t, <=);
                        break;
                    }
                    case 52: {
                        vcmp("i16x8.le_u", uint16_t, <=);
                        break;
                    }
                    case 53: {
                        vcmp("i16x8.ge_s", int16_t, >=);
                        break;
                    }
                    case 54: {
                        vcmp("i16x8.ge_u", uint16_t, >=);
                        break;
                    }
                    case 55: {
                        vcmp("i32x4.eq", uint32_t, ==);
                        break;
                    }
                    case 56: {
                        vcmp("i32x4.ne", uint32_t, !=);
                        break;
                    }
                    case 57: {
                        vcmp("i32x4.lt_s", int32_t, <);
                        break;
                    }
                    case 58: {
                        vcmp("i32x4.lt_u", uint32_t, <);
                        break;
                    }
                    case 59: {
                        vcmp("i32x4.gt_s", int32_t, >);
                        break;
                    }
                    case 60: {
                        vcmp("i32x4.gt_u", uint32_t, >);
                        break;
                    }
                    case 61: {
                        vcmp("i32x4.le_s", int32_t, <=);
                        break;
                    }
                    case 62: {
                        vcmp("i32x4.le_u", uint32_t, <=);
                        break;
                    }
                    case 63: {
                        vcmp("i32x4.ge_s", int32_t, >=);
                        break;
                    }
                    case 64: {
                        vcmp("i32x4.ge_u", uint32_t, >=);
                        break;
                    }
                    case 65: {
                        vcmp("f32x4.eq", float32_t, ==);
                        break;
                    }
                    case 66: {
                        vcmp("f32x4.ne", float32_t, !=);
                        break;
                    }
                    case 67: {
                        vcmp("f32x4.lt", float32_t, <);
                        break;
                    }
                    case 68: {
                        vcmp("f32x4.gt", float32_t, >);
                        break;
                    }
                    case 69: {
                        vcmp("f32x4.le", float32_t, <=);
                        break;
                    }
                    case 70: {
                        vcmp("f32x4.ge", float32_t, >=);
                        break;
                    }
                    case 71: {
                        vcmp("f64x2.eq", float64_t, ==);
                        break;
                    }
                    case 72: {
                        vcmp("f64x2.ne", float64_t, !=);
                        break;
                    }
                    case 73: {
                        vcmp("f64x2.lt", float64_t, <);
                        break;
                    }
                    case 74: {
                        vcmp("f64x2.gt", float64_t, >);
                        break;
                    }
                    case 75: {
                        vcmp("f64x2.le", float64_t, <=);
                        break;
                    }
                    case 76: {
                        vcmp("f64x2.ge", float64_t, >=);
                        break;
                    }
                    case 77: {
                        unop("v128.not", v128_t, ~);
                        break;
                    }
                    case 78: {
                        binop("v128.and", v128_t, &);
                        break;
                    }
                    case 79: {
                        apply2("v128.andnot", v128_t, andnot<v128_t>);
                        break;
                    }
                    case 80: {
                        binop("v128.or", v128_t, |);
                        break;
                    }
                    case 81: {
                        binop("v128.xor", v128_t, ^);
                        break;
                    }
                    case 82: {
                        trace("v128.bitselect");
                        args3(v128_t, v, w, c);
                        ret(v128_t, (v & c) | (w & ~c));
                        break;
                    }
                    case 83: {
                        cmpk("v128.any_true", v128_t, !=, 0);
                        break;
                    }
                    case 84: {
                        load_lane("v128.load8_lane", uint8_t);
                        break;
                    }
                    case 85: {
                        load_lane("v128.load16_lane", uint16_t);
                        break;
                    }
                    case 86: {
                        load_lane("v128.load32_lane", uint32_t);
                        break;
                    }
                    case 87: {
                        load_lane("v128.load64_lane", uint64_t);
                        break;
                    }
                    case 88: {
                        store_lane("v128.store8_lane", uint8_t);
                        break;
                    }
                    case 89: {
                        store_lane("v128.store16_lane", uint16_t);
                        break;
                    }
                    case 90: {
                        store_lane("v128.store32_lane", uint32_t);
                        break;
                    }
                    case 91: {
                        store_lane("v128.store64_lane", uint64_t);
                        break;
                    }
                    case 92: {
                        load("v128.load32_zero", uint32_t, uint32_t);
                        args1(uint32_t, v);
                        ret(v128_t, extend<uint128_t>(v));
                        break;
                    }
                    case 93: {
                        load("v128.load64_zero", uint64_t, uint64_t);
                        args1(uint64_t, v);
                        ret(v128_t, extend<uint128_t>(v));
                        break;
                    }
                    case 94: {
                        trace("f32x4.demote_f64x2_zero");
                        vargs1(float64_t, lanes);
                        vret(float32_t)({fdemote<float32_t>(lanes[0]), fdemote<float32_t>(lanes[1]), fdemote<float32_t>(0), fdemote<float32_t>(0)});
                        break;
                    }
                    case 95: {
                        trace("f64x2.promote_low_f32x4");
                        vargs1(float32_t, lanes);
                        vret(float64_t)({fpromote<float64_t>(lanes[0]), fpromote<float64_t>(lanes[1])});
                        break;
                    }
                    case 96: {
                        vapply("i8x16.abs", int8_t, std::abs);
                        break;
                    }
                    case 97: {
                        vunop("i8x16.neg", int8_t, -);
                        break;
                    }
                    case 98: {
                        vapply("i8x16.popcnt", uint8_t, popcnt<uint8_t>);
                        break;
                    }
                    case 99: {
                        vcmpk("i8x16.all_true", uint8_t, ==, 0);
                        cmpk(nullptr, v128_t, ==, 0);
                        break;
                    }
                    case 100: {
                        vmask("i8x16.bitmask", int8_t, vv, uint32_t, vv < 0 ? 1 : 0);
                        break;
                    }
                    case 101: {
                        vmap2("i8x16.narrow_i16x8_s", int8_t, sat<int8_t>, int16_t);
                        break;
                    }
                    case 102: {
                        vmap2("i8x16.narrow_i16x8_u", uint8_t, sat<uint8_t>, int16_t);
                        break;
                    }
                    case 103: {
                        vapply("f32x4.ceil", float32_t, fceil<float32_t>);
                        break;
                    }
                    case 104: {
                        vapply("f32x4.floor", float32_t, ffloor<float32_t>);
                        break;
                    }
                    case 105: {
                        vapply("f32x4.trunc", float32_t, ftrunc<float32_t>);
                        break;
                    }
                    case 106: {
                        vapply("f32x4.nearest", float32_t, fnearest<float32_t>);
                        break;
                    }
                    case 107: {
                        vapply2a("i8x16.shl", uint8_t, shl<uint8_t>, uint32_t);
                        break;
                    }
                    case 108: {
                        vapply2a("i8x16.shr_s", int8_t, shr<int8_t>, uint32_t);
                        break;
                    }
                    case 109: {
                        vapply2a("i8x16.shr_u", uint8_t, shr<uint8_t>, uint32_t);
                        break;
                    }
                    case 110: {
                        vbinop("i8x16.add", uint8_t, +);
                        break;
                    }
                    case 111: {
                        vapply2("i8x16.add_sat_s", int8_t, add_sat<int8_t>);
                        break;
                    }
                    case 112: {
                        vapply2("i8x16.add_sat_u", int8_t, add_sat<uint8_t>);
                        break;
                    }
                    case 113: {
                        vbinop("i8x16.sub", uint8_t, -);
                        break;
                    }
                    case 114: {
                        vapply2("i8x16.sub_sat_s", int8_t, sub_sat<int8_t>);
                        break;
                    }
                    case 115: {
                        vapply2("i8x16.sub_sat_u", int8_t, sub_sat<uint8_t>);
                        break;
                    }
                    case 116: {
                        vapply("f64x2.ceil", float64_t, fceil<float64_t>);
                        break;
                    }
                    case 117: {
                        vapply("f64x2.floor", float64_t, ffloor<float64_t>);
                        break;
                    }
                    case 118: {
                        vapply2("i8x16.min_s", int8_t, std::min<int8_t>);
                        break;
                    }
                    case 119: {
                        vapply2("i8x16.min_u", uint8_t, std::min<uint8_t>);
                        break;
                    }
                    case 120: {
                        vapply2("i8x16.max_s", int8_t, std::max<int8_t>);
                        break;
                    }
                    case 121: {
                        vapply2("i8x16.max_u", uint8_t, std::max<uint8_t>);
                        break;
                    }
                    case 122: {
                        vapply("f64x2.trunc", float64_t, ftrunc<float64_t>);
                        break;
                    }
                    case 123: {
                        vapply2("i8x16.avgr_u", uint8_t, avgr<uint8_t>);
                        break;
                    }
                    case 124: {
                        trace("i16x8.extadd_pairwise_i8x16_s");
                        vargs1(int8_t, lanes);
                        vlanes_t<int16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int16_t>(lanes[2*i]) + extend<int16_t>(lanes[2*i + 1]);
                        vret(int16_t)(olanes);
                        break;
                    }
                    case 125: {
                        trace("i16x8.extadd_pairwise_i8x16_u");
                        vargs1(uint8_t, lanes);
                        vlanes_t<uint16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint16_t>(lanes[2*i]) + extend<uint16_t>(lanes[2*i + 1]);
                        vret(uint16_t)(olanes);
                        break;
                    }
                    case 126: {
                        trace("i32x4.extadd_pairwise_i16x8_s");
                        vargs1(int16_t, lanes);
                        vlanes_t<int32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int32_t>(lanes[2*i]) + extend<int32_t>(lanes[2*i + 1]);
                        vret(int32_t)(olanes);
                        break;
                    }
                    case 127: {
                        trace("i32x4.extadd_pairwise_i16x8_u");
                        vargs1(uint16_t, lanes);
                        vlanes_t<uint32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint32_t>(lanes[2*i]) + extend<uint32_t>(lanes[2*i + 1]);
                        vret(uint32_t)(olanes);
                        break;
                    }
                    case 128: {
                        vapply("i16x8.abs", int16_t, std::abs);
                        break;
                    }
                    case 129: {
                        vunop("i16x8.neg", int16_t, -);
                        break;
                    }
                    case 130: {
                        vapply2("i16x8.q15mulr_sat_s", int16_t, qmulr_sat<int16_t>);
                        break;
                    }
                    case 131: {
                        vcmpk("i16x8.all_true", uint16_t, ==, 0);
                        cmpk(nullptr, v128_t, ==, 0);
                        break;
                    }
                    case 132: {
                        vmask("i16x8.bitmask", int16_t, vv, uint32_t, vv < 0 ? 1 : 0);
                        break;
                    }
                    case 133: {
                        vmap2("i16x8.narrow_i32x4_s", int16_t, sat<int16_t>, int32_t);
                        break;
                    }
                    case 134: {
                        vmap2("i16x8.narrow_i32x4_u", uint16_t, sat<uint16_t>, int32_t);
                        break;
                    }
                    case 135: {
                        trace("i16x8.extend_low_i8x16_s");
                        vargs1(int8_t, lanes);
                        vret(int16_t)({extend<int16_t>(lanes[0]), extend<int16_t>(lanes[1]), extend<int16_t>(lanes[2]), extend<int16_t>(lanes[3]), extend<int16_t>(lanes[4]), extend<int16_t>(lanes[5]), extend<int16_t>(lanes[6]), extend<int16_t>(lanes[7])});
                        break;
                    }
                    case 136: {
                        trace("i16x8.extend_high_i8x16_s");
                        vargs1(int8_t, lanes);
                        vret(int16_t)({extend<int16_t>(lanes[8]), extend<int16_t>(lanes[9]), extend<int16_t>(lanes[10]), extend<int16_t>(lanes[11]), extend<int16_t>(lanes[12]), extend<int16_t>(lanes[13]), extend<int16_t>(lanes[14]), extend<int16_t>(lanes[15])});
                        break;
                    }
                    case 137: {
                        trace("i16x8.extend_low_i8x16_u");
                        vargs1(uint8_t, lanes);
                        vret(uint16_t)({extend<uint16_t>(lanes[0]), extend<uint16_t>(lanes[1]), extend<uint16_t>(lanes[2]), extend<uint16_t>(lanes[3]), extend<uint16_t>(lanes[4]), extend<uint16_t>(lanes[5]), extend<uint16_t>(lanes[6]), extend<uint16_t>(lanes[7])});
                        break;
                    }
                    case 138: {
                        trace("i16x8.extend_high_i8x16_u");
                        vargs1(uint8_t, lanes);
                        vret(uint16_t)({extend<uint16_t>(lanes[8]), extend<uint16_t>(lanes[9]), extend<uint16_t>(lanes[10]), extend<uint16_t>(lanes[11]), extend<uint16_t>(lanes[12]), extend<uint16_t>(lanes[13]), extend<uint16_t>(lanes[14]), extend<uint16_t>(lanes[15])});
                        break;
                    }
                    case 139: {
                        vapply2a("i16x8.shl", uint16_t, shl<uint16_t>, uint32_t);
                        break;
                    }
                    case 140: {
                        vapply2a("i16x8.shr_s",int16_t, shr<int16_t>,  uint32_t);
                        break;
                    }
                    case 141: {
                        vapply2a("i16x8.shr_u", uint16_t, shr<uint16_t>, uint32_t);
                        break;
                    }
                    case 142: {
                        vbinop("i16x8.add", uint16_t, +);
                        break;
                    }
                    case 143: {
                        vapply2("i16x8.add_sat_s", int16_t, add_sat<int16_t>);
                        break;
                    }
                    case 144: {
                        vapply2("i16x8.add_sat_u", int16_t, add_sat<uint16_t>);
                        break;
                    }
                    case 145: {
                        vbinop("i16x8.sub", uint16_t, -);
                        break;
                    }
                    case 146: {
                        vapply2("i16x8.sub_sat_s", int16_t, sub_sat<int16_t>);
                        break;
                    }
                    case 147: {
                        vapply2("i16x8.sub_sat_u", int16_t, sub_sat<uint16_t>);
                        break;
                    }
                    case 148: {
                        vapply("f64x2.nearest", float64_t, fnearest<float64_t>);
                        break;
                    }
                    case 149: {
                        vbinop("i16x8.mul", uint16_t, *);
                        break;
                    }
                    case 150: {
                        vapply2("i16x8.min_s", int16_t, std::min<int16_t>);
                        break;
                    }
                    case 151: {
                        vapply2("i16x8.min_u", uint16_t, std::min<uint16_t>);
                        break;
                    }
                    case 152: {
                        vapply2("i16x8.max_s", int16_t, std::max<int16_t>);
                        break;
                    }
                    case 153: {
                        vapply2("i16x8.max_u", uint16_t, std::max<uint16_t>);
                        break;
                    }
                    case 155: {
                        vapply2("i16x8.avgr_u", uint16_t, avgr<uint16_t>);
                        break;
                    }
                    case 156: {
                        trace("i16x8.extmul_low_i8x16_s");
                        vargs2(int8_t, lanes1, lanes2);
                        vlanes_t<int16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int16_t>(lanes1[i]) * extend<int16_t>(lanes2[i]);
                        vret(int16_t)(olanes);
                        break;
                    }
                    case 157: {
                        trace("i16x8.extmul_high_i8x16_s");
                        vargs2(int8_t, lanes1, lanes2);
                        vlanes_t<int16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int16_t>(lanes1[olanes.size() + i]) * extend<int16_t>(lanes2[olanes.size() + i]);
                        vret(int16_t)(olanes);
                        break;
                    }
                    case 158: {
                        trace("i16x8.extmul_low_i8x16_u");
                        vargs2(uint8_t, lanes1, lanes2);
                        vlanes_t<uint16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint16_t>(lanes1[i]) * extend<uint16_t>(lanes2[i]);
                        vret(uint16_t)(olanes);
                        break;
                    }
                    case 159: {
                        trace("i16x8.extmul_high_i8x16_u");
                        vargs2(uint8_t, lanes1, lanes2);
                        vlanes_t<uint16_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint16_t>(lanes1[olanes.size() + i]) * extend<uint16_t>(lanes2[olanes.size() + i]);
                        vret(uint16_t)(olanes);
                        break;
                    }
                    case 160: {
                        vapply("i32x4.abs", int32_t, std::abs);
                        break;
                    }
                    case 161: {
                        vunop("i32x4.neg", int32_t, -);
                        break;
                    }
                    case 163: {
                        vcmpk("i32x4.all_true", uint32_t, ==, 0);
                        cmpk(nullptr, v128_t, ==, 0);
                        break;
                    }
                    case 164: {
                        vmask("i32x4.bitmask", int32_t, vv, uint32_t, vv < 0 ? 1 : 0);
                        break;
                    }
                    case 167: {
                        trace("i32x4.extend_low_i16x8_s");
                        vargs1(int16_t, lanes);
                        vret(int32_t)({extend<int32_t>(lanes[0]), extend<int32_t>(lanes[1]), extend<int32_t>(lanes[2]), extend<int32_t>(lanes[3])});
                        break;
                    }
                    case 168: {
                        trace("i32x4.extend_high_i16x8_s");
                        vargs1(int16_t, lanes);
                        vret(int32_t)({extend<int32_t>(lanes[4]), extend<int32_t>(lanes[5]), extend<int32_t>(lanes[6]), extend<int32_t>(lanes[7])});
                        break;
                    }
                    case 169: {
                        trace("i32x4.extend_low_i16x8_u");
                        vargs1(uint16_t, lanes);
                        vret(uint32_t)({extend<uint32_t>(lanes[0]), extend<uint32_t>(lanes[1]), extend<uint32_t>(lanes[2]), extend<uint32_t>(lanes[3])});
                        break;
                    }
                    case 170: {
                        trace("i32x4.extend_high_i16x8_u");
                        vargs1(uint16_t, lanes);
                        vret(uint32_t)({extend<uint32_t>(lanes[4]), extend<uint32_t>(lanes[5]), extend<uint32_t>(lanes[6]), extend<uint32_t>(lanes[7])});
                        break;
                    }
                    case 171: {
                        vapply2a("i32x4.shl", uint32_t, shl<uint32_t>, uint32_t);
                        break;
                    }
                    case 172: {
                        vapply2a("i32x4.shr_s", int32_t, shr<int32_t>, uint32_t);
                        break;
                    }
                    case 173: {
                        vapply2a("i32x4.shr_u", uint32_t, shr<uint32_t>, uint32_t);
                        break;
                    }
                    case 174: {
                        vbinop("i32x4.add", uint32_t, +);
                        break;
                    }
                    case 177: {
                        vbinop("i32x4.sub", uint32_t, -);
                        break;
                    }
                    case 181: {
                        vbinop("i32x4.mul", uint32_t, *);
                        break;
                    }
                    case 182: {
                        vapply2("i32x4.min_s", int32_t, std::min<int32_t>);
                        break;
                    }
                    case 183: {
                        vapply2("i32x4.min_u", uint32_t, std::min<uint32_t>);
                        break;
                    }
                    case 184: {
                        vapply2("i32x4.max_s", int32_t, std::max<int32_t>);
                        break;
                    }
                    case 185: {
                        vapply2("i32x4.max_u", uint32_t, std::max<uint32_t>);
                        break;
                    }
                    case 186: {
                        trace("i32x4.dot_i16x8_s");
                        vargs2(int16_t, lanes1, lanes2);

                        vlanes_t<int32_t, 8> temp;
                        static_assert(temp.size() == lanes1.size() and temp.size() == lanes2.size());
                        for (auto i = 0; i < temp.size(); ++i) temp[i] = extend<int32_t>(lanes1[i]) * extend<int32_t>(lanes2[i]);

                        vlanes_t<int32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = temp[2*i] + temp[2*i+1];
                        vret(int32_t)(olanes);
                        break;
                    }
                    case 188: {
                        trace("i32x4.extmul_low_i16x8_s");
                        vargs2(int16_t, lanes1, lanes2);
                        vlanes_t<int32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int32_t>(lanes1[i]) * extend<int32_t>(lanes2[i]);
                        vret(int32_t)(olanes);
                        break;
                    }
                    case 189: {
                        trace("i32x4.extmul_high_i16x8_s");
                        vargs2(int16_t, lanes1, lanes2);
                        vlanes_t<int32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int32_t>(lanes1[olanes.size() + i]) * extend<int32_t>(lanes2[olanes.size() + i]);
                        vret(int32_t)(olanes);
                        break;
                    }
                    case 190: {
                        trace("i32x4.extmul_low_i16x8_u");
                        vargs2(uint16_t, lanes1, lanes2);
                        vlanes_t<uint32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint32_t>(lanes1[i]) * extend<uint32_t>(lanes2[i]);
                        vret(uint32_t)(olanes);
                        break;
                    }
                    case 191: {
                        trace("i32x4.extmul_high_i16x8_u");
                        vargs2(uint16_t, lanes1, lanes2);
                        vlanes_t<uint32_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint32_t>(lanes1[olanes.size() + i]) * extend<uint32_t>(lanes2[olanes.size() + i]);
                        vret(uint32_t)(olanes);
                        break;
                    }
                    case 192: {
                        vapply("i64x2.abs", int64_t, std::abs);
                        break;
                    }
                    case 193: {
                        vunop("i64x2.neg", int64_t, -);
                        break;
                    }
                    case 195: {
                        vcmpk("i64x2.all_true", uint64_t, ==, 0);
                        cmpk(nullptr, v128_t, ==, 0);
                        break;
                    }
                    case 196: {
                        vmask("i64x2.bitmask", int64_t, vv, uint32_t, vv < 0 ? 1 : 0);
                        break;
                    }
                    case 199: {
                        trace("i64x2.extend_low_i32x4_s");
                        vargs1(int32_t, lanes);
                        vret(int64_t)({extend<int64_t>(lanes[0]), extend<int64_t>(lanes[1])});
                        break;
                    }
                    case 200: {
                        trace("i64x2.extend_high_i32x4_s");
                        vargs1(int32_t, lanes);
                        vret(int64_t)({extend<int64_t>(lanes[2]), extend<int64_t>(lanes[3])});
                        break;
                    }
                    case 201: {
                        trace("i64x2.extend_low_i32x4_u");
                        vargs1(uint32_t, lanes);
                        vret(uint64_t)({extend<uint64_t>(lanes[0]), extend<uint64_t>(lanes[1])});
                        break;
                    }
                    case 202: {
                        trace("i64x2.extend_high_i32x4_u");
                        vargs1(uint32_t, lanes);
                        vret(uint64_t)({extend<uint64_t>(lanes[2]), extend<uint32_t>(lanes[3])});
                        break;
                    }

                    case 203: {
                        vapply2a("i64x2.shl", uint64_t, shl<uint64_t>, uint32_t);
                        break;
                    }
                    case 204: {
                        vapply2a("i64x2.shr_s", int64_t, shr<int64_t>, uint32_t);
                        break;
                    }
                    case 205: {
                        vapply2a("i64x2.shr_u", uint64_t, shr<uint64_t>, uint32_t);
                        break;
                    }
                    case 206: {
                        vbinop("i64x2.add", uint64_t, +);
                        break;
                    }
                    case 209: {
                        vbinop("i64x2.sub", uint64_t, -);
                        break;
                    }
                    case 213: {
                        vbinop("i64x2.mul", uint64_t, *);
                        break;
                    }
                    case 214: {
                        vcmp("i64x2.eq", uint64_t, ==);
                        break;
                    }
                    case 215: {
                        vcmp("i64x2.ne", uint64_t, !=);
                        break;
                    }
                    case 216: {
                        vcmp("i64x2.lt_s", int64_t, <);
                        break;
                    }
                    case 217: {
                        vcmp("i64x2.gt_s", int64_t, >);
                        break;
                    }
                    case 218: {
                        vcmp("i64x2.le_s", int64_t, <=);
                        break;
                    }
                    case 219: {
                        vcmp("i64x2.ge_s", int64_t, >=);
                        break;
                    }
                    case 220: {
                        trace("i64x2.extmul_low_i32x2_s");
                        vargs2(int32_t, lanes1, lanes2);
                        vlanes_t<int64_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int64_t>(lanes1[i]) * extend<int64_t>(lanes2[i]);
                        vret(int64_t)(olanes);
                        break;
                    }
                    case 221: {
                        trace("i64x2.extmul_high_i32x4_s");
                        vargs2(int32_t, lanes1, lanes2);
                        vlanes_t<int64_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<int64_t>(lanes1[olanes.size() + i]) * extend<int64_t>(lanes2[olanes.size() + i]);
                        vret(int64_t)(olanes);
                        break;
                    }
                    case 222: {
                        trace("i64x2.extmul_low_i32x4_u");
                        vargs2(uint32_t, lanes1, lanes2);
                        vlanes_t<uint64_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint64_t>(lanes1[i]) * extend<uint64_t>(lanes2[i]);
                        vret(uint64_t)(olanes);
                        break;
                    }
                    case 223: {
                        trace("i64x2.extmul_high_i32x4_u");
                        vargs2(uint32_t, lanes1, lanes2);
                        vlanes_t<uint64_t> olanes;
                        for (auto i = 0; i < olanes.size(); ++i) olanes[i] = extend<uint64_t>(lanes1[olanes.size() + i]) * extend<uint64_t>(lanes2[olanes.size() + i]);
                        vret(uint64_t)(olanes);
                        break;
                    }
                    case 224: {
                        vapply("f32x4.abs", float32_t, fabs<float32_t>);
                        break;
                    }
                    case 225: {
                        vapply("f32x4.neg", float32_t, fneg<float32_t>);
                        break;
                    }
                    case 227: {
                        vapply("f32x4.sqrt", float32_t, fsqrt<float32_t>);
                        break;
                    }
                    case 228: {
                        vfbinop("f32x4.add", float32_t, +);
                        break;
                    }
                    case 229: {
                        vfbinop("f32x4.sub", float32_t, -);
                        break;
                    }
                    case 230: {
                        vfbinop("f32x4.mul", float32_t, *);
                        break;
                    }
                    case 231: {
                        vfbinop("f32x4.div", float32_t, /);
                        break;
                    }
                    case 232: {
                        vapply2("f32x4.min", float32_t, fmin<float32_t>);
                        break;
                    }
                    case 233: {
                        vapply2("f32x4.max", float32_t, fmax<float32_t>);
                        break;
                    }
                    case 234: {
                        vapply2("f32x4.pmin", float32_t, std::min<float32_t>);
                        break;
                    }
                    case 235: {
                        vapply2("f32x4.pmax", float32_t, std::max<float32_t>);
                        break;
                    }
                    case 236: {
                        vapply("f64x2.abs", float64_t, fabs<float64_t>);
                        break;
                    }
                    case 237: {
                        vapply("f64x2.neg", float64_t, fneg<float64_t>);
                        break;
                    }
                    case 239: {
                        vapply("f64x2.sqrt", float64_t, fsqrt<float64_t>);
                        break;
                    }
                    case 240: {
                        vfbinop("f64x2.add", float64_t, +);
                        break;
                    }
                    case 241: {
                        vfbinop("f64x2.sub", float64_t, -);
                        break;
                    }
                    case 242: {
                        vfbinop("f64x2.mul", float64_t, *);
                        break;
                    }
                    case 243: {
                        vfbinop("f64x2.div", float64_t, /);
                        break;
                    }
                    case 244: {
                        vapply2("f64x2.min", float64_t, fmin<float64_t>);
                        break;
                    }
                    case 245: {
                        vapply2("f64x2.max", float64_t, fmax<float64_t>);
                        break;
                    }
                    case 246: {
                        vapply2("f64x2.pmin", float64_t, std::min<float64_t>);
                        break;
                    }
                    case 247: {
                        vapply2("f64x2.pmax", float64_t, std::max<float64_t>);
                        break;
                    }
                    case 248: {
                        trace("i32x4.trunc_sat_f32x4_s");
                        vmap(int32_t, ftrunc_sat<int32_t>, vlanes_t<float32_t>);
                        break;
                    }
                    case 249: {
                        trace("i32x4.trunc_sat_f32x4_u");
                        vmap(uint32_t, ftrunc_sat<uint32_t>, vlanes_t<float32_t>);
                        break;
                    }
                    case 250: {
                        trace("f32x4.convert_i32x4_s");
                        vargs1(int32_t, ilanes);
                        vret(float32_t)({fconvert<float32_t>(ilanes[0]), fconvert<float32_t>(ilanes[1]), fconvert<float32_t>(ilanes[2]), fconvert<float32_t>(ilanes[3])});
                        break;
                    }
                    case 251: {
                        trace("f32x4.convert_i32x4_u");
                        vargs1(uint32_t, lanes);
                        vret(float32_t)({fconvert<float32_t>(lanes[0]), fconvert<float32_t>(lanes[1]), fconvert<float32_t>(lanes[2]), fconvert<float32_t>(lanes[3])});
                        break;
                    }
                    case 252: {
                        trace("i32x4.trunc_sat_f64x2_s_zero");
                        vargs1(float64_t, lanes);
                        vret(int32_t)({ftrunc_sat<int32_t>(lanes[0]), ftrunc_sat<int32_t>(lanes[1]), ftrunc_sat<int32_t>(0), ftrunc_sat<int32_t>(0)});
                        break;
                    }
                    case 253: {
                        trace("i32x4.trunc_sat_f64x2_u_zero");
                        vargs1(float64_t, lanes);
                        vret(uint32_t)({ftrunc_sat<uint32_t>(lanes[0]), ftrunc_sat<uint32_t>(lanes[1]), ftrunc_sat<uint32_t>(0), ftrunc_sat<uint32_t>(0)});
                        break;
                    }
                    case 254: {
                        trace("f64x2.convert_low_i32x4_s");
                        vargs1(int32_t, lanes);
                        vret(float64_t)({fconvert<float64_t>(lanes[0]), fconvert<float64_t>(lanes[1])});
                        break;
                    }
                    case 255: {
                        trace("f64x2.convert_low_i32x4_u");
                        vargs1(uint32_t, lanes);
                        vret(float64_t)({fconvert<float64_t>(lanes[0]), fconvert<float64_t>(lanes[1])});
                        break;
                    }
                    default:
                        return fstr("Unhandled vector opcode in expression 0xfd %d", (int) opcode);
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

