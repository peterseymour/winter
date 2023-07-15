#include "runtime.hpp"
#include "os.hpp"
#include <sys/mman.h>
#include <cmath>
#include <numeric>
#include "interfaces/default.hpp"
#include "interfaces/wasi.hpp"
#include "interfaces/wave.hpp"


bool tracing_on = false;
bool runtime_validating_on = true;


ref_t type_traits<reference_type_t>::null(reference_type_t type) {
    switch (type) {
        case reference_type_t::FUNCREF:
            return NullFunction::instance()->ref();
        case reference_type_t::EXTERNREF:
            return nullptr;
        default:
            error("Invalid reference_type_t value");
            return nullptr;
    }
}

bool type_traits<reference_type_t>::isnull(ref_t ref) {
    return ref == null(reference_type_t::FUNCREF) or ref == null(reference_type_t::EXTERNREF);
}

void segmentation_fault(byte_t* addr) {
    error("Segmentation fault at", (void*) addr);
}

void memory_out_of_bounds(byte_t* addr) {
    throw Trap{.message=MEMORY_OUT_OF_BOUNDS};
}

void data_out_of_bounds(byte_t* addr) {
    throw Trap{.message="Data access out of bounds"};
}

void table_out_of_bounds(byte_t* addr) {
    throw Trap{.message=TABLE_OUT_OF_BOUNDS};
}

void element_out_of_bounds(byte_t* addr) {
    throw Trap{.message="Element access out of bounds"};
}

void stack_underflow(byte_t* addr) {
    throw Trap{.message="call stack underflow"};
}

void stack_overflow(byte_t* addr) {
    throw Trap{.message="call stack exhausted"};
}

void function_out_of_bounds(byte_t* addr) {
    throw Trap{.message="Function access out of bounds"};
}

void global_out_of_bounds(byte_t* addr) {
    throw Trap{.message="Global access out of bounds"};
}



range_t<byte_t> malloc_blocks(uint64_t nblocks, uint64_t block_size, os::guard_handler_t lo_guard, os::guard_handler_t hi_guard, bool noerror) {
    const auto mem = os::memmap(os::page_size() + (nblocks * block_size) + os::page_size(), noerror);

    if (noerror and mem.size() == 0)
        return mem;

    os::guard(mem.begin(), mem.begin() + os::page_size(), lo_guard);
    os::guard(mem.end() - os::page_size(), mem.end(), hi_guard != nullptr ? hi_guard : lo_guard);

    return range_t<byte_t>(mem.begin() + os::page_size(), mem.end() - os::page_size());
}


range_t<byte_t> malloc_region(uint64_t region_size, os::guard_handler_t lo_guard, os::guard_handler_t hi_guard, bool noerror) {
    return malloc_blocks(os::page_count(region_size), os::page_size(), lo_guard, hi_guard, noerror);
}

void free_blocks(const range_t<byte_t>& mem) {
    os::memunmap(range_t<byte_t>(mem.begin() - os::page_size(), mem.end() + os::page_size()));
}

std::ostream& operator<<(std::ostream& os, const TypedValue& typed_value) {
    switch (typed_value.type()) {
        case value_type_t::I32:
            return os << typed_value.as<int32_t>();

        case value_type_t::I64:
            return os << typed_value.as<int64_t>();

        case value_type_t::F32: {
            const auto f32 = typed_value.as<float32_t>();

            if (std::isnan(f32))
                return os << "nan:0x" << std::hex << std::setw(8) << float_bits(f32) << std::dec << std::endl;
            else
                return os << std::hexfloat << f32;
        }
        case value_type_t::F64: {
            const auto f64 = typed_value.as<float64_t>();

            if (std::isnan(f64))
                return os << "nan:0x" << std::hex << std::setw(8) << float_bits(f64) << std::dec << std::endl;
            else
                return os << std::hexfloat << f64;
        }
        case value_type_t::V128:
            return os << typed_value.as<v128_t>();

        case value_type_t::FUNCREF: {
            const funcref_t f = typed_value.as<funcref_t>();

            os << "funcref:";
            if (f == type_traits<reference_type_t>::null(reference_type_t::FUNCREF))
                return os << "null";
            else
                return os << f;
        }
        case value_type_t::EXTERNREF: {
            const externref_t r = typed_value.as<externref_t>();

            os << "externref:";
            if (r == type_traits<reference_type_t>::null(reference_type_t::EXTERNREF))
                return os << "null";
            else
                return os << std::hex << r->typed_value() << std::dec;
        }
        default:
            error("Unhandled TypedValue output");
    }

    return os;
}

funcref_t Function::ref() const {
    if (fcall_cache == nullptr) {
        auto fcall = new FastCall;
        build_fast_call(nullptr, *fcall);
        fcall_cache = fcall;
    }

    return fcall_cache;
}

NullFunction::NullFunction()
    : Function({.instance=nullptr}, 0, nullptr, nullptr, new function_body_t{
        .body_size = 0,
        .locals = Arr<local_entry_t>{},
        .instructions = Many<instruction_t>{},
    }, nullptr)
{
}

const NullFunction* NullFunction::instance() {
    static const NullFunction* nf = new NullFunction;

    return nf;
}

void NullFunction::build_fast_call(const Instance* caller, FastCall& fcall) const {
    check(caller == nullptr, "Unexpected known caller for null function ref");

    fcall = {
        .nargs = 0,
        .nlocals = 0,
        IF_RUNTIME_VALIDATE_SWITCH(.signature = nullptr,)
        .code = fcall.inline_code,
        .inline_code = {TRAP, TRAP_UNINIT_ELEM},
    };
}

const char* NullFunction::invoke(Invoker* invoker) const {
    error("Can't invoke null function");
    return nullptr;
}

WASMFunction::WASMFunction(Instance* instance, funcidx_t funcidx, const func_sig_t* signature, const function_body_t* body)
    : Function({.instance = instance}, funcidx, nullptr, signature, body, nullptr),
      nlocals(std::accumulate(begin(body->locals), end(body->locals), 0, [](uint32_t acc, auto local) {return acc + local->count;})),
      auxs(nullptr)
{
}

#include "auxillary.cpp" //WASMFunction::build_auxillary

void WASMFunction::build_fast_call(const Instance* caller, FastCall& fcall) const {
    if (caller == owning.instance) {
        if (auxs == nullptr) {
            fcall = {
                .nargs = (uint32_t) signature->params.size,
                .nlocals = nlocals,
                IF_RUNTIME_VALIDATE_SWITCH(.signature = signature,)
                .code = fcall.inline_code,
                .inline_code = {TRAP, TRAP_GUARD},
            };
        } else {
            fcall = {
                .nargs = (uint32_t) signature->params.size,
                .nlocals = nlocals,
                IF_RUNTIME_VALIDATE_SWITCH(.signature = signature,)
                .code = body->instructions.begin,
                .auxs = auxs,
            };
        }
    } else {
        const auto& far_fcall = owning.instance->fast_call[funcidx];

        fcall = {
            .nargs = (uint32_t) signature->params.size,
            .nlocals = nlocals,
            IF_RUNTIME_VALIDATE_SWITCH(.signature = signature,)
            .code = (const byte_t*) new uint64_t[2]{(((uint64_t) &far_fcall) << 16) | (TRAP_CALL_FAR << 8) | TRAP, (uint64_t) owning.instance},
        };
    }
}

const char* WASMFunction::invoke(Invoker* invoker) const {
    return invoker->invoke(this);
}

HostFunction::HostFunction(Interface* interface, const identifier_t* module_name, const identifier_t* name, const func_sig_t* signature, HostFn host_fn)
    : Function({.interface = interface}, -1, name, signature, nullptr, host_fn) {
}

const char* HostFunction::invoke(Invoker* invoker) const {
    return invoker->invoke(this);
}

void HostFunction::build_fast_call(const Instance* caller, FastCall& fcall) const {
    fcall = {
        //interface call will manage its own frame
        .nargs = 0,
        .nlocals = 0,
        IF_RUNTIME_VALIDATE_SWITCH(.signature = signature,)
        .code = fcall.inline_code,
        .raw = (((uint64_t) host_fn) << 16) | (TRAP_CALL_HOST << 8) | TRAP,
    };
}

Memory::Memory(size_t min_npages, size_t max_npages, os::guard_handler_t guard)
    : min_npages(min_npages), max_npages(max_npages), buffer(malloc_blocks(min_npages, WASM_PAGE_SIZE, guard, guard, true))
{
    check(buffer.size() == min_npages * WASM_PAGE_SIZE, "Memory allocation size failure");
}

bool Memory::grow(size_t npages, os::guard_handler_t guard) {
    const auto new_npages = size_in_pages() + npages;

    if (new_npages <= max_npages) {
        auto new_buffer = malloc_blocks(new_npages, WASM_PAGE_SIZE, guard, guard, true);

        if (new_buffer.size() == new_npages * WASM_PAGE_SIZE) {
            std::copy(buffer.begin(), buffer.end(), new_buffer.begin());
            destroy();
            buffer = new_buffer;

            return true;
        }
    }

    return false;
}

void Memory::destroy() {
    free_range(buffer);
}

Table::Table(reference_type_t elemtype, size_t min_nelems, size_t max_nelems, ref_t initial, os::guard_handler_t guard)
    : elemtype(elemtype), min_nelems(min_nelems), max_nelems(max_nelems), elems(malloc_array<ref_t>(min_nelems, guard, guard, true)) {

    check(elems.size() == min_nelems, "Table allocation size failure");

    std::fill(elems.begin(), elems.end(), initial);
}

bool Table::grow(size_t nelems, os::guard_handler_t guard, ref_t initial) {
    const auto new_nelems = size() + nelems;

    if (new_nelems <= max_nelems) {
        auto new_elems = malloc_array<ref_t>(new_nelems, guard, guard, true);

        if (new_elems.size() == new_nelems) {
            std::fill(
                std::copy(elems.begin(), elems.end(), new_elems.begin()),
                new_elems.end(), initial);

            destroy();
            elems = new_elems;
            return true;
        }
    }
    return false;
}

void Table::destroy() {
    free_range(elems);
}

ValueStack::ValueStack(const range_t<Elem>& mem)
    : begin(mem.begin()), end(mem.end()), sp(end) {
}

void ValueStack::debug(const Elem* fp) const {
    check(sp >= begin, "Invalid stack stack");

    std::cout << "{";
    for (auto p = end; p != sp; --p) {
        if (p != end)
            std::cout << ' ';
        if (p == fp)
            std::cout << '|';
        std::cout << (p-1)->as<int32_t>();
    }
    std::cout << "}\n";
}

CallStack::CallStack(const range_t<Elem>& mem)
    : begin(mem.begin()), end(mem.end()), sp(end) {
}

Function* External::import_function(const identifier_t& name, const func_sig_t& signature) const {
    auto fn = get_function(name);

    if (fn == nullptr)
        error(exists(name) ? "incompatible import type, expected function" : "unknown import, function", this->name(), ":", name);

    if (signature != *fn->signature)
        error("incompatible import type supplied for function ", this->name(), ":", name, *fn->signature, ", found", signature);

    return fn;
}

Table* External::import_table(const identifier_t& name, reference_type_t elemtype, size_t min_nelems, size_t max_nelems) const {
    auto tbl = get_table(name);

    if (tbl == nullptr)
        error(exists(name) ? "incompatible import type, expected table" : "unknown import, table", this->name(), ":", name);

    if (elemtype != tbl->elemtype or tbl->size() < min_nelems or max_nelems < tbl->max_nelems)
        error("incompatible import type supplied for table ", this->name(), ":", name, tbl->elemtype, tbl->min_nelems, tbl->max_nelems, ", found", elemtype, min_nelems, max_nelems);

    return tbl;
}

Memory* External::import_memory(const identifier_t& name, size_t min_npages, size_t max_npages) const {
    auto mem = get_memory(name);

    if (mem == nullptr)
        error(exists(name) ? "incompatible import type, expected memory" : "unknown import, memory", this->name(), ":", name);

    if (mem->size_in_pages() < min_npages or max_npages < mem->max_npages)
        error("incompatible import type supplied for memory ", this->name(), ":", name, mem->min_npages, mem->max_npages, ", found", min_npages, max_npages);

    return mem;
}

Global* External::import_global(const identifier_t& name, value_type_t type, bool mutability) const {
    auto gbl = get_global(name);

    if (gbl == nullptr)
        error(exists(name) ? "incompatible import type, expected global" : "unknown import, global", this->name(), ":", name);

    if (type != gbl->type() or mutability != gbl->mutability())
        error("incompatible import type supplied for global", this->name(), ":", name, gbl->type(), gbl->mutability() ? " (mut)" : "", ", found", mutability ? " (mut)" : "");

    return gbl;
}

bool Registry::contains(const identifier_t& name) {
    return registrations.find(name) != registrations.end();
}

External* Registry::add(const identifier_t& name, External* external, bool overwrite) {
    check(overwrite or not contains(name), "External already registered:", name);

    return registrations[name] = external;
}

External* Registry::lookup(const identifier_t& name) {
    auto i = registrations.find(name);

    if (i != registrations.end())
        return i->second;
    else if (name == Default::NAME)
        return registrations[name] = new Default;
    else if (name == Wasi::NAME)
        return registrations[name] = new Wasi;
    else if (name == Wave::NAME)
        return registrations[name] = new Wave;
    else {
        if (registrations.size() > 0) {
            std::cout << "-- Registrations --" << std::endl;
            for (auto e : registrations)
                std::cout << "  - " << e.first << std::endl;
        }
        error("incompatible import type,", name, " is not registered");
    }

    return nullptr;
}

ThreadPool::ThreadPool(size_t n)
    :
threads{new Thread{
    malloc_blocks(12, WASM_PAGE_SIZE, stack_overflow, stack_underflow).cast<Value>(),
    malloc_blocks(12, WASM_PAGE_SIZE, stack_overflow, stack_underflow).cast<CallContext>(),
}} {}

Thread* ThreadPool::operator[](size_t i) const {
    check(i < sizeof(threads), "Not supporting multiple threads");

    return threads[i];
}

Instance::Instance(const identifier_t& module_name, const Module* module) :
    module_name(module_name),
    module(module),
    function_space(malloc_array<Function*>(count_imports(external_kind_t::FUNCTION) + (module->function_section ? module->function_section->_.size : 0), function_out_of_bounds)),
    fast_call(malloc_array<FastCall>(count_imports(external_kind_t::FUNCTION) + (module->function_section ? module->function_section->_.size : 0), function_out_of_bounds)),
    memory_space(malloc_array<Memory*>(count_imports(external_kind_t::MEMORY) + (module->linear_memory_section ? module->linear_memory_section->_.size : 0), memory_out_of_bounds)),
    datas(malloc_array<range_t<const byte_t>>(module->data_section ? module->data_section->_.size : 0, data_out_of_bounds)),
    table_space(malloc_array<Table*>(count_imports(external_kind_t::TABLE) + (module->table_section ? module->table_section->_.size : 0), data_out_of_bounds)),
    elements(malloc_array<range_t<const ref_t>>(module->element_section ? module->element_section->_.size : 0, data_out_of_bounds)),
    global_space(malloc_array<Global*>(count_imports(external_kind_t::GLOBAL) + (module->global_section ? module->global_section->_.size : 0), global_out_of_bounds)),
    start_funcidx(std::numeric_limits<funcidx_t>::max())
{
}

size_t Instance::count_imports(external_kind_t kind) const {
    size_t count = 0;
    if (module->import_section)
        for (auto imp : module->import_section->_)
            if (imp->kind == kind)
                ++count;

    return count;
}

void Instance::read_imports(Registry& registry) {
    funcidx_t funcidx = 0;
    uint32_t tblidx = 0;
    uint32_t memidx = 0;
    uint32_t gblidx = 0;

    if (module->import_section) {
        for (auto imp : module->import_section->_) {
            const auto& module_name = imp->module_name;
            const auto& export_name = imp->export_name;

            auto* external = registry.lookup(module_name);

            switch (imp->kind) {
                case external_kind_t::FUNCTION: {
                    const auto& signature = module->type_section->_.elems[imp->function.sig_index]->func;

                    auto* fn = external->import_function(export_name, signature);

                    function_space[funcidx] = fn;
                    fn->build_fast_call(this, fast_call[funcidx]);

                    ++funcidx;
                    break;
                }
                case external_kind_t::TABLE: {
                    const auto* tbl_desc = imp->table.desc;

                    const auto min_nelems = tbl_desc->resizable->flags == 0x00 ? tbl_desc->resizable->_0x00.minimum : tbl_desc->resizable->_0x01.minimum;
                    const auto max_nelems = tbl_desc->resizable->flags == 0x00 ? std::numeric_limits<size_t>::max() : tbl_desc->resizable->_0x01.maximum;

                    table_space[tblidx] = external->import_table(export_name, tbl_desc->element_type, min_nelems, max_nelems);

                    ++tblidx;
                    break;
                }
                case external_kind_t::MEMORY: {
                    const auto* mem_desc = imp->memory.desc;

                    const auto min_npages = mem_desc->limits->flags == 0x00 ? mem_desc->limits->_0x00.minimum    : mem_desc->limits->_0x01.minimum;
                    const auto max_npages = mem_desc->limits->flags == 0x00 ? std::numeric_limits<size_t>::max() : mem_desc->limits->_0x01.maximum;

                    memory_space[memidx] = external->import_memory(export_name, min_npages, max_npages);

                    ++memidx;
                    break;
                }
                case external_kind_t::GLOBAL: {
                    const auto* gbl_desc = imp->global.desc;

                    global_space[gblidx] = external->import_global(export_name, gbl_desc->type, gbl_desc->mutability);

                    ++gblidx;
                    break;
                }
                default:
                    error("Unhandled import kind");
            }
        }
    }
}

void Instance::read_functions() {
    if (module->function_section == nullptr or module->code_section == nullptr)
        return;

    const auto& funcs = module->function_section->_;
    const auto& bodys = module->code_section->_;

    check(funcs.size == bodys.size, "Mismatch function and code section sizes");

    const auto nimports = count_imports(external_kind_t::FUNCTION);

    for (size_t i = 0; i != funcs.size; ++i) {
        const auto funcidx = nimports + i;

        const auto signature = &module->type_section->_.elems[funcs.elems[i]->sig_index]->func;
        const auto body = bodys.elems[i];

        function_space[funcidx] = new WASMFunction(this, funcidx, signature, body);
        function_space[funcidx]->build_fast_call(this, fast_call[funcidx]);
    }
}

void Instance::read_tables() {
    if (module->table_section == nullptr)
        return;

    const auto& tables = module->table_section->_;

    const auto nimports = count_imports(external_kind_t::TABLE);

    for (size_t i = 0; i != tables.size; ++i) {
        const auto tblidx = nimports + i;

        const auto* table_desc = tables.elems[i];

        const auto elemtype = table_desc->element_type;
        const auto nullref = type_traits<reference_type_t>::null(elemtype);

        const auto limits = table_desc->resizable;

        if (limits->flags == 0x00)
            table_space[tblidx] = new Table(elemtype, limits->_0x00.minimum, std::numeric_limits<size_t>::max(), nullref, table_out_of_bounds);
        else if (limits->flags == 0x01)
            table_space[tblidx] = new Table(elemtype, limits->_0x01.minimum, limits->_0x01.maximum, nullref, table_out_of_bounds);
        else
            error("Unsupported table limits");
    }
}

void Instance::read_memories() {
    if (module->linear_memory_section == nullptr)
        return;

    const auto nimports = count_imports(external_kind_t::MEMORY);

    for (size_t i = 0; i != module->linear_memory_section->_.size; ++i) {
        const auto memidx = nimports + i;

        const auto mem_desc = module->linear_memory_section->_.elems[i];
        const auto limits = mem_desc->limits;

        if (limits->flags == 0x00)
            memory_space[memidx] = new Memory(limits->_0x00.minimum, 0xffff, memory_out_of_bounds);
        else if (limits->flags == 0x01)
            memory_space[memidx] = new Memory(limits->_0x01.minimum, limits->_0x01.maximum, memory_out_of_bounds);
        else
            error("Unsupported memory limits");
    }
}

void Instance::read_globals() {
    if (module->global_section == nullptr)
        return;

    const auto& globs = module->global_section->_;

    const auto nimports = count_imports(external_kind_t::GLOBAL);

    for (size_t i = 0; i != globs.size; ++i) {
        const auto globidx = nimports + i;
        const auto* glob_decl = globs.elems[i];
        const auto globtype = glob_decl->desc->type;
        const bool mutability = glob_decl->desc->mutability;
        const auto expr = glob_decl->init->instruction.begin;

        switch (globtype) {
            case value_type_t::I32:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::I32>(expr));
                break;
            case value_type_t::I64:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::I64>(expr));
                break;
            case value_type_t::F32:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::F32>(expr));
                break;
            case value_type_t::F64:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::F64>(expr));
                break;
            case value_type_t::V128:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::V128>(expr));
                break;
            case value_type_t::FUNCREF:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::FUNCREF>(expr));
                break;
            case value_type_t::EXTERNREF:
                global_space[globidx] = new Global(globtype, mutability, evaluate<value_type_t::EXTERNREF>(expr));
                break;
            default:
                error("Unexpected global type", globtype);
        }
    }
}

void Instance::read_elements() {
    static constexpr auto PASSIVE_OR_DECL_FLAG = 1 << 0;
    static constexpr auto TABLE_INDEX_FLAG = 1 << 1;
    static constexpr auto ELEMENT_EXPRS_FLAG = 1 << 2;

    if (module->element_section == nullptr)
        return;

    auto eval_consts = [&](const Arr<varuint32_t>& consts, reference_type_t elemtype) {
        auto vals = malloc_array<ref_t>(consts.size, element_out_of_bounds);

        for (size_t j = 0; j != consts.size; ++j) {
            if (elemtype == reference_type_t::FUNCREF)
                vals[j] = function_space[consts.elems[j]]->ref();
            else
                error("Bad const element type");
        }

        return vals;
    };

    auto eval_exprs = [&](const Arr<instantiation_time_initializer_t>& exprs, reference_type_t elemtype) {
        auto vals = malloc_array<ref_t>(exprs.size, element_out_of_bounds);

        for (size_t j = 0; j != exprs.size; ++j) {
            const auto expr = exprs.elems[j]->instruction.begin;

            switch (elemtype) {
                case reference_type_t::FUNCREF:
                    vals[j] = evaluate<value_type_t::FUNCREF>(expr);
                    break;
                case reference_type_t::EXTERNREF:
                    vals[j] = evaluate<value_type_t::EXTERNREF>(expr);
                    break;
                default:
                    error("Unexpected element type", elemtype);
            }
        }

        return vals;
    };

    for (size_t i = 0; i != module->element_section->_.size; ++i) {
        const auto* elem_init = module->element_section->_.elems[i];

        uint32_t tblidx = 0;
        const byte_t* offset_expr = nullptr;
        reference_type_t elemtype;
        range_t<const ref_t> elem_values;

        if (elem_init->flags == 0x00) {
            offset_expr = elem_init->_0x00.offset->instruction.begin;
            elemtype = reference_type_t::FUNCREF;
            elem_values = eval_consts(elem_init->_0x00.elems, elemtype);
        } else if (elem_init->flags == 0x01) {
            check(elem_init->_0x01.elemkind == 0x00, "Unexpected element kind");
            elemtype = reference_type_t::FUNCREF;
            elem_values = eval_consts(elem_init->_0x01.elems, elemtype);
        } else if (elem_init->flags == 0x02) {
            tblidx = elem_init->_0x02.tableidx;
            offset_expr = elem_init->_0x02.offset->instruction.begin;
            check(elem_init->_0x02.elemkind == 0x00, "Unexpected element kind");
            elemtype = reference_type_t::FUNCREF;
            elem_values = eval_consts(elem_init->_0x02.elems, elemtype);
        } else if (elem_init->flags == 0x03) {
            check(elem_init->_0x03.elemkind == 0x00, "Unexpected element kind");
            elemtype = reference_type_t::FUNCREF;
            elem_values = eval_consts(elem_init->_0x03.elems, elemtype);
        } else if (elem_init->flags == 0x04) {
            offset_expr = elem_init->_0x04.offset->instruction.begin;
            elemtype = reference_type_t::FUNCREF;
            elem_values = eval_exprs(elem_init->_0x04.elems, elemtype);
        } else if (elem_init->flags == 0x05) {
            elemtype = elem_init->_0x05.elemtype;
            elem_values = eval_exprs(elem_init->_0x05.elems, elemtype);
        } else if (elem_init->flags == 0x06) {
            tblidx = elem_init->_0x06.tableidx;
            offset_expr = elem_init->_0x06.offset->instruction.begin;
            elemtype = elem_init->_0x06.elemtype;
            elem_values = eval_exprs(elem_init->_0x06.elems, elemtype);
        } else if (elem_init->flags == 0x07) {
            elemtype = elem_init->_0x07.elemtype;
            elem_values = eval_exprs(elem_init->_0x07.elems, elemtype);
        } else {
            error("Unhandled table initializer type", elem_init->flags);
        }

        check(elemtype == reference_type_t::FUNCREF or elemtype == reference_type_t::EXTERNREF, "Unexpected element type");

        if (elem_init->flags & PASSIVE_OR_DECL_FLAG) {
            check(offset_expr == nullptr, "Passive/declarative elements has offset expression");
            if (elem_init->flags & (1 << 1))
                ; //declarative
            else
                elements[i] = elem_values; //passive
        }
        else { //active
            const uint32_t offset = offset_expr != nullptr ? evaluate<value_type_t::I32>(offset_expr) : 0;

            //table.init
            check(offset + elem_values.size() <= table_space[tblidx]->size(), TABLE_OUT_OF_BOUNDS);

            auto p = &(*table_space[tblidx])[offset];
            for (auto elemval : elem_values)
                *p++ = elemval;

            //elem.drop
            elements[i] = range_t<const ref_t>();
        }
    }
}

void Instance::read_datas() {
    static constexpr auto PASSIVE_FLAG = 1 << 0;
    static constexpr auto MEMIDX_FLAG = 1 << 1;

    if (module->data_section == nullptr)
        return;

    for (size_t i = 0; i != module->data_section->_.size; ++i) {
        const auto* data_init = module->data_section->_.elems[i];

        uint32_t memidx = 0;
        const byte_t* offset_expr = nullptr;
        range_t<const byte_t> data_bytes;

        if (data_init->flags == 0x00) {
            offset_expr = data_init->_0x00.offset->instruction.begin;
            data_bytes = range_t<const byte_t>(begin(data_init->_0x00.data), end(data_init->_0x00.data));
        } else if (data_init->flags == 0x01) {
            data_bytes = range_t<const byte_t>(begin(data_init->_0x01.data), end(data_init->_0x01.data));
        } else if (data_init->flags == 0x02) {
            memidx = data_init->_0x02.memidx;
            offset_expr = data_init->_0x02.offset->instruction.begin;
            data_bytes = range_t<const byte_t>(begin(data_init->_0x02.data), end(data_init->_0x02.data));
        } else {
            error("Unhandled data initializer type");
        }

        check(memidx == 0, "Data initializers only support initializing default memory instance");

        if (data_init->flags & PASSIVE_FLAG) {
            check(offset_expr == nullptr, "Passive data has offset expression");
            datas[i] = data_bytes;
        }
        else { //active
            auto& mem = *memory_space[memidx];

            const auto n = data_bytes.size();
            const uint32_t d = offset_expr != nullptr ? evaluate<value_type_t::I32>(offset_expr) : 0;

            //memory.init
            check(d + n <= mem.size(), MEMORY_OUT_OF_BOUNDS);

            std::copy(data_bytes.begin(), data_bytes.end(), mem.begin() + d);

            //data.drop
            datas[i] = range_t<const byte_t>();
        }
    }
}

void Instance::read_start() {
    if (module->start_section == nullptr)
        return;

    start_funcidx = module->start_section->index;
}

const Function* Instance::init(Registry& registry) {
    read_imports(registry);

    read_functions();
    read_tables();
    read_memories();
    read_globals();

    read_elements();
    read_datas();

    read_start();

    check(memory_space.size() <= 1, "Expected no more than one memory region defined or imported");

    return start_funcidx != std::numeric_limits<funcidx_t>::max() ? function_space[start_funcidx] : nullptr;
}

block_type_t Instance::resolve_block_signature_type(const block_signature_type_t* block_sig_type) const {
    if (block_sig_type->type_value == enumval(type_encoding_t, VOID))
        return block_type_t{.nargs=0, .nrets=0};

    else if (block_sig_type->type_value < 0)
        return block_type_t{.nargs=0, .nrets=1};

    else {
        const auto fn_sig = module->type_section->_.elems[block_sig_type->type_value];
        check(fn_sig->form == signature_type_t::FUNC, "Invalid block signature");
        return block_type_t{.nargs=(uint32_t) fn_sig->func.params.size, .nrets=(uint32_t) fn_sig->func.returns.size};
    }
}

const export_t* Instance::get_export(external_kind_t kind, const identifier_t& name) const {
    if (module->export_section != nullptr)
        for (auto exp : module->export_section->_)
            if (exp->kind == kind and exp->name == name)
                return exp;

    return nullptr;
}

#define trace(instr) IF_TRACING( \
    std::cout << "eval[" << std::setw(4) << (instr_ip-base_ip) << "     ]: "; \
    std::cout << instr; \
    std::cout << std::endl; \
)

#define ileb(v)         auto v##_ = sdecode(ip); ip += v##_.size; auto v = v##_.value;
#define uleb(v)         auto v##_ = udecode(ip); ip += v##_.size; auto v = v##_.value;
#define read(typ, v)    auto v = *reinterpret_cast<const typ*>(ip); ip += sizeof(typ) / sizeof(byte_t)
#define ret(t, v)       value = TypedValue(t, v)

TypedValue Instance::interpret_const(const byte_t* code) const {
    IF_TRACE_SWITCH(
        const byte_t* base_ip = code;
        const byte_t* instr_ip = nullptr;
    )

    TypedValue value(value_type_t::I32, 0);

    const byte_t* ip = code;

    while (true) {
        IF_TRACE_SWITCH(instr_ip = ip;)

        auto opcode = *ip++;

        switch (opcode) {
            case 0x0b: {
                trace("end");
                return value;
            }
            case 0x23: {
                uleb(n)
                trace("global.get " << n);
                value = global_space[n]->typed_value();
                break;
            }
            case 0x41: {
                ileb(n)
                trace("i32.const " << n);
                ret(value_type_t::I32, (uint32_t) n);
                break;
            }
            case 0x42: {
                ileb(n)
                trace("i64.const " << n);
                ret(value_type_t::I64, (uint64_t) n);
                break;
            }
            case 0x43: {
                read(float32_t, f);
                trace("f32.const " << f);
                ret(value_type_t::F32, f);
                break;
            }
            case 0x44: {
                read(float64_t, f);
                trace("f64.const " << f);
                ret(value_type_t::F64, f);
                break;
            }
            case 0xd0: {
                ileb(t);
                trace("ref.null " << t);
                switch (t) {
                    case enumval(reference_type_t, FUNCREF):
                        ret(value_type_t::FUNCREF, (funcref_t) type_traits<reference_type_t>::null(reference_type_t::FUNCREF));
                        break;
                    case enumval(reference_type_t, EXTERNREF):
                        ret(value_type_t::EXTERNREF, (externref_t) type_traits<reference_type_t>::null(reference_type_t::EXTERNREF));
                        break;
                    default:
                        error("Bad ref.null type");
                }
                break;
            }
            case 0xd2: {
                uleb(f);
                trace("ref.func " << f);
                ret(value_type_t::I32, (uint32_t) f); //just store local index for element sections
                break;
            }
            default:
                error("Unhandled opcode in const expression", std::hex, (int) opcode);
        }
    }
}

Function* Instance::get_function(const identifier_t& name) const {
    auto exp = get_export(external_kind_t::FUNCTION, name);

    return exp ? function_space[exp->function.index] : nullptr;
}

Table* Instance::get_table(const identifier_t& name) const {
    auto exp = get_export(external_kind_t::TABLE, name);

    return exp ? table_space[exp->table.index] : nullptr;
}

Memory* Instance::get_memory(const identifier_t& name) const {
    auto exp = get_export(external_kind_t::MEMORY, name);

    return exp ? memory_space[exp->memory.index] : nullptr;
}

Global* Instance::get_global(const identifier_t& name) const {
    auto exp = get_export(external_kind_t::GLOBAL, name);

    return exp ? global_space[exp->global.index] : nullptr;
}

asm (
".global udecode\n"
"udecode:\n"
    "mov %r11, 0x8080808080808080\n"
    "mov %rax, [%rdi]\n"       //get 8 bytes

    "andn %r10, %rax, %r11\n"  //r10 = inv prefix bits
    "jz umultiword\n"          //does this word have terminating byte
    "blsmsk %r10, %r10\n"      //r10 = bytes mask

    "popcnt %rdx, %r10\n"      //rdx = #bits in mask
    "shr %rdx, 3\n"            //rdx = #bytes
    "andn %r10, %r11, %r10\n"  //r10 = payload bits mask
    "pext %rax, %rax, %r10\n"  //rax = result

    "ret\n"

"umultiword:\n"                //really just 2 more possible bytes, could handle this more specifically
    "mov %r10, %r11\n"         //all bytes needed
    "not %r10\n"               //r10 = payload bits mask
    "pext %r10, %rax, %r10\n"  //r10 = result (first word)

    "movzx %rax, word ptr [rdi+8]\n"      //get next 2 bytes

    "andn %rdi, %rax, %r11\n"  //rdi = inv prefix bits
    "blsmsk %rdi, %rdi\n"      //rdi = bytes mask

    "popcnt %rdx, %rdi\n"      //rdx = #bits in mask
    "shr %rdx, 3\n"            //rdx = #bytes
    "andn %rdi, %r11, %rdi\n"  //rdi = payload bits mask
    "pext %rax, %rax, %rdi\n"  //rax = result (second word)

    "add rdx, 8\n"             //rdx = #total bytes
    "shl %rax, 56\n"
    "or rax, %r10\n"

    "ret\n"
);



asm (
".global sdecode\n"
"sdecode:\n"
    "mov %r11, 0x8080808080808080\n"
    "mov %rax, [%rdi]\n"       //get 8 bytes

    "andn %r10, %rax, %r11\n"  //r10 = inv prefix bits
    "jz smultiword\n"          //does this word have terminating byte
    "blsmsk %r10, %r10\n"      //r10 = bytes mask

    "popcnt %rdx, %r10\n"      //rdx = #bits in mask
    "shr %rdx, 3\n"            //rdx = #bytes
    "andn %r10, %r11, %r10\n"  //r10 = payload bits mask
    "pext %rax, %rax, %r10\n"  //rax = result (unsigned)

    "pext %r10, %r10, %r10\n"  //r10 = result bit mask
    "shr %r10, 1\n"
    "not %r10\n"               //r10 = extended sign bits
    "test %r10, %rax\n"
    "cmovz %r10, %rax\n"       //apply sign bits if non-zero
    "or %rax, %r10\n"

    "ret\n"

"smultiword:\n"                //really just 2 more possible bytes, could handle this more specifically
    "mov %r10, %r11\n"         //all bytes needed
    "not %r10\n"               //r10 = payload bits mask
    "pext %r10, %rax, %r10\n"  //r10 = payloads (first word)

    "movzx %rax, word ptr [rdi+8]\n"      //get next 2 bytes

    "andn %rdi, %rax, %r11\n"  //r10 = inv prefix bits
    "blsmsk %rdi, %rdi\n"      //r10 = bytes mask

    "popcnt %rdx, %rdi\n"      //rdx = #bits in mask
    "shr %rdx, 3\n"            //rdx = #bytes
    "andn %rdi, %r11, %rdi\n"  //rdi = payload bits mask
    "pext %rax, %rax, %rdi\n"  //rax = result (unsigned)

    "pext %rdi, %rdi, %rdi\n"  //rdi = result bit mask
    "shr %rdi, 1\n"
    "not %rdi\n"               //rdi = extended sign bits
    "test %rdi, %rax\n"
    "cmovz %rdi, %rax\n"       //apply sign bits if non-zero
    "or %rax, %rdi\n"

    "add rdx, 8\n"
    "shl %rax, 56\n"
    "or rax, %r10\n"

    "ret\n"
);