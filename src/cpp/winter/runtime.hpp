#ifndef HEADER_RUNTIME
#define HEADER_RUNTIME

#include "wasm/binary.hpp"
#include "os.hpp"
#include <map>


extern bool tracing_on;

#ifdef TRACE_SWITCH
    #define IF_TRACE_SWITCH(...) __VA_ARGS__
    #define IF_TRACING(...) if (tracing_on) {__VA_ARGS__}
#else
    #define IF_TRACE_SWITCH(...)
    #define IF_TRACING(...)
#endif

extern bool runtime_validating_on;

#ifdef RUNTIME_VALIDATION
    #define IF_RUNTIME_VALIDATE_SWITCH(...) __VA_ARGS__
    #define IF_RUNTIME_VALIDATING(...) if (runtime_validating_on) {__VA_ARGS__}
#else
    #define IF_RUNTIME_VALIDATE_SWITCH(...)
    #define IF_RUNTIME_VALIDATING(...)
#endif

struct Trap {
    const char* const message;
};

#define MEMORY_OUT_OF_BOUNDS "out of bounds memory access"
#define TABLE_OUT_OF_BOUNDS "out of bounds table access"
#define INTEGER_DIVIDE_BY_ZERO "integer divide by zero"
#define INTEGER_OVERFLOW "integer overflow"

void segmentation_fault(byte_t* addr);
void memory_out_of_bounds(byte_t* addr);
void data_out_of_bounds(byte_t* addr);
void table_out_of_bounds(byte_t* addr);
void element_out_of_bounds(byte_t* addr);
void stack_underflow(byte_t* addr);
void stack_overflow(byte_t* addr);
void function_out_of_bounds(byte_t* addr);
void global_out_of_bounds(byte_t* addr);

constexpr auto WASM_PAGE_SIZE = 65536;

range_t<byte_t> malloc_blocks(uint64_t nblocks, uint64_t block_size, os::guard_handler_t lo_guard, os::guard_handler_t hi_guard=nullptr, bool noerror=false);
range_t<byte_t> malloc_region(uint64_t region_size, os::guard_handler_t lo_guard, os::guard_handler_t hi_guard=nullptr, bool noerror=false);

template<typename T>
range_t<T> malloc_array(uint64_t count, os::guard_handler_t lo_guard, os::guard_handler_t hi_guard=nullptr, bool noerror=false) {
    auto mem = malloc_region(count * sizeof(T), lo_guard, hi_guard, noerror).cast<T>();

    if (noerror and mem.size() == 0)
        return mem;

    return make_range(mem.begin(), count);
}

void free_blocks(const range_t<byte_t>& mem);

template<typename T>
void free_range(const range_t<T>& mem) {
    free_blocks(mem.template cast<byte_t>());
}

typedef uint128_t v128_t;
typedef const void* ref_t;
typedef const struct FastCall* funcref_t;
typedef const struct ExternValue* externref_t;

union Value {
    uint32_t i32;
    uint64_t i64;
    float32_t f32;
    float64_t f64;
    v128_t v128;
    ref_t ref;
    funcref_t funcref;
    externref_t externref;
    uint128_t raw;

    template<typename T>
    T as() const {
        return raw_read<T>(raw);
    }
};

static_assert(sizeof(Value) == 16, "Invalid Value size");
static_assert(sizeof(uint32_t) == 4, "Invalid uint32_t size");
static_assert(sizeof(int32_t) == 4, "Invalid int32_t size");
static_assert(sizeof(uint64_t) == 8, "Invalid uint64_t size");
static_assert(sizeof(int64_t) == 8, "Invalid int64_t size");
static_assert(sizeof(float32_t) == 4, "Invalid float32_t size");
static_assert(sizeof(float64_t) == 8, "Invalid float64_t size");
static_assert(sizeof(v128_t) == 16, "Invalid v128_t size");
static_assert(sizeof(ref_t) == 8, "Invalid ref_t size");
static_assert(sizeof(funcref_t) == 8, "Invalid funcref_t size");
static_assert(sizeof(externref_t) == 8, "Invalid externref_t size");

typedef uint64_t addr33_t;
typedef uint32_t funcidx_t;

static_assert(sizeof(addr33_t) > 4, "Invalid addr33_t size");
static_assert(sizeof(funcidx_t) == 4, "Invalid funcidx_t size");


template<typename T>
struct type_traits;

template<>
struct type_traits<reference_type_t>
{
    static ref_t null(reference_type_t type);
    static bool isnull(ref_t ref);
};

template<>
struct type_traits<value_type_t>
{
    template<value_type_t type> struct builtin;
    template<typename T>        struct of;
};

template<> struct type_traits<value_type_t>::builtin<value_type_t::I32>         {typedef uint32_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::I64>         {typedef uint64_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::F32>         {typedef float32_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::F64>         {typedef float64_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::V128>        {typedef v128_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::FUNCREF>     {typedef funcref_t type;};
template<> struct type_traits<value_type_t>::builtin<value_type_t::EXTERNREF>   {typedef externref_t type;};

template<> struct type_traits<value_type_t>::of<uint32_t>       {static constexpr auto value = value_type_t::I32;};
template<> struct type_traits<value_type_t>::of<int32_t>        {static constexpr auto value = value_type_t::I32;};
template<> struct type_traits<value_type_t>::of<uint64_t>       {static constexpr auto value = value_type_t::I64;};
template<> struct type_traits<value_type_t>::of<int64_t>        {static constexpr auto value = value_type_t::I64;};
template<> struct type_traits<value_type_t>::of<float32_t>      {static constexpr auto value = value_type_t::F32;};
template<> struct type_traits<value_type_t>::of<float64_t>      {static constexpr auto value = value_type_t::F64;};
template<> struct type_traits<value_type_t>::of<v128_t>         {static constexpr auto value = value_type_t::V128;};
template<> struct type_traits<value_type_t>::of<funcref_t>      {static constexpr auto value = value_type_t::FUNCREF;};
template<> struct type_traits<value_type_t>::of<externref_t>    {static constexpr auto value = value_type_t::EXTERNREF;};


struct TypedValue {
    value_type_t _type;
    Value _value;
public:
    TypedValue(value_type_t type, const Value& value)
        : _type(type), _value(value)
    {
    }

    template<typename T>
    TypedValue(value_type_t type, const T& value)
        : _type(type)
    {
        IF_RUNTIME_VALIDATING(check(type_traits<value_type_t>::of<T>::value == _type, "Typed value mismatch on create");)
        raw_write(_value, value);
    }

    value_type_t type() const {return _type;}
    Value value() const {return _value;}

    template<typename T>
    T as() const {
        IF_RUNTIME_VALIDATING(check(type_traits<value_type_t>::of<T>::value == _type, "Typed value mismatch on read");)
        return _value.as<T>();
    }

    void set(Value value) {_value = value;}
};

std::ostream& operator<<(std::ostream& os, const TypedValue& typed_value);


struct Instance;
struct Thread;
struct Interface;
struct Invoker;


typedef void (*HostFn)(Interface* interface, Instance&, Thread& thread);

struct aux_t {
    int32_t ip_offset;
    int32_t xp_offset;
    uint32_t nkeep;
    uint32_t ndrop;
};

static_assert(sizeof(aux_t) == 16);

struct FastCall {
    uint32_t nargs;
    uint32_t nlocals;
    IF_RUNTIME_VALIDATE_SWITCH(const func_sig_t* signature;)
    const byte_t* code;
    union {
        byte_t inline_code[8];
        uint64_t raw;
        const aux_t* auxs;
    };
};

static_assert(sizeof(FastCall) == 24 IF_RUNTIME_VALIDATE_SWITCH(+ 8));

struct Function {
    union {
        Instance* instance;
        Interface* interface;
    } owning;
    const funcidx_t funcidx;
    const identifier_t* name;
    const func_sig_t* signature;
    const function_body_t* body;
    const HostFn host_fn;
    mutable const FastCall* fcall_cache;

    Function(decltype(owning) owning, funcidx_t funcidx, const identifier_t* name, const func_sig_t* signature, const function_body_t* body, const HostFn host_fn) :
        owning(owning),
        funcidx(funcidx),
        name(name),
        signature(signature),
        body(body),
        host_fn(host_fn),
        fcall_cache(nullptr) {}

    virtual void build_fast_call(const Instance* caller, FastCall& fcall) const = 0;
    virtual const char* invoke(Invoker* invoker) const = 0;

    funcref_t ref() const;
};

struct NullFunction : Function {
    NullFunction();

    static const NullFunction* instance();

    virtual void build_fast_call(const Instance* caller, FastCall& fcall) const override;
    virtual const char* invoke(Invoker* invoker) const override;
};

struct WASMFunction : Function {
    const uint32_t nlocals;
    const aux_t* auxs;

    WASMFunction(Instance* instance, funcidx_t funcidx, const func_sig_t* signature, const function_body_t* body);

    void build_auxillary();

    virtual void build_fast_call(const Instance* caller, FastCall& fcall) const override;
    virtual const char* invoke(Invoker* invoker) const override;
};

struct HostFunction : Function {
    HostFunction(Interface* interface, const identifier_t* module_name, const identifier_t* name, const func_sig_t* signature, HostFn host_fn);

    virtual void build_fast_call(const Instance* caller, FastCall& fcall) const override;
    virtual const char* invoke(Invoker* invoker) const override;
};

class ExternValue {
    TypedValue _typed_value;
public:
    template<typename T>
    ExternValue(value_type_t type, const T& value)
        : _typed_value(type, value) {}

    const TypedValue& typed_value() const {return _typed_value;}
};

struct Invoker {
    virtual const char* invoke(const WASMFunction* fn) = 0;
    virtual const char* invoke(const HostFunction* fn) = 0;
};

struct Memory {
    const size_t min_npages;
    const size_t max_npages;
private:
    range_t<byte_t> buffer;
public:
    Memory(size_t min_npages, size_t max_npages, os::guard_handler_t guard);

    auto begin() {return buffer.begin();}
    auto end() {return buffer.end();}
    auto size() const {return buffer.size();}
    uint32_t size_in_pages() const {return size() / WASM_PAGE_SIZE;}

    bool grow(size_t npages, os::guard_handler_t guard);
    void destroy();

    template<typename S>
    S& deref(size_t n) {
        IF_RUNTIME_VALIDATING(if (n + sizeof(S) > size()) memory_out_of_bounds(buffer.begin() + n);)
        return *reinterpret_cast<S*>(buffer.begin() + n);
    }
};


struct Table {
    const reference_type_t elemtype;
    const size_t min_nelems;
    const size_t max_nelems;
private:
    range_t<ref_t> elems;

    template<typename E>
    void check_usage(reference_type_t elemtype);
public:
    Table(reference_type_t elemtype, size_t min_nelems, size_t max_nelems, ref_t initial, os::guard_handler_t guard);

    size_t size() const {return elems.size();}

    bool grow(size_t nelems, os::guard_handler_t guard, ref_t initial);
    void destroy();

    const ref_t& operator[](size_t i) const {return elems[i];}
    ref_t& operator[](size_t i) {return elems[i];}

    template<typename E>
    range_t<E> view() {
        check_usage<E>(elemtype);
        return elems.cast<E>();
    }
};

template<>
inline void Table::check_usage<funcref_t>(reference_type_t elemtype) {
    check(elemtype == reference_type_t::FUNCREF, "Using table incorrectly");
}

template<>
inline void Table::check_usage<externref_t>(reference_type_t elemtype) {
    check(elemtype == reference_type_t::EXTERNREF, "Using table incorrectly");
}

template<>
inline void Table::check_usage<ref_t>(reference_type_t elemtype) {
}

class Global {
    TypedValue _typed_value;
    const bool _mutability;
public:
    template<typename T>
    Global(value_type_t type, bool mutability, const T& value)
        : _typed_value(type, value), _mutability(mutability) {}

    Global& operator=(const Global& global) = delete;

    auto type() const {return _typed_value.type();}
    bool mutability() const {return _mutability;}
    const TypedValue& typed_value() const {return _typed_value;}

    template<typename T>
    auto get() const {
        return _typed_value.as<T>();
    }

    void set(Value value) {
        IF_RUNTIME_VALIDATING(check(_mutability, "Error setting non-mutable global");)
        _typed_value.set(value);
    }
};


struct ValueStack {
    typedef Value Elem;
private:
    Elem* begin;
    Elem* end;
    Elem* sp;
public:
    static Elem& frame_elem(Elem* fp, uint32_t index) {
        return *(fp - index - 1);
    }

    ValueStack(const range_t<Elem>& mem);

    bool empty() const {return size() == 0;}

    size_t size() const {return end - sp;}

    template<typename T>
    void push(T v) {
        static_assert(sizeof(T) <= sizeof(Elem), "Invalid push size");

        IF_RUNTIME_VALIDATING(if (sp < begin + 1) stack_overflow((byte_t*) sp);)

        *reinterpret_cast<T*>(--sp) = v;
    }

    Elem& top() {
        return *sp;
    }

    const Elem& top() const {
        return *sp;
    }

    void reserve(uint32_t count, Value dflt) {
        IF_RUNTIME_VALIDATING(if (sp < begin + count) stack_overflow((byte_t*) sp);)

        sp -= count;
        std::fill(sp, sp + count, dflt);
    }

    template<typename T=Elem>
    T pop() {
        static_assert(sizeof(T) <= sizeof(Elem), "Invalid pop size");

        return *reinterpret_cast<const T*>(sp++);
    }

    void drop(uint32_t n=1) {
        sp += n;
    }

    void move(uint32_t nkeep, uint32_t ndrop) {
        if (ndrop != 0) {
            auto src_top = sp + nkeep;

            sp += nkeep + ndrop;

            for(auto i = 0; i < nkeep; ++i)
                push<Elem>(*(--src_top));
        }
    }

    Elem* frame(uint32_t nargs, uint32_t nlocals=0) {
        auto fp = sp + nargs;
        reserve(nlocals, Value{.raw=0});
        return fp;
    }

    void debug(const Elem* fp=nullptr) const;
};


struct CallContext {
    IF_TRACE_SWITCH(const byte_t* base_ip;)
    IF_TRACE_SWITCH(const aux_t* base_xp;)
    const byte_t* ip;
    const aux_t* xp;

    union {
        Value* fp;
        Instance* instance;
    };
};


struct CallStack {
    typedef CallContext Elem;
private:
    Elem* begin;
    Elem* end;
    Elem* sp;
public:
    CallStack(const range_t<Elem>& mem);

    bool empty() const {return size() == 0;}

    size_t size() const {return end - sp;}

    void push(const Elem& elem) {
        IF_RUNTIME_VALIDATING(if (sp < begin + 1) stack_overflow((byte_t*) sp);)
        *(--sp) = elem;
    }

    Elem& top() {
        return *sp;
    }

    void pop() {
        ++sp;
    }

    const Elem& last() {
        return *(sp - 1);
    }
};


struct External {
    virtual const identifier_t& name() const = 0;

    virtual Function* get_function(const identifier_t& name) const = 0;
    virtual Table* get_table(const identifier_t& name) const = 0;
    virtual Memory* get_memory(const identifier_t& name) const = 0;
    virtual Global* get_global(const identifier_t& name) const = 0;
    virtual bool exists(const identifier_t& name) const {return get_function(name) or get_table(name) or get_memory(name) or get_global(name);}

    virtual Function* import_function(const identifier_t& name, const func_sig_t& signature) const;
    virtual Table* import_table(const identifier_t& name, reference_type_t elemtype, size_t min_nelems, size_t max_nelems) const;
    virtual Memory* import_memory(const identifier_t& name, size_t min_npages, size_t max_npages) const;
    virtual Global* import_global(const identifier_t& name, value_type_t type, bool mutability) const;

    template<typename T>
    bool is() {return dynamic_cast<T*>(this) != nullptr;}

    template<typename T>
    T* as() {
        T* t = dynamic_cast<T*>(this);
        check(t != nullptr, "Invalid external cast");
        return t;
    }
};

class Registry {
    std::map<identifier_t, External*> registrations;
public:
    bool contains(const identifier_t& name);
    External* add(const identifier_t& name, External* external, bool overwrite=false);
    External* lookup(const identifier_t& name);

    auto begin() {return registrations.begin();}
    auto end() {return registrations.end();}
};


struct Thread {
    ValueStack stack;
    CallStack call_stack;
};


class ThreadPool {
    Thread* threads[1];
public:
    ThreadPool(size_t n);
    Thread* operator[](size_t i) const;
};


struct Instance : External {
    const identifier_t module_name;
    const Module* module;

    range_t<Function*> function_space;
    range_t<FastCall> fast_call;
    range_t<Memory*> memory_space;
    range_t<range_t<const byte_t>> datas;
    range_t<Table*> table_space;
    range_t<range_t<const ref_t>> elements;
    range_t<Global*> global_space;
    funcidx_t start_funcidx;

    size_t count_imports(external_kind_t kind) const;

    void read_imports(Registry& registry);

    void read_functions();
    void read_tables();
    void read_memories();
    void read_globals();

    void read_elements();
    void read_datas();

    void read_start();

    block_type_t resolve_block_signature_type(const block_signature_type_t* block_sig_type) const;

    const export_t* get_export(external_kind_t kind, const identifier_t& name) const;

    TypedValue interpret_const(const byte_t* code) const;

    template<value_type_t valtype>
    struct Evaluator {
        static auto evaluate(Instance& instance, const byte_t* expr) {
            typedef typename type_traits<value_type_t>::builtin<valtype>::type builtintype;

            const auto typed_value = instance.interpret_const(expr);

            check(typed_value.type() == valtype, "Expression evaluated to unexpected type", typed_value.type(), "expected", valtype);

            return typed_value.as<builtintype>();
        }
    };

    template<value_type_t type>
    auto evaluate(const byte_t* expr) {
        return Evaluator<type>::evaluate(*this, expr);
    }
public:
    Instance(const identifier_t& module_name, const Module* module);

    const Function* init(Registry& registry);

    virtual const identifier_t& name() const override {return module_name;}
    virtual Function* get_function(const identifier_t& name) const override;
    virtual Table* get_table(const identifier_t& name) const override;
    virtual Memory* get_memory(const identifier_t& name) const override;
    virtual Global* get_global(const identifier_t& name) const override;
};

template<>
struct Instance::Evaluator<value_type_t::FUNCREF> {
    static funcref_t evaluate(Instance& instance, const byte_t* expr) {
        const auto typed_value = instance.interpret_const(expr);

        check(typed_value.type() == value_type_t::I32 or typed_value.type() == value_type_t::FUNCREF, "Bad funcref expression value");

        return typed_value.type() == value_type_t::FUNCREF ? typed_value.as<funcref_t>() : instance.function_space[typed_value.as<funcidx_t>()]->ref();
    }
};

constexpr byte_t TRAP             = 0xff;

constexpr byte_t TRAP_UNINIT_ELEM = 1;
constexpr byte_t TRAP_EXIT        = 2;
constexpr byte_t TRAP_CALL_HOST   = 3;
constexpr byte_t TRAP_GUARD       = 4;
constexpr byte_t TRAP_CALL_FAR    = 5;
constexpr byte_t TRAP_RETURN_FAR  = 6;


struct uleb128_t {
    uint64_t value; //rax
    uint64_t size;  //rdx
};


struct sleb128_t {
    int64_t value;  //rax
    uint64_t size;  //rdx
};


extern "C" uleb128_t udecode(const void*);
extern "C" sleb128_t sdecode(const void*);


#endif