template<typename T>
struct Arr {
    uint32_t size;
    const typename VarType<T>::type* elems;
};

template<typename T>
struct VarType<Arr<T>> {
    using type = Arr<T>;
};

template<>
struct VarType<byte_t> {
    using type = byte_t;
};

template<> byte_t read<byte_t>(Reader& rdr);

template<>
struct VarType<uint32_t> {
    using type = uint32_t;
};

template<> uint32_t read<uint32_t>(Reader& rdr);

struct varuint1_t;

template<>
struct VarType<varuint1_t> {
    using type = uint8_t;
};

template<> uint8_t read<varuint1_t>(Reader& rdr);

struct varuint7_t;

template<>
struct VarType<varuint7_t> {
    using type = uint8_t;
};

template<> uint8_t read<varuint7_t>(Reader& rdr);

struct varuint32_t;

template<>
struct VarType<varuint32_t> {
    using type = uint32_t;
};

template<> uint32_t read<varuint32_t>(Reader& rdr);

struct varuint64_t;

template<>
struct VarType<varuint64_t> {
    using type = uint64_t;
};

template<> uint64_t read<varuint64_t>(Reader& rdr);

struct varsint7_t;

template<>
struct VarType<varsint7_t> {
    using type = int8_t;
};

template<> int8_t read<varsint7_t>(Reader& rdr);

struct varsint32_t;

template<>
struct VarType<varsint32_t> {
    using type = int32_t;
};

template<> int32_t read<varsint32_t>(Reader& rdr);

struct varsint64_t;

template<>
struct VarType<varsint64_t> {
    using type = int64_t;
};

template<> int64_t read<varsint64_t>(Reader& rdr);

typedef float float32_t;

template<>
struct VarType<float32_t> {
    using type = float;
};

template<> float read<float32_t>(Reader& rdr);

typedef double float64_t;

template<>
struct VarType<float64_t> {
    using type = double;
};

template<> double read<float64_t>(Reader& rdr);

enum class type_encoding_t : int8_t {
    I32 = -0x01,
    I64 = -0x02,
    F32 = -0x03,
    F64 = -0x04,
    V128 = -0x05,
    FUNCREF = -0x10,
    EXTERNREF = -0x11,
    FUNC = -0x20,
    VOID = -0x40,
};

template<>
struct VarType<type_encoding_t> {
    using type = type_encoding_t;
};

template<> type_encoding_t read<type_encoding_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, type_encoding_t value);

typedef uint64_t varuptr_t;

template<>
struct VarType<varuptr_t> {
    using type = varuptr_t;
};

template<> varuptr_t read<varuptr_t>(Reader& rdr);

struct memflags_t {
    union {
        uint32_t raw;
        struct {
            uint32_t align : 32;

        };
    };
} __attribute__ ((__packed__));

template<>
struct VarType<memflags_t> {
    using type = memflags_t;
};

template<> memflags_t read<memflags_t>(Reader& rdr);

typedef Arr<byte_t> identifier_t;

enum class external_kind_t : uint8_t {
    FUNCTION = 0x00,
    TABLE = 0x01,
    MEMORY = 0x02,
    GLOBAL = 0x03,
};

template<>
struct VarType<external_kind_t> {
    using type = external_kind_t;
};

template<> external_kind_t read<external_kind_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, external_kind_t value);

enum class reference_type_t : int8_t {
    FUNCREF = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::FUNCREF),
    EXTERNREF = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::EXTERNREF),
};

template<>
struct VarType<reference_type_t> {
    using type = reference_type_t;
};

template<> reference_type_t read<reference_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, reference_type_t value);

enum class vector_value_type_t : int8_t {
    V128 = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::V128),
};

template<>
struct VarType<vector_value_type_t> {
    using type = vector_value_type_t;
};

template<> vector_value_type_t read<vector_value_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, vector_value_type_t value);

enum class integer_value_type_t : int8_t {
    I32 = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::I32),
    I64 = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::I64),
};

template<>
struct VarType<integer_value_type_t> {
    using type = integer_value_type_t;
};

template<> integer_value_type_t read<integer_value_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, integer_value_type_t value);

enum class floating_point_value_type_t : int8_t {
    F32 = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::F32),
    F64 = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::F64),
};

template<>
struct VarType<floating_point_value_type_t> {
    using type = floating_point_value_type_t;
};

template<> floating_point_value_type_t read<floating_point_value_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, floating_point_value_type_t value);

enum class signature_type_t : int8_t {
    FUNC = typename std::underlying_type<type_encoding_t>::type(type_encoding_t::FUNC),
};

template<>
struct VarType<signature_type_t> {
    using type = signature_type_t;
};

template<> signature_type_t read<signature_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, signature_type_t value);

enum class value_type_t : int8_t {
    I32 = typename std::underlying_type<integer_value_type_t>::type(integer_value_type_t::I32),
    I64 = typename std::underlying_type<integer_value_type_t>::type(integer_value_type_t::I64),
    F32 = typename std::underlying_type<floating_point_value_type_t>::type(floating_point_value_type_t::F32),
    F64 = typename std::underlying_type<floating_point_value_type_t>::type(floating_point_value_type_t::F64),
    V128 = typename std::underlying_type<vector_value_type_t>::type(vector_value_type_t::V128),
    FUNCREF = typename std::underlying_type<reference_type_t>::type(reference_type_t::FUNCREF),
    EXTERNREF = typename std::underlying_type<reference_type_t>::type(reference_type_t::EXTERNREF),
};

template<>
struct VarType<value_type_t> {
    using type = value_type_t;
};

template<> value_type_t read<value_type_t>(Reader& rdr);

std::ostream& operator<<(std::ostream& os, value_type_t value);

struct instruction_t {
    const byte_t prefix;
    const byte_t opcode;
    const int8_t depth;
    const int8_t nvals;

    static constexpr int8_t var_nvals = std::numeric_limits<int8_t>::min();

    bool operator!=(const instruction_t& i) const {
        return opcode != i.opcode;
    }
};

template<>
struct Many<instruction_t> {
    byte_t* begin;
    byte_t* end;
};

template<>
struct VarType<instruction_t> {
    using type = instruction_t;
};

template<> instruction_t read<instruction_t>(Reader& rdr);

struct block_signature_type_t {
    union {
        int32_t type_value;
        uint32_t type_index;
    };
};

template<> const block_signature_type_t* read<block_signature_type_t>(Reader& rdr);

struct resizable_limits_t {
    uint32_t flags;
    union {
        struct {
            uint32_t minimum;
        } _0x00;
        struct {
            uint32_t minimum;
            uint32_t maximum;
        } _0x01;
    };
};

template<> const resizable_limits_t* read<resizable_limits_t>(Reader& rdr);

struct instantiation_time_initializer_t {
    Many<instruction_t> instruction;
};

template<> const instantiation_time_initializer_t* read<instantiation_time_initializer_t>(Reader& rdr);

struct table_description_t {
    reference_type_t element_type;
    const resizable_limits_t* resizable;
};

template<> const table_description_t* read<table_description_t>(Reader& rdr);

struct linear_memory_description_t {
    const resizable_limits_t* limits;
};

template<> const linear_memory_description_t* read<linear_memory_description_t>(Reader& rdr);

struct global_description_t {
    value_type_t type;
    uint8_t mutability;
};

template<> const global_description_t* read<global_description_t>(Reader& rdr);

struct local_entry_t {
    uint32_t count;
    value_type_t type;
};

template<> const local_entry_t* read<local_entry_t>(Reader& rdr);

struct function_signature_t {
    signature_type_t form;
    union {
        struct {
            Arr<value_type_t> params;
            Arr<value_type_t> returns;
        } func;
    };
};

template<> const function_signature_t* read<function_signature_t>(Reader& rdr);

struct import_t {
    identifier_t module_name;
    identifier_t export_name;
    external_kind_t kind;
    union {
        struct {
            uint32_t sig_index;
        } function;
        struct {
            const table_description_t* desc;
        } table;
        struct {
            const linear_memory_description_t* desc;
        } memory;
        struct {
            const global_description_t* desc;
        } global;
    };
};

template<> const import_t* read<import_t>(Reader& rdr);

struct function_declaration_t {
    uint32_t sig_index;
};

template<> const function_declaration_t* read<function_declaration_t>(Reader& rdr);

struct global_declaration_t {
    const global_description_t* desc;
    const instantiation_time_initializer_t* init;
};

template<> const global_declaration_t* read<global_declaration_t>(Reader& rdr);

struct export_t {
    identifier_t name;
    external_kind_t kind;
    union {
        struct {
            uint32_t index;
        } function;
        struct {
            uint32_t index;
        } table;
        struct {
            uint32_t index;
        } memory;
        struct {
            uint32_t index;
        } global;
    };
};

template<> const export_t* read<export_t>(Reader& rdr);

struct table_initializer_t {
    uint32_t flags;
    union {
        struct {
            const instantiation_time_initializer_t* offset;
            Arr<varuint32_t> elems;
        } _0x00;
        struct {
            byte_t elemkind;
            Arr<varuint32_t> elems;
        } _0x01;
        struct {
            uint32_t tableidx;
            const instantiation_time_initializer_t* offset;
            byte_t elemkind;
            Arr<varuint32_t> elems;
        } _0x02;
        struct {
            byte_t elemkind;
            Arr<varuint32_t> elems;
        } _0x03;
        struct {
            const instantiation_time_initializer_t* offset;
            Arr<instantiation_time_initializer_t> elems;
        } _0x04;
        struct {
            reference_type_t elemtype;
            Arr<instantiation_time_initializer_t> elems;
        } _0x05;
        struct {
            uint32_t tableidx;
            const instantiation_time_initializer_t* offset;
            reference_type_t elemtype;
            Arr<instantiation_time_initializer_t> elems;
        } _0x06;
        struct {
            reference_type_t elemtype;
            Arr<instantiation_time_initializer_t> elems;
        } _0x07;
    };
};

template<> const table_initializer_t* read<table_initializer_t>(Reader& rdr);

struct function_body_t {
    uint32_t body_size;
    Arr<local_entry_t> locals;
    Many<instruction_t> instructions;
};

template<> const function_body_t* read<function_body_t>(Reader& rdr);

struct data_initializer_t {
    uint32_t flags;
    union {
        struct {
            const instantiation_time_initializer_t* offset;
            Arr<byte_t> data;
        } _0x00;
        struct {
            Arr<byte_t> data;
        } _0x01;
        struct {
            uint32_t memidx;
            const instantiation_time_initializer_t* offset;
            Arr<byte_t> data;
        } _0x02;
    };
};

template<> const data_initializer_t* read<data_initializer_t>(Reader& rdr);

struct DataCountSection {
    uint32_t size;
    uint32_t seccount;
};

template<> const DataCountSection* read<DataCountSection>(Reader& rdr);

template<> bool isa<DataCountSection>(Reader& rdr);

struct TypeSection {
    uint32_t size;
    Arr<function_signature_t> _;
};

template<> const TypeSection* read<TypeSection>(Reader& rdr);

template<> bool isa<TypeSection>(Reader& rdr);

struct ImportSection {
    uint32_t size;
    Arr<import_t> _;
};

template<> const ImportSection* read<ImportSection>(Reader& rdr);

template<> bool isa<ImportSection>(Reader& rdr);

struct FunctionSection {
    uint32_t size;
    Arr<function_declaration_t> _;
};

template<> const FunctionSection* read<FunctionSection>(Reader& rdr);

template<> bool isa<FunctionSection>(Reader& rdr);

struct TableSection {
    uint32_t size;
    Arr<table_description_t> _;
};

template<> const TableSection* read<TableSection>(Reader& rdr);

template<> bool isa<TableSection>(Reader& rdr);

struct LinearMemorySection {
    uint32_t size;
    Arr<linear_memory_description_t> _;
};

template<> const LinearMemorySection* read<LinearMemorySection>(Reader& rdr);

template<> bool isa<LinearMemorySection>(Reader& rdr);

struct GlobalSection {
    uint32_t size;
    Arr<global_declaration_t> _;
};

template<> const GlobalSection* read<GlobalSection>(Reader& rdr);

template<> bool isa<GlobalSection>(Reader& rdr);

struct ExportSection {
    uint32_t size;
    Arr<export_t> _;
};

template<> const ExportSection* read<ExportSection>(Reader& rdr);

template<> bool isa<ExportSection>(Reader& rdr);

struct StartSection {
    uint32_t size;
    uint32_t index;
};

template<> const StartSection* read<StartSection>(Reader& rdr);

template<> bool isa<StartSection>(Reader& rdr);

struct ElementSection {
    uint32_t size;
    Arr<table_initializer_t> _;
};

template<> const ElementSection* read<ElementSection>(Reader& rdr);

template<> bool isa<ElementSection>(Reader& rdr);

struct CodeSection {
    uint32_t size;
    Arr<function_body_t> _;
};

template<> const CodeSection* read<CodeSection>(Reader& rdr);

template<> bool isa<CodeSection>(Reader& rdr);

struct DataSection {
    uint32_t size;
    Arr<data_initializer_t> _;
};

template<> const DataSection* read<DataSection>(Reader& rdr);

template<> bool isa<DataSection>(Reader& rdr);

struct CustomSection {
    Arr<byte_t> bytes;
};

template<> const CustomSection* read<CustomSection>(Reader& rdr);

template<> bool isa<CustomSection>(Reader& rdr);

struct Module {
    uint32_t magic_cookie;
    uint32_t version;
    const TypeSection* type_section;
    const ImportSection* import_section;
    const FunctionSection* function_section;
    const TableSection* table_section;
    const LinearMemorySection* linear_memory_section;
    const GlobalSection* global_section;
    const ExportSection* export_section;
    const StartSection* start_section;
    const ElementSection* element_section;
    const DataCountSection* data_count_section;
    const CodeSection* code_section;
    const DataSection* data_section;
};

template<> const Module* read<Module>(Reader& rdr);
