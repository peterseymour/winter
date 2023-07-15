
template<> byte_t read<byte_t>(Reader& rdr) {
    return rdr.read<byte_t>();
}

template<> uint32_t read<uint32_t>(Reader& rdr) {
    return rdr.read<uint32_t>();
}

template<> uint8_t read<varuint1_t>(Reader& rdr) {
    auto value = rdr.read_leb128<uint64_t>();

    check(value <= 0x1L, "varuint1_t out of range @", rdr.relpos());

    return value;
}

template<> uint8_t read<varuint7_t>(Reader& rdr) {
    auto value = rdr.read_leb128<uint64_t>();

    check(value <= 0x7fL, "varuint7_t out of range @", rdr.relpos());

    return value;
}

template<> uint32_t read<varuint32_t>(Reader& rdr) {
    auto value = rdr.read_leb128<uint64_t>();

    check(value <= 0xffffffffL, "varuint32_t out of range @", rdr.relpos());

    return value;
}

template<> uint64_t read<varuint64_t>(Reader& rdr) {
    auto value = rdr.read_leb128<uint64_t>();

    //check(value <= 0xffffffffffffffffL, "varuint64_t out of range @", rdr.relpos());

    return value;
}

template<> int8_t read<varsint7_t>(Reader& rdr) {
    auto value = rdr.read_leb128<int64_t>();

    check(-0x40L <= value and value <= 0x3fL, "varsint7_t out of range @", rdr.relpos());

    return value;
}

template<> int32_t read<varsint32_t>(Reader& rdr) {
    auto value = rdr.read_leb128<int64_t>();

    check(-0x80000000L <= value and value <= 0x7fffffffL, "varsint32_t out of range @", rdr.relpos());

    return value;
}

template<> int64_t read<varsint64_t>(Reader& rdr) {
    auto value = rdr.read_leb128<int64_t>();

    //check(-0x8000000000000000L <= value and value <= 0x7fffffffffffffffL, "varsint64_t out of range @", rdr.relpos());

    return value;
}

template<> float read<float32_t>(Reader& rdr) {
    return rdr.read<float>();
}

template<> double read<float64_t>(Reader& rdr) {
    return rdr.read<double>();
}

template<> type_encoding_t read<type_encoding_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    type_encoding_t e;

    switch(value) {
        case -0x01: e = type_encoding_t::I32; break;
        case -0x02: e = type_encoding_t::I64; break;
        case -0x03: e = type_encoding_t::F32; break;
        case -0x04: e = type_encoding_t::F64; break;
        case -0x05: e = type_encoding_t::V128; break;
        case -0x10: e = type_encoding_t::FUNCREF; break;
        case -0x11: e = type_encoding_t::EXTERNREF; break;
        case -0x20: e = type_encoding_t::FUNC; break;
        case -0x40: e = type_encoding_t::VOID; break;

        default:
            error("Unhandled enum value for type_encoding_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, type_encoding_t value) {
    switch(value) {
        case type_encoding_t::I32: return os << "i32";
        case type_encoding_t::I64: return os << "i64";
        case type_encoding_t::F32: return os << "f32";
        case type_encoding_t::F64: return os << "f64";
        case type_encoding_t::V128: return os << "v128";
        case type_encoding_t::FUNCREF: return os << "funcref";
        case type_encoding_t::EXTERNREF: return os << "externref";
        case type_encoding_t::FUNC: return os << "func";
        case type_encoding_t::VOID: return os << "void";

        default:
            error("Unhandled enum value for type_encoding_t", (int) value);
    };

    return os;
}

template<> varuptr_t read<varuptr_t>(Reader& rdr) {
    auto value = rdr.read_leb128<uint64_t>();

    //check(value <= 0xffffffffffffffffL, "varuptr_t out of range @", rdr.relpos());

    return value;
}

template<> memflags_t read<memflags_t>(Reader& rdr) {
    return {.raw = read<varuint32_t>(rdr)};
}

template<> external_kind_t read<external_kind_t>(Reader& rdr) {
    auto value = read<varuint7_t>(rdr);
    external_kind_t e;

    switch(value) {
        case 0x00: e = external_kind_t::FUNCTION; break;
        case 0x01: e = external_kind_t::TABLE; break;
        case 0x02: e = external_kind_t::MEMORY; break;
        case 0x03: e = external_kind_t::GLOBAL; break;

        default:
            error("Unhandled enum value for external_kind_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, external_kind_t value) {
    switch(value) {
        case external_kind_t::FUNCTION: return os << "Function";
        case external_kind_t::TABLE: return os << "Table";
        case external_kind_t::MEMORY: return os << "Memory";
        case external_kind_t::GLOBAL: return os << "Global";

        default:
            error("Unhandled enum value for external_kind_t", (int) value);
    };

    return os;
}

template<> reference_type_t read<reference_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    reference_type_t e;

    switch(value) {
        case -0x10: e = reference_type_t::FUNCREF; break;
        case -0x11: e = reference_type_t::EXTERNREF; break;

        default:
            error("Unhandled enum value for reference_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, reference_type_t value) {
    switch(value) {
        case reference_type_t::FUNCREF: return os << "funcref";
        case reference_type_t::EXTERNREF: return os << "externref";

        default:
            error("Unhandled enum value for reference_type_t", (int) value);
    };

    return os;
}

template<> vector_value_type_t read<vector_value_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    vector_value_type_t e;

    switch(value) {
        case -0x05: e = vector_value_type_t::V128; break;

        default:
            error("Unhandled enum value for vector_value_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, vector_value_type_t value) {
    switch(value) {
        case vector_value_type_t::V128: return os << "v128";

        default:
            error("Unhandled enum value for vector_value_type_t", (int) value);
    };

    return os;
}

template<> integer_value_type_t read<integer_value_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    integer_value_type_t e;

    switch(value) {
        case -0x01: e = integer_value_type_t::I32; break;
        case -0x02: e = integer_value_type_t::I64; break;

        default:
            error("Unhandled enum value for integer_value_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, integer_value_type_t value) {
    switch(value) {
        case integer_value_type_t::I32: return os << "i32";
        case integer_value_type_t::I64: return os << "i64";

        default:
            error("Unhandled enum value for integer_value_type_t", (int) value);
    };

    return os;
}

template<> floating_point_value_type_t read<floating_point_value_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    floating_point_value_type_t e;

    switch(value) {
        case -0x03: e = floating_point_value_type_t::F32; break;
        case -0x04: e = floating_point_value_type_t::F64; break;

        default:
            error("Unhandled enum value for floating_point_value_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, floating_point_value_type_t value) {
    switch(value) {
        case floating_point_value_type_t::F32: return os << "f32";
        case floating_point_value_type_t::F64: return os << "f64";

        default:
            error("Unhandled enum value for floating_point_value_type_t", (int) value);
    };

    return os;
}

template<> signature_type_t read<signature_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    signature_type_t e;

    switch(value) {
        case -0x20: e = signature_type_t::FUNC; break;

        default:
            error("Unhandled enum value for signature_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, signature_type_t value) {
    switch(value) {
        case signature_type_t::FUNC: return os << "func";

        default:
            error("Unhandled enum value for signature_type_t", (int) value);
    };

    return os;
}

template<> value_type_t read<value_type_t>(Reader& rdr) {
    auto value = read<varsint7_t>(rdr);
    value_type_t e;

    switch(value) {
        case -0x01: e = value_type_t::I32; break;
        case -0x02: e = value_type_t::I64; break;
        case -0x03: e = value_type_t::F32; break;
        case -0x04: e = value_type_t::F64; break;
        case -0x05: e = value_type_t::V128; break;
        case -0x10: e = value_type_t::FUNCREF; break;
        case -0x11: e = value_type_t::EXTERNREF; break;

        default:
            error("Unhandled enum value for value_type_t", (int) value);
    };

    return e;
}

std::ostream& operator<<(std::ostream& os, value_type_t value) {
    switch(value) {
        case value_type_t::I32: return os << "i32";
        case value_type_t::I64: return os << "i64";
        case value_type_t::F32: return os << "f32";
        case value_type_t::F64: return os << "f64";
        case value_type_t::V128: return os << "v128";
        case value_type_t::FUNCREF: return os << "funcref";
        case value_type_t::EXTERNREF: return os << "externref";

        default:
            error("Unhandled enum value for value_type_t", (int) value);
    };

    return os;
}

template<> instruction_t read<instruction_t>(Reader& rdr) {
    const auto opcode = rdr.read<byte_t>();
    int8_t depth = 0;
    int8_t nvals = 0;
    if ((0x00 <= opcode and opcode <= 0x01) or opcode == 0x45 or opcode == 0x50 or (0x67 <= opcode and opcode <= 0x69) or (0x79 <= opcode and opcode <= 0x7B) or (0x8B <= opcode and opcode <= 0x91) or (0x99 <= opcode and opcode <= 0x9F) or opcode == 0xD1 or (0xA7 <= opcode and opcode <= 0xC4)) {
    } else if ((0x02 <= opcode and opcode <= 0x03)) {
        ::read<block_signature_type_t>(rdr);
        ++depth;
    } else if (opcode == 0x04) {
        ::read<block_signature_type_t>(rdr);
        ++depth;
        nvals = -1;
    } else if (opcode == 0x05) {
        ;
        nvals = instruction_t::var_nvals;
    } else if (opcode == 0x0B) {
        --depth;
        nvals = instruction_t::var_nvals;
    } else if ((0x0C <= opcode and opcode <= 0x0D) or opcode == 0x10 or opcode == 0x12) {
        ::read<varuint32_t>(rdr);
        nvals = instruction_t::var_nvals;
    } else if (opcode == 0x0E) {
        ::read<Arr<varuint32_t>>(rdr);
        ::read<varuint32_t>(rdr);
        nvals = instruction_t::var_nvals;
    } else if (opcode == 0x0F) {
        nvals = instruction_t::var_nvals;
    } else if (opcode == 0x11) {
        ::read<varuint32_t>(rdr);
        ::read<varuint32_t>(rdr);
        nvals = instruction_t::var_nvals;
    } else if (opcode == 0x1C) {
        ::read<Arr<value_type_t>>(rdr);
        nvals = -2;
    } else if (opcode == 0x1A or (0x46 <= opcode and opcode <= 0x4F) or (0x51 <= opcode and opcode <= 0x66) or (0x6A <= opcode and opcode <= 0x78) or (0x7C <= opcode and opcode <= 0x8A) or (0x92 <= opcode and opcode <= 0x98) or (0xA0 <= opcode and opcode <= 0xA6)) {
        nvals = -1;
    } else if (opcode == 0x1B) {
        nvals = -2;
    } else if (opcode == 0x20 or opcode == 0x23 or opcode == 0xD2) {
        ::read<varuint32_t>(rdr);
        nvals = 1;
    } else if (opcode == 0x21 or opcode == 0x24) {
        ::read<varuint32_t>(rdr);
        nvals = -1;
    } else if (opcode == 0x22 or opcode == 0x25) {
        ::read<varuint32_t>(rdr);
    } else if (opcode == 0x26) {
        ::read<varuint32_t>(rdr);
        nvals = -2;
    } else if ((0x28 <= opcode and opcode <= 0x35)) {
        ::read<memflags_t>(rdr);
        ::read<varuptr_t>(rdr);
    } else if ((0x36 <= opcode and opcode <= 0x3E)) {
        ::read<memflags_t>(rdr);
        ::read<varuptr_t>(rdr);
        nvals = -2;
    } else if (opcode == 0x3F) {
        ::read<varuint1_t>(rdr);
        nvals = 1;
    } else if (opcode == 0x40) {
        ::read<varuint1_t>(rdr);
    } else if (opcode == 0x41) {
        ::read<varsint32_t>(rdr);
        nvals = 1;
    } else if (opcode == 0x42) {
        ::read<varsint64_t>(rdr);
        nvals = 1;
    } else if (opcode == 0x43) {
        ::read<float32_t>(rdr);
        nvals = 1;
    } else if (opcode == 0x44) {
        ::read<float64_t>(rdr);
        nvals = 1;
    } else if (opcode == 0xD0) {
        ::read<reference_type_t>(rdr);
        nvals = 1;
    } else if (opcode == 0xFC) {
        const auto opcode = rdr.read<byte_t>();
        if ((0x00 <= opcode and opcode <= 0x07)) {
        } else if (opcode == 0x08) {
            ::read<varuint32_t>(rdr);
            ::read<byte_t>(rdr);
            nvals = -3;
        } else if (opcode == 0x09 or opcode == 0x0D) {
            ::read<varuint32_t>(rdr);
        } else if (opcode == 0x0A) {
            ::read<byte_t>(rdr);
            ::read<byte_t>(rdr);
            nvals = -3;
        } else if (opcode == 0x0B) {
            ::read<byte_t>(rdr);
            nvals = -3;
        } else if (opcode == 0x0C or opcode == 0x0E) {
            ::read<varuint32_t>(rdr);
            ::read<varuint32_t>(rdr);
            nvals = -3;
        } else if (opcode == 0x0F) {
            ::read<varuint32_t>(rdr);
            nvals = -1;
        } else if (opcode == 0x10) {
            ::read<varuint32_t>(rdr);
            nvals = 1;
        } else if (opcode == 0x11) {
            ::read<varuint32_t>(rdr);
            nvals = -3;
        } else 
            error("Bad opcode", std::hex, std::setw(2), std::setfill('0'), 0xFC, (int) opcode, "while reading instruction");

        return {.prefix=0xFC, .opcode=opcode, .depth=depth, .nvals=nvals};
    }
    else 
        error("Bad opcode", std::hex, std::setw(2), std::setfill('0'), (int) opcode, "while reading instruction");

    return {.prefix=0, .opcode=opcode, .depth=depth, .nvals=nvals};
}

template<>
struct ReaderFor<Many<instruction_t>> {
    static typename VarType<Many<instruction_t>>::type read(Reader& rdr) {
        const auto begin = rdr.imgpos();

        for(int32_t depth = 1; depth > 0;) {
            const auto instr = ::read<instruction_t>(rdr);
            depth += instr.depth;

            check(depth >= 0 or instr.opcode == 11, "Invalid final instruction");
        };

        const auto end = rdr.imgpos();

        return {.begin=begin, .end=end};
    }
};

template<> const block_signature_type_t* read<block_signature_type_t>(Reader& rdr) {
    block_signature_type_t* r = new block_signature_type_t;
    if (true) {
        r->type_value = ::read<varsint32_t>(rdr);
    } else 
        error("Unhandled condition for block_signature_type_t");

    return r;
}

template<> const resizable_limits_t* read<resizable_limits_t>(Reader& rdr) {
    resizable_limits_t* r = new resizable_limits_t;
    r->flags = ::read<varuint32_t>(rdr);
    if (r->flags == 0x00) {
        r->_0x00.minimum = ::read<varuint32_t>(rdr);
    } else if (r->flags == 0x01) {
        r->_0x01.minimum = ::read<varuint32_t>(rdr);
        r->_0x01.maximum = ::read<varuint32_t>(rdr);
    } else 
        error("Unhandled condition for resizable_limits_t", r->flags);

    return r;
}

template<> const instantiation_time_initializer_t* read<instantiation_time_initializer_t>(Reader& rdr) {
    instantiation_time_initializer_t* r = new instantiation_time_initializer_t;
    r->instruction = ::read<Many<instruction_t>>(rdr);

    return r;
}

template<> const table_description_t* read<table_description_t>(Reader& rdr) {
    table_description_t* r = new table_description_t;
    r->element_type = ::read<reference_type_t>(rdr);
    r->resizable = ::read<resizable_limits_t>(rdr);

    return r;
}

template<> const linear_memory_description_t* read<linear_memory_description_t>(Reader& rdr) {
    linear_memory_description_t* r = new linear_memory_description_t;
    r->limits = ::read<resizable_limits_t>(rdr);

    return r;
}

template<> const global_description_t* read<global_description_t>(Reader& rdr) {
    global_description_t* r = new global_description_t;
    r->type = ::read<value_type_t>(rdr);
    r->mutability = ::read<varuint1_t>(rdr);

    return r;
}

template<> const local_entry_t* read<local_entry_t>(Reader& rdr) {
    local_entry_t* r = new local_entry_t;
    r->count = ::read<varuint32_t>(rdr);
    r->type = ::read<value_type_t>(rdr);

    return r;
}

template<> const function_signature_t* read<function_signature_t>(Reader& rdr) {
    function_signature_t* r = new function_signature_t;
    r->form = ::read<signature_type_t>(rdr);
    if (r->form == signature_type_t::FUNC) {
        r->func.params = ::read<Arr<value_type_t>>(rdr);
        r->func.returns = ::read<Arr<value_type_t>>(rdr);
    } else 
        error("Unhandled condition for function_signature_t", r->form);

    return r;
}

template<> const import_t* read<import_t>(Reader& rdr) {
    import_t* r = new import_t;
    r->module_name = ::read<identifier_t>(rdr);
    r->export_name = ::read<identifier_t>(rdr);
    r->kind = ::read<external_kind_t>(rdr);
    if (r->kind == external_kind_t::FUNCTION) {
        r->function.sig_index = ::read<varuint32_t>(rdr);
    } else if (r->kind == external_kind_t::TABLE) {
        r->table.desc = ::read<table_description_t>(rdr);
    } else if (r->kind == external_kind_t::MEMORY) {
        r->memory.desc = ::read<linear_memory_description_t>(rdr);
    } else if (r->kind == external_kind_t::GLOBAL) {
        r->global.desc = ::read<global_description_t>(rdr);
    } else 
        error("Unhandled condition for import_t", r->kind);

    return r;
}

template<> const function_declaration_t* read<function_declaration_t>(Reader& rdr) {
    function_declaration_t* r = new function_declaration_t;
    r->sig_index = ::read<varuint32_t>(rdr);

    return r;
}

template<> const global_declaration_t* read<global_declaration_t>(Reader& rdr) {
    global_declaration_t* r = new global_declaration_t;
    r->desc = ::read<global_description_t>(rdr);
    r->init = ::read<instantiation_time_initializer_t>(rdr);

    return r;
}

template<> const export_t* read<export_t>(Reader& rdr) {
    export_t* r = new export_t;
    r->name = ::read<identifier_t>(rdr);
    r->kind = ::read<external_kind_t>(rdr);
    if (r->kind == external_kind_t::FUNCTION) {
        r->function.index = ::read<varuint32_t>(rdr);
    } else if (r->kind == external_kind_t::TABLE) {
        r->table.index = ::read<varuint32_t>(rdr);
    } else if (r->kind == external_kind_t::MEMORY) {
        r->memory.index = ::read<varuint32_t>(rdr);
    } else if (r->kind == external_kind_t::GLOBAL) {
        r->global.index = ::read<varuint32_t>(rdr);
    } else 
        error("Unhandled condition for export_t", r->kind);

    return r;
}

template<> const table_initializer_t* read<table_initializer_t>(Reader& rdr) {
    table_initializer_t* r = new table_initializer_t;
    r->flags = ::read<varuint32_t>(rdr);
    if (r->flags == 0x00) {
        r->_0x00.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x00.elems = ::read<Arr<varuint32_t>>(rdr);
    } else if (r->flags == 0x01) {
        r->_0x01.elemkind = ::read<byte_t>(rdr);
        r->_0x01.elems = ::read<Arr<varuint32_t>>(rdr);
    } else if (r->flags == 0x02) {
        r->_0x02.tableidx = ::read<varuint32_t>(rdr);
        r->_0x02.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x02.elemkind = ::read<byte_t>(rdr);
        r->_0x02.elems = ::read<Arr<varuint32_t>>(rdr);
    } else if (r->flags == 0x03) {
        r->_0x03.elemkind = ::read<byte_t>(rdr);
        r->_0x03.elems = ::read<Arr<varuint32_t>>(rdr);
    } else if (r->flags == 0x04) {
        r->_0x04.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x04.elems = ::read<Arr<instantiation_time_initializer_t>>(rdr);
    } else if (r->flags == 0x05) {
        r->_0x05.elemtype = ::read<reference_type_t>(rdr);
        r->_0x05.elems = ::read<Arr<instantiation_time_initializer_t>>(rdr);
    } else if (r->flags == 0x06) {
        r->_0x06.tableidx = ::read<varuint32_t>(rdr);
        r->_0x06.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x06.elemtype = ::read<reference_type_t>(rdr);
        r->_0x06.elems = ::read<Arr<instantiation_time_initializer_t>>(rdr);
    } else if (r->flags == 0x07) {
        r->_0x07.elemtype = ::read<reference_type_t>(rdr);
        r->_0x07.elems = ::read<Arr<instantiation_time_initializer_t>>(rdr);
    } else 
        error("Unhandled condition for table_initializer_t", r->flags);

    return r;
}

template<> const function_body_t* read<function_body_t>(Reader& rdr) {
    function_body_t* r = new function_body_t;
    r->body_size = ::read<varuint32_t>(rdr);
    r->locals = ::read<Arr<local_entry_t>>(rdr);
    r->instructions = ::read<Many<instruction_t>>(rdr);

    return r;
}

template<> const data_initializer_t* read<data_initializer_t>(Reader& rdr) {
    data_initializer_t* r = new data_initializer_t;
    r->flags = ::read<varuint32_t>(rdr);
    if (r->flags == 0x00) {
        r->_0x00.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x00.data = ::read<Arr<byte_t>>(rdr);
    } else if (r->flags == 0x01) {
        r->_0x01.data = ::read<Arr<byte_t>>(rdr);
    } else if (r->flags == 0x02) {
        r->_0x02.memidx = ::read<varuint32_t>(rdr);
        r->_0x02.offset = ::read<instantiation_time_initializer_t>(rdr);
        r->_0x02.data = ::read<Arr<byte_t>>(rdr);
    } else 
        error("Unhandled condition for data_initializer_t", r->flags);

    return r;
}

template<> const DataCountSection* read<DataCountSection>(Reader& rdr) {
    DataCountSection* r = new DataCountSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->seccount = ::read<varuint32_t>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid DataCountSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<DataCountSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x0c)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const TypeSection* read<TypeSection>(Reader& rdr) {
    TypeSection* r = new TypeSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<function_signature_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid TypeSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<TypeSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x01)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const ImportSection* read<ImportSection>(Reader& rdr) {
    ImportSection* r = new ImportSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<import_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid ImportSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<ImportSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x02)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const FunctionSection* read<FunctionSection>(Reader& rdr) {
    FunctionSection* r = new FunctionSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<function_declaration_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid FunctionSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<FunctionSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x03)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const TableSection* read<TableSection>(Reader& rdr) {
    TableSection* r = new TableSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<table_description_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid TableSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<TableSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x04)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const LinearMemorySection* read<LinearMemorySection>(Reader& rdr) {
    LinearMemorySection* r = new LinearMemorySection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<linear_memory_description_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid LinearMemorySection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<LinearMemorySection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x05)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const GlobalSection* read<GlobalSection>(Reader& rdr) {
    GlobalSection* r = new GlobalSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<global_declaration_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid GlobalSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<GlobalSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x06)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const ExportSection* read<ExportSection>(Reader& rdr) {
    ExportSection* r = new ExportSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<export_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid ExportSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<ExportSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x07)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const StartSection* read<StartSection>(Reader& rdr) {
    StartSection* r = new StartSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->index = ::read<varuint32_t>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid StartSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<StartSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x08)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const ElementSection* read<ElementSection>(Reader& rdr) {
    ElementSection* r = new ElementSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<table_initializer_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid ElementSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<ElementSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x09)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const CodeSection* read<CodeSection>(Reader& rdr) {
    CodeSection* r = new CodeSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<function_body_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid CodeSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<CodeSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x0a)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const DataSection* read<DataSection>(Reader& rdr) {
    DataSection* r = new DataSection;
    auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);
    r->size = ::read<varuint32_t>(rdr);
    r->_ = ::read<Arr<data_initializer_t>>(rdr);
    check(r->size == rdr.pos() - begin, "Invalid DataSection size", rdr.pos() - begin);

    return r;
}

template<> bool isa<DataSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x0b)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const CustomSection* read<CustomSection>(Reader& rdr) {
    CustomSection* r = new CustomSection;
    r->bytes = ::read<Arr<byte_t>>(rdr);

    return r;
}

template<> bool isa<CustomSection>(Reader& rdr) {
    if (rdr.atend() or rdr.peek<byte_t>() != 0x00)
        return false;

    rdr.read<byte_t>();
    return true;
}

template<> const Module* read<Module>(Reader& rdr) {
    Module* r = new Module;
    r->magic_cookie = ::read<uint32_t>(rdr);
    r->version = ::read<uint32_t>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->type_section = ::read<Maybe<TypeSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->import_section = ::read<Maybe<ImportSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->function_section = ::read<Maybe<FunctionSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->table_section = ::read<Maybe<TableSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->linear_memory_section = ::read<Maybe<LinearMemorySection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->global_section = ::read<Maybe<GlobalSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->export_section = ::read<Maybe<ExportSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->start_section = ::read<Maybe<StartSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->element_section = ::read<Maybe<ElementSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->data_count_section = ::read<Maybe<DataCountSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->code_section = ::read<Maybe<CodeSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);
    r->data_section = ::read<Maybe<DataSection>>(rdr);
    ::read<Many<const CustomSection>>(rdr);

    return r;
}
