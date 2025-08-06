#python3 gen.py ../src/cpp/wasm/gen/

from ast import *
import re
from itertools import chain


vartypes = {
    'byte_t': 'byte_t',
    'uint32_t': 'uint32_t',
    'float32_t': 'float',
    'float64_t': 'double',
    'varuint1_t': 'uint8_t',
    'varuint7_t': 'uint8_t',
    'varuint32_t': 'uint32_t',
    'varuint64_t': 'uint64_t',
    'varsint7_t': 'int8_t',
    'varsint32_t': 'int32_t',
    'varsint64_t': 'int64_t',
    'v128_t': 'uint128_t',
}


def templtype(typ, typevar=None):
    if typevar:
        return f"{templtype(typ)}<{typevar}>";

    typ = Type.coerce(typ)

    if isinstance(typ, TypeRef):
        return typ.name
    elif isinstance(typ, ArrayOf):
        return f"Arr<{templtype(typ.element_type)}>"
    else:
        raise NotImplementedError


def vartype(typ, typevar=None):
    if isinstance(typ, list):
        typ = Type.coerce(typ)

    if typ in vartypes:
        return vartypes[typ]
    elif isinstance(typ, str):
        return f"const {typ}*" if named_types[typ].is_ref_type() else f"{typ}";
    elif isinstance(typ, TypeRef):
        return vartype(typ.name)
    else:
        return templtype(typ, typevar=typevar)

def varname(name):
    return re.sub( '(?<!^)(?=[A-Z])', '_', name).lower()


def generate_read_fields(typename, accessor, struct, tgt, indent=4, post_field=None):
    for field in struct.fields:
        lhs = f"{accessor}{varname(field.name)}"

        if field.count == 1:
            rhs = f"::read<{templtype(field.type)}>(rdr)"
        elif field.count == '?':
            rhs = f"::read<Maybe<{templtype(field.type)}>>(rdr)"
        elif field.count == '*':
            rhs = f"::read<Many<{'const ' if templtype(field.type) != 'instruction_t' else ''}{templtype(field.type)}>>(rdr)"
        elif isinstance(field.count, str):
            tgt.write(f"""
                auto {field.name} = new typename VarType<{templtype(field.type)}>::type[v.{field.count}];
                for (auto i = 0; i < v.{field.count}; ++i)
                    {field.name}[i] = ::read<{templtype(field.type)}>(rdr);
            """, indent=indent)

            rhs = f"{field.name}"
        else:
            raise NotImplementedError(f"field.count = {field.count!r}")

        tgt.writeln(f"{lhs} = {rhs};", indent=indent)

        if post_field:
            post_field(typename, accessor, field, tgt, indent=indent)

    if struct.cond_fields:
        OPS = {'=': '=='}

        tgt.f.write(' ' * indent)

        attrnames = set()
        for cond, substruct in struct.cond_fields:
            if isinstance(cond, Boolean):
                if cond.istrue:
                    tgt.writeln(f"if (true) {{")

                    for field in substruct.fields:
                        assert field.count == 1
                        tgt.writeln(f"{accessor}{varname(field.name)} = ::read<{templtype(field.type)}>(rdr);", indent=indent + 4)
                else:
                    continue

            elif isinstance(cond.getter, Attribute):
                attrnames.add(cond.getter.name)

                for cond_field in struct.fields:
                    if cond_field.name == cond.getter.name:
                        break
                else:
                    raise NameError(f"Invalid condition field '{cond.getter.name}'")

                if isinstance(cond_field.type.resolve(), EnumType):
                    value = f"{templtype(cond_field.type)}::{cond.value.upper()}"
                elif isinstance(cond_field.type.resolve(), PrimitiveType):
                    value = f"{cond.value}"
                else:
                    raise NotImplementedError

                tgt.writeln(f"if ({accessor}{cond.getter.name} {OPS[cond.op]} {value}) {{")

                for field in substruct.fields:
                    assert field.count == 1
                    tgt.writeln(f"{accessor}{'' if str.isalpha(cond.value[0]) else '_'}{cond.value.lower()}.{varname(field.name)} = ::read<{templtype(field.type)}>(rdr);", indent=indent + 4)
            else:
                raise NotImplementedError
            tgt.write(f"}} else", indent=indent); tgt.f.write(' ')

        tgt.write(f"""
            error("Unhandled condition for {typename}"{', ' + ', '.join(accessor + attrname for attrname in attrnames) if attrnames else ''});
        """, indent=indent + 4)

    if struct.tail_type:
        tgt.writeln(f"{accessor}_ = ::read<{templtype(struct.tail_type)}>(rdr);", indent=indent)


def generate_struct(typename, struct, hpp, tpp, cpp, typevar=None, pre=None, post=None, post_field=None):
    if typevar:
        hpp.write(f"template<typename {typevar}>")

    hpp.write(f"""
        struct {typename} {{
    """)

    for field in struct.fields:
        if field.count in ('?', 1):
            hpp.writeln(f"{vartype(field.type)} {varname(field.name)};", indent=4)
        elif field.count == '*':
            hpp.writeln(f"Many<{'const ' if templtype(field.type) != 'instruction_t' else ''}{templtype(field.type)}> {varname(field.name)};", indent=4)
        elif isinstance(field.count, str):
            hpp.writeln(f"const typename VarType<{templtype(field.type)}>::type* {varname(field.name)};", indent=4)
        else:
            raise NotImplementedError

    if struct.cond_fields:
        hpp.writeln(f"union {{", indent=4)
        for cond, substruct in struct.cond_fields:
            if isinstance(cond, Boolean):
                for field in substruct.fields:
                    assert field.count == 1
                    hpp.writeln(f"{vartype(field.type)} {varname(field.name)};", indent=8)
            elif isinstance(cond.getter, Attribute):
                hpp.writeln(f"struct {{", indent=8)
                for field in substruct.fields:
                    assert field.count == 1
                    hpp.writeln(f"{vartype(field.type)} {varname(field.name)};", indent=12)
                hpp.writeln(f"}} {'' if str.isalpha(cond.value[0]) else '_'}{cond.value.lower()};", indent=8)
            else:
                raise NotImplementedError
        hpp.writeln(f"}};", indent=4)

    if struct.tail_type:
        hpp.writeln(f"{vartype(struct.tail_type)} _;", indent=4)

    hpp.writeln(f"}};")

    if not typevar:
        hpp.write(f"""
            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);
        """)

        cpp.write(f"""
            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                {typename}* r = new {typename};
        """)

        if (pre): cpp.writeln(pre, indent=4)

        generate_read_fields(typename, "r->", struct, cpp, indent=4, post_field=post_field)

        if (post): cpp.writeln(post, indent=4)

        cpp.write(f"""
                return r;
            }}
        """)
    else:
        T = f"{templtype(typename, typevar=typevar)}"

        hpp.write(f"""
            template<typename T>
            struct VarType<Arr<T>> {{
                using type = Arr<T>;
            }};
        """)

        tpp.write(f"""
            template<typename {typevar}>
            struct ReaderFor<{T}> {{
                static typename VarType<{T}>::type read(Reader& rdr) {{
                    {T} v;
        """)

        if (pre): tpp.writeln(pre, indent=4)

        generate_read_fields(typename, "v.", struct, tpp, indent=8, post_field=post_field)

        if (post): tpp.writeln(post, indent=4)

        tpp.write(f"""
                    return v;
                }}
            }};
        """)


def generate(typename, typ, hpp, tpp, cpp):
    m = re.match(r"^(\w+)\((\w+)\)$", typename)

    if m:
        typename, typevar = m.groups()
    else:
        typevar = None

    if isinstance(typ, PrimitiveType):
        if vartype(typename) != typename:
            if vartype(typename).endswith('_t') and typename != "v128_t":
                hpp.write(f"""
                    struct {typename};
                """)
            else:
                hpp.write(f"""
                    typedef {vartype(typename)} {typename};
                """)

        assert not typ.is_ref_type()

        hpp.write(f"""
            template<>
            struct VarType<{typename}> {{
                using type = {vartype(typename)};
            }};
        """)

        if isinstance(typ, (IntegerType, FloatType, VectorType)):
            hpp.write(f"""
                template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);
            """)

            cpp.write(f"""
                template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                    return rdr.read<{vartype(typename)}>();
                }}
            """)

        elif isinstance(typ, LEB128Type):
            typename = typename

            hpp.write(f"""
                template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);
            """)

            check = typ.nbits < 64

            if typ.signed:
                condition = f"{typ.minval:#x}L <= value and value <= {typ.maxval:#x}L"
            else:
                condition = f"value <= {typ.maxval:#x}L"

            cpp.write(f"""
                template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                    auto value = rdr.read_leb128<{('' if typ.signed else 'u') + 'int64_t'}>();

                    {'' if check else '//'}check({condition}, "{typename} out of range @", rdr.relpos());

                    return value;
                }}
            """)
        else:
            raise NotImplementedError

    elif isinstance(typ, EnumType):
        hpp.write(f"""
            enum class {typename} : {vartype(typ.alias)} {{
        """)

        options = []
        def inherit(base_type_alias, name):
            nfound = 0
            for subopt in base_type_alias.resolve().options:
                if subopt.name == '*':
                    nfound += inherit(subopt.base_type, name)
                elif name in ('*', subopt.name):
                    options.append(subopt)
                    hpp.writeln(f"{subopt.name.upper()} = typename std::underlying_type<{base_type_alias.name}>::type({base_type_alias.name}::{subopt.name.upper()}),", indent=4)
                    nfound += 1

            return nfound

        for opt in typ.options:
            if isinstance(opt, EnumType.InheritedOption):
                if inherit(opt.base_type, opt.name) == 0:
                    raise ValueError(f"Failed to find '{opt.name}' in '{opt.base_type.name}'")
            else:
                options.append(opt)
                hpp.writeln(f"{opt.name.upper()} = {opt.value},", indent=4)

        hpp.writeln(f"}};")

        hpp.write(f"""
            template<>
            struct VarType<{typename}> {{
                using type = {typename};
            }};

            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);

            std::ostream& operator<<(std::ostream& os, {typename} value);
        """)

        cpp.write(f"""
            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                auto value = read<{templtype(typ.alias)}>(rdr);
                {vartype(typename)} e;

                switch(value) {{
        """)

        for option in options:
            while isinstance(option, EnumType.InheritedOption):
                for opt in option.base_type.resolve().options:
                    if opt.name == option.name:
                        option = opt

            cpp.writeln(f"case {option.value}: e = {typename}::{option.name.upper()}; break;", indent=8)


        cpp.write(f"""
                    default:
                        error("Unhandled enum value for {typename}", (int) value);
                }};

                return e;
            }}

            std::ostream& operator<<(std::ostream& os, {typename} value) {{
                switch(value) {{
        """)

        for option in options:
            while isinstance(option, EnumType.InheritedOption):
                for opt in option.base_type.resolve().options:
                    if opt.name == option.name:
                        option = opt

            cpp.writeln(f"case {typename}::{option.name.upper()}: return os << \"{option.name}\";", indent=8)


        cpp.write(f"""
                    default:
                        error("Unhandled enum value for {typename}", (int) value);
                }};

                return os;
            }}
        """)

    elif isinstance(typ, BitFieldType):
        hpp.write(f"""
            struct {typename} {{
                union {{
                    {vartype(typ.alias)} raw;
                    struct {{
        """)

        p = 0
        for field in typ.bit_fields:
            if field.begin != p:
                hpp.writeln(f"{vartype(typ.alias)} : {field.begin - field.p};", indent=12)
                p = field.begin

            hpp.writeln(f"{vartype(typ.alias)} {field.name} : {field.end - field.begin};", indent=12)
            p = field.end

        hpp.write(f"""
                    }};
                }};
            }} __attribute__ ((__packed__));

            template<>
            struct VarType<{typename}> {{
                using type = {typename};
            }};

            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);
        """)

        cpp.write(f"""
            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                return {{.raw = read<{templtype(typ.alias)}>(rdr)}};
            }}
        """)

    elif isinstance(typ, ArrayOf):
        hpp.write(f"""
            typedef {vartype(typ)} {typename};
        """)

    elif isinstance(typ, ChoiceType):
        chosen = sorted((ref.resolve() for ref in typ.choices), key=lambda primtype: primtype.nbits, reverse=True)[0]

        assert isinstance(chosen, PrimitiveType)

        hpp.write(f"""
            typedef {('' if chosen.signed else 'u') + 'int64_t'} {typename};
        """)

        generate(typename, chosen, hpp, tpp, cpp)

    elif isinstance(typ, StructType) and typevar:
        generate_struct(typename.title(), typ, hpp, tpp, cpp, typevar=typevar)

    elif isinstance(typ, StructType) and typename:
        generate_struct(typename, typ, hpp, tpp, cpp)

    elif isinstance(typ, SectionType) and typename.endswith("Section"):
        kwargs = {
            'pre': f'auto capture = rdr.pos(); ::read<varuint32_t>(rdr); auto begin = rdr.pos(capture);',
            'post': f'check(r->size == rdr.pos() - begin, "Invalid {typename} size", rdr.pos() - begin);',
        }

        generate_struct(typename, typ.struct, hpp, tpp, cpp, **(kwargs if typename != "CustomSection" else {}))

        hpp.write(f"""
            template<> bool isa<{typename}>(Reader& rdr);
        """)

        cpp.write(f"""
            template<> bool isa<{typename}>(Reader& rdr) {{
                if (rdr.atend() or rdr.peek<byte_t>() != {typ.opcode})
                    return false;

                rdr.read<byte_t>();
                return true;
            }}
        """)

    elif isinstance(typ, InstructionType):
        hpp.write(f"""
            struct {typename} {{
                const {templtype(typ.opcode_type)} prefix;
                const {templtype(typ.opcode_type)} opcode;
                const {templtype(typ.depth_type)} depth;
                const {templtype(typ.nvals_type)} nvals;

                static constexpr {templtype(typ.nvals_type)} var_nvals = std::numeric_limits<{templtype(typ.nvals_type)}>::min();

                bool operator!=(const {typename}& i) const {{
                    return opcode != i.opcode;
                }}
            }};

            template<>
            struct Many<instruction_t> {{
                byte_t* begin;
                byte_t* end;
            }};

            template<>
            struct VarType<{typename}> {{
                using type = {typename};
            }};

            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr);
        """)

        cpp.write(f"""
            template<> {vartype(typename)} read<{templtype(typename)}>(Reader& rdr) {{
                const auto opcode = rdr.read<{templtype(typ.opcode_type)}>();
                {templtype(typ.depth_type)} depth = 0;
                {templtype(typ.nvals_type)} nvals = 0;
        """)

        cpp.f.write(' ' * 4)

        def write_instrs_reader(rngs, depth, nvals, imm_types, indent=0):
            cond = ' or '.join(f"opcode == 0x{rng:02X}" if isinstance(rng, int) else f"(0x{rng[0]:02X} <= opcode and opcode <= 0x{rng[1]:02X})" for rng in rngs)

            cpp.writeln(f"if ({cond}) {{")

            for imm_type in imm_types:
                cpp.writeln(f"::read<{templtype(imm_type)}>(rdr);", indent=indent+8)

            if depth != 0:
                cpp.writeln("++depth;" if depth == 1 else "--depth;" if depth == -1 else ';' if depth is None else None, indent=indent+8)

            if nvals != 0:
                cpp.writeln(f"nvals = {nvals if nvals is not None else typename + '::var_nvals'};", indent=indent+8)

            cpp.write(f"}} else", indent=indent+4); cpp.f.write(" ")

        def write_instrs_footer(prefix, indent):
            if prefix:
                cpp.write(f"""
                            error(fstr("Bad opcode {prefix:02X} %d while reading instruction", opcode));

                        return {{.prefix=0x{prefix:02x}, .opcode=opcode, .depth=depth, .nvals=nvals}};
                """, indent=indent)
            else:
                cpp.write(f"""
                            error(fstr("Bad opcode %02x while reading instruction", opcode));

                        return {{.prefix=0, .opcode=opcode, .depth=depth, .nvals=nvals}};
                """, indent=indent)

        prefixes = set()
        for prefix, rngs, _, depth, nvals, imm_types in typ.opcode_ranges():
            if prefix is None:
                write_instrs_reader(rngs, depth, nvals, imm_types)
            else:
                prefixes.add(prefix)

        for prefix in sorted(prefixes):
            cpp.writeln(f"if (opcode == 0x{prefix:02X}) {{")
            cpp.writeln(f"const auto opcode = rdr.read<{templtype(typ.opcode_type)}>();", indent=8);
            cpp.f.write(' ' * 8)
            for rngs, _, depth, nvals, imm_types in (item[1:] for item in typ.opcode_ranges() if item[0] == prefix):
                write_instrs_reader(rngs, depth, nvals, imm_types, indent=4)
            write_instrs_footer(prefix=prefix, indent=8)
            cpp.write(f"}} else", indent=4); cpp.f.write(" ")

        write_instrs_footer(prefix=None, indent=4)
        cpp.write(f"}}\n", indent=0);

        cpp.write(f"""
            template<>
            struct ReaderFor<Many<{typename}>> {{
                static typename VarType<Many<{typename}>>::type read(Reader& rdr) {{
                    const auto begin = rdr.imgpos();

                    for(int32_t depth = 1; depth > 0;) {{
                        const auto instr = ::read<instruction_t>(rdr);
                        depth += instr.depth;

                        check(depth >= 0 or instr.opcode == {typ.TERMINAL}, "Invalid final instruction");
                    }};

                    const auto end = rdr.imgpos();

                    return {{.begin=begin, .end=end}};
                }}
            }};
        """)

    elif isinstance(typ, ModuleType):
        def read_custom_section(typename, accessor, field, tgt, indent):
            if field.name.endswith('Section') or field.name == "version":
                tgt.writeln("::read<Many<const CustomSection>>(rdr);", indent=4)

        generate_struct(typename, typ.struct, hpp, tpp, cpp, post_field=read_custom_section)
    else:
        raise NotImplementedError(f"{typename} {type(typ)}")


def init(hpp, tpp, cpp):
    hpp.write("#include <limits>\n\n")


class Writer:
    def __init__(self, f):
        self.f = f

    def write(self, s, indent=0):
        lines = s.strip(' ').split('\n')
        dedent = min(re.match(r"^(\s*)[^ ]+.*$", line).group(1) for line in lines if len(line.strip()) > 0)
        self.f.write('\n'.join(' ' * indent + line[len(dedent):] if line.startswith(dedent) else line for line in lines))

    def writeln(self, s, indent=0):
        self.f.write(' '*indent + s + "\n")

if __name__ == "__main__":
    import sys
    from pathlib import Path

    args = [arg for arg in sys.argv[1:]]

    if len(args) != 1:
        print("Usage gen.py <targetdir>")
        exit(1)

    dirpath = Path(args[0])

    dirpath.mkdir(exist_ok=True)

    with open(dirpath / "spec.hpp", "w") as hpp, open(dirpath / "spec.tpp", "w") as tpp, open(dirpath / "spec.cpp", "w") as cpp:
        init(Writer(hpp), Writer(tpp), Writer(cpp))

        for typename, typ in chain(poly_types.items(), sorted(named_types.items(), key=lambda item: item[1].depth)):
            generate(typename, typ, Writer(hpp), Writer(tpp), Writer(cpp))