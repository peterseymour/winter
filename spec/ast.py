import json, re
from collections import defaultdict


with open("WebAssembly.json") as f:
    standard = json.load(f)


named_types = {}
poly_types = {}
type_ref_counts = defaultdict(int)


class Type:
    @staticmethod
    def is_ref_type():
        return False

    @classmethod
    def coerce(Self, v):
        if isinstance(v, str):
            return TypeRef(name=v) if not re.match(r"^\[.*\]$", v) else Self.coerce([v[1:-1]])
        elif isinstance(v, list) and len(v) == 1:
            return ArrayOf(element_type=Self.coerce(v[0]))
        elif isinstance(v, Type):
            return v
        else:
            raise ValueError(str(v))

    @classmethod
    def flatten(Self, typ):
        typ = Self.coerce(typ).resolve()

        try:
            if isinstance(typ, ArrayOf):
                return Self.flatten(typ.element_type)
            else:
                return typ
        except KeyError as e:
            print(f"WARNING: Missing type '{typ.name}'")

    def __init__(self, **kwargs):
        for k, v in kwargs.items():
            setattr(self, k, v)
        self.depth = 0 if self.is_ref_type() else -100

    def resolve(self):
        return self

    def bind(self, depth=0):
        for v in self.binds() if hasattr(self, 'binds') else (getattr(self, name) for name in self.BINDS):
            typ = self.flatten(self.coerce(v))
            if typ:
                type_ref_counts[typ] += 1
                typ.bind(depth=depth-1)
        self.depth = min(self.depth, depth)

    @classmethod
    def parse_name(Self, name):
        if re.match(r"^\[.*\]$", name):
            return True, f"arr({name[1:-1]})"

        return False, name

    @classmethod
    def emit_name(Self, typ):
        return Self.coerce(typ).emit()


class Condition:
    def __init__(self, op, getter, value):
        self.op = op
        self.getter = getter
        self.value = value

    def emit(self):
        return f"{self.getter.emit()}{self.op}{self.value}"

class Boolean:
    def __init__(self, istrue):
        self.istrue = istrue

    def emit(self):
        return "T" if self.istrue else "F"

class Attribute:
    def __init__(self, name):
        self.name = name

    def emit(self):
        return self.name

class TableAttribute:
    def __init__(self, index, attr):
        self.index = index
        self.attr = attr

    def emit(self):
        return f"tables[{self.index}].{self.attr}"


class PrimitiveType(Type):
    BINDS = ()

    @classmethod
    def parse(Self, spec):
        size_in_bytes = spec.pop('Size (in bytes)')
        description = spec.pop('Desc')

        for sign in ('unsigned', 'signed'):
            if sign in description:
                signed = (sign == 'signed')
                break
        else:
            signed = None

        if 'LEB128' in description:
            assert signed is not None

            nbits = int(re.search(r"(\d+) bit", description).group(1))

            if signed:
                minval, maxval = -2**(nbits-1), (2**(nbits-1))-1
            else:
                minval, maxval = 0, (2**nbits)-1

            return LEB128Type(
                signed = signed,
                nbits = nbits,
                minval = minval,
                maxval = maxval,
            )
        elif 'IEEE 754-2008' in description:
            assert signed is None

            return FloatType(
                nbits = int(size_in_bytes) * 8,
            )
        elif 'vector' in description:
            assert signed is None

            nbits = int(re.search(r"(\d+) bit", description).group(1))

            return VectorType(
                nbits = nbits,
            )
        else:
            assert signed is not None

            return IntegerType(
                signed = signed,
                nbits = int(size_in_bytes) * 8,
            )

    def emit(self):
        return f"<{self.size_in_bytes} byte {self.encoding}>"



class IntegerType(PrimitiveType):
    def emit(self):
        return f"prim '{'signed' if self.signed else 'unsigned'} {self.nbits}-bit integer'"


class FloatType(PrimitiveType):
    def emit(self):
        return f"prim '{self.nbits}-bit IEEE 754-2008 float'"


class LEB128Type(PrimitiveType):
    def emit(self):
        return f"prim '{'signed' if self.signed else 'unsigned'} LEB128 encoded integer [{self.minval:#x}, {self.maxval:#x})'"


class VectorType(PrimitiveType):
    def emit(self):
        return f"prim {self.nbits}-bit vector'"

class AliasType:
    @classmethod
    def parse(Self, spec):
        if 'Enum' in spec:
            return EnumType.parse(spec)
        elif 'BitFields' in spec:
            return BitFieldType.parse(spec)
        else:
            alias = spec.pop('Alias')

            if isinstance(alias, (str, list)):
                return TypeRef.parse(alias)
            elif 'Choice' in alias:
                return ChoiceType.parse(alias)
            else:
                raise NotImplementedError(str(alias))


class TypeRef(Type):
    BINDS = {'name'}

    @classmethod
    def parse(Self, spec):
        if isinstance(spec, (str, list)):
            return Type.coerce(spec)
        else:
            raise NotImplementedError(str(spec))

    def resolve(self):
        return named_types[self.name]

    def emit(self):
        return f"{self.name}"


class ArrayOf(Type):
    BINDS = {'element_type'}

    def emit(self):
        return f"arr({Type.emit_name(self.element_type)})"


class ChoiceType(Type):
    def binds(self):
        for choice in self.choices:
            yield choice

    @classmethod
    def parse(Self, spec):
        choices = spec.pop('Choice')
        assert len(spec) == 0
        return Self(
            choices = [TypeRef.parse(choice) for choice in choices],
        )

    def emit(self):
        return f"({'|'.join(Type.emit_name(choice) for choice in self.choices)})"


class EnumType(Type):
    class Option:
        def __init__(self, name, value):
            self.name = name
            self.value = value

        def emit(self):
            return f"{self.name}={self.value}"

    class InheritedOption:
        def __init__(self, base_type, name):
            self.base_type = Type.coerce(base_type)
            self.name = name

        def emit(self):
            return f"{Type.emit_name(self.base_type)}.{self.name}"

    def binds(self):
        for option in self.options:
            if isinstance(option, self.Option):
                pass
            elif isinstance(option, self.InheritedOption):
                yield option.base_type
            else:
                raise NotImplementedError

        yield from ()

    @classmethod
    def parse(Self, spec):
        enum = spec.pop('Enum')
        description = spec.pop('Desc', None)

        if isinstance(enum, dict):
            options = [Self.Option(name, value) for name, value in enum.items()]
        elif isinstance(enum, list):
            options = [Self.InheritedOption(value.pop('Type'), value.pop('Value')) for value in enum]
            assert all(len(value) == 0 for value in enum)
        else:
            raise NotImplementedError

        return Self(
            alias = AliasType.parse(spec),
            options = options,
        )

    def emit(self):
        return f"{Type.emit_name(self.alias)}[{'|'.join(option.emit() for option in self.options)}]"


class BitFieldType(Type):
    BINDS = ()

    class BitField:
        @classmethod
        def parse(Self, name, spec):
            rng = spec.pop('Range')

            return Self(
                name = name,
                begin = rng[0],
                end = rng[1],
            )

        def __init__(self, name, begin, end):
            self.name = name
            self.begin = begin
            self.end = end

    @classmethod
    def parse(Self, spec):
        bit_fields = spec.pop('BitFields')

        return Self(
            alias = AliasType.parse(spec),
            bit_fields = sorted((Self.BitField.parse(fieldname, fieldspec) for fieldname, fieldspec in bit_fields.items()), key=lambda bit_field: bit_field.begin),
        )

    def emit(self):
        return f"{Type.emit_name(self.alias)}[" + ','.join(f"{bit_field.name}={bit_field.begin}:{bit_field.end}" for bit_field in self.bit_fields) + "]"



class StructType(Type):
    @staticmethod
    def is_ref_type():
        return True

    def binds(self):
        for field in self.fields:
            yield field.type

        for _, struct in self.cond_fields:
            yield struct

        if self.tail_type:
            yield self.tail_type

    class Field:
        @classmethod
        def parse(Self, name, spec):
            description = spec.pop('Desc', None)

            return Self(
                name = name,
                type = Type.coerce(spec.pop('Type')),
                count = spec.pop('Count', 1),
            )

        def __init__(self, name, type, count):
            self.name = name
            self.type = type
            self.count = count

    @classmethod
    def parse(Self, spec):
        fields = []
        cond_fields = []
        tail_type = None
        for name in list(spec):
            field_spec = spec.pop(name)

            if re.match(r"^\w+$", name):
                fields.append(Self.Field.parse(name, field_spec))
                assert len(field_spec) == 0
            elif name == '*?':
                for cond, fields_spec in field_spec.items():
                    if len({'='}.intersection(cond)) > 0:
                        cond_elem, cond_name, cond_op, cond_value = None, *re.match(r"^(.+)([=&])(\w+)$", cond).groups()

                        m = re.match(r"^(\w+)$", cond_name)

                        if not m:
                            raise NotImplementedError

                        cond_elem = Attribute(name=cond_name)

                        cond_fields.append((Condition(cond_op, cond_elem, cond_value), TypeRef.parse(fields_spec) if isinstance(fields_spec, str) else StructType.parse(fields_spec)))
                        assert isinstance(fields_spec, str) or len(fields_spec) == 0
                    elif cond in ("true", "false"):
                        cond_fields.append((Boolean(cond == "true"), TypeRef.parse(fields_spec) if isinstance(fields_spec, str) else StructType.parse(fields_spec)))
                        assert isinstance(fields_spec, str) or len(fields_spec) == 0
                    else:
                        raise NotImplementedError
            elif name == '*':
                assert tail_type is None
                tail_type = TypeRef.parse(field_spec)
            else:
                raise NotImplementedError

        return Self(
            fields = fields,
            cond_fields = cond_fields,
            tail_type = tail_type,
        )


    def emit(self):
        for field in self.fields:
            yield f"{field.name} : {Type.emit_name(field.type)}{'' if field.count == 1 else field.count if field.count in ('?', '*') else '*' + field.count}"

        for cond, struct in self.cond_fields:
            lines = struct.emit()

            if isinstance(lines, str):
                lines = [lines]

            for line in lines:
                yield f"{cond.emit()} => {line}"

        if self.tail_type:
            yield self.tail_type.emit()


class EncodingType(Type):
    @classmethod
    def parse(Self, spec):
        description = spec.pop('Desc')

        return AliasType.parse(spec)


class ComplexType(Type):
    @classmethod
    def parse(Self, spec):
        if 'Fields' in spec:
            return StructType.parse(spec.pop('Fields'))
        elif 'Enum' in spec:
            return EnumType.parse(spec)
        else:
            raise NotImplementedError


class LanguageType(Type):
    @classmethod
    def parse(Self, spec):
        if 'Fields' in spec:
            return StructType.parse(spec.pop('Fields'))
        elif 'Enum' in spec:
            return EnumType.parse(spec)
        else:
            raise NotImplementedError


class InstructionType(Type):
    TERMINAL = 0x0B
    opcode_type = 'byte_t'
    depth_type = 'int8_t'
    nvals_type = 'int8_t'

    class Instruction:
        @classmethod
        def parse(Self, params):
            if isinstance(params, list):
                return Self(
                    imms = [(imm[0], Type.coerce(imm[1])) for imm in params if isinstance(imm, list)],
                    depth = params[-1].get('depth', 0) if params and isinstance(params[-1], dict) else 0,
                    nvals = params[-1].get('stack_delta', 0) if params and isinstance(params[-1], dict) else 0,
                )
            elif isinstance(params, dict): #multi-byte opcodes
                return InstructionType.parse(params)
            else:
                assert False

        def __init__(self, imms, depth, nvals):
            self.imms = imms
            self.depth = depth
            self.nvals = nvals

    def binds(self):
        for instr in self.instrset.values():
            if isinstance(instr, InstructionType):
                yield from instr.binds()
            else:
                for name, typ in instr.imms:
                    yield typ

    @classmethod
    def parse(Self, instrset):
        return InstructionType(
            instrset = {int(opcode, 16 if opcode.startswith('0x') else 10): Self.Instruction.parse(instrset.pop(opcode)) for opcode in sorted(list(instrset))},
        )

    def opcode_ranges(self, prefix=None):
        by_imms = defaultdict(list)
        lookup = {}

        for opcode, instr in self.instrset.items():
            if isinstance(instr, InstructionType):
                continue

            imm_types = tuple(typ for name, typ in instr.imms)
            key = tuple(typ.emit() for typ in imm_types) + (instr.depth, instr.nvals)
            by_imms[key].append(opcode)
            lookup[key] = imm_types

        for key, opcodes in by_imms.items():
            rngs = []

            def add(b, l):
                if b == l:
                    rngs.append(b)
                else:
                    rngs.append((b, l))

            b = l = None
            for opcode in opcodes + [None]:
                if b is None:
                    b = opcode
                    l = opcode
                elif opcode is None:
                    add(b, l)
                elif opcode == l + 1:
                    l = opcode
                else:
                    add(b, l)
                    b = opcode
                    l = opcode

            yield prefix, rngs, list(key[:-2]), *key[-2:], lookup[key]

        for opcode, instr in self.instrset.items():
            if isinstance(instr, InstructionType):
                yield from instr.opcode_ranges(prefix=opcode)

    def emit(self):
        for prefix, rngs, imm_type_names, depth, nvals, imm_types in self.opcode_ranges():
            yield ' '.join(([f"{prefix:#04X}"] if prefix else []) + ['|'.join(f"{rng[0]:#04X}-{rng[1]:#04X}" if isinstance(rng, tuple) else f"{rng:#04X}" for rng in rngs)] + imm_type_names) + (f" (nvals {nvals if nvals is not None else '*'})") + (f" (depth {depth})" if depth != 0 else '')


class ExecutionType(Type):
    @classmethod
    def parse(Self, spec):
        return InstructionType.parse(spec)


class SectionType(Type):
    BINDS = {'struct'}

    @staticmethod
    def is_ref_type():
        return True

    @classmethod
    def parse(Self, spec):
        opcode = spec.pop('OpCode')

        return Self(
            opcode = opcode,
            struct = StructType.parse(spec.pop('Fields'))
        )

    def emit(self):
        yield f"{self.opcode}"
        yield from self.struct.emit()


class ModuleType(Type):
    BINDS = {'struct'}

    @staticmethod
    def is_ref_type():
        return True

    @classmethod
    def parse(Self, spec):
        return Self(
            struct = StructType.parse(spec.pop('Fields')),
        )

    def emit(self):
        for line in self.struct.emit():
            yield line
            if 'magic' not in line:
                yield ": CustomSection*"


for TypeCls in (PrimitiveType, EncodingType, ComplexType, LanguageType, ExecutionType, SectionType, ModuleType):
    for name, spec in standard.pop(f'{TypeCls.__name__}s').items():
        ispoly, name = TypeCls.parse_name(name)

        catalog = poly_types if ispoly else named_types
        assert name not in catalog
        catalog[name] = TypeCls.parse(spec)
        assert len(spec) == 0


for typ in named_types.values():
    typ.bind()

assert not standard
