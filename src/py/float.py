import struct
import math



def _2__(n):
    return 1 << n

def ones(n):
    return _2__(n) - 1


class Float:
    class Literal:
        def __eq__(self, f):
            raise NotImplementedError

        def with_type(self, typ):
            raise NotImplementedError

    @classmethod
    def parse(Self, s):
        if s[0] in ('+', '-'):
            sign, content = s[0], s[1:].lower()
        else:
            sign, content = '+', s.lower()

        if content.startswith('nan:0x'):
            return NaNWithPayload(sign, content[6:])
        elif content in ('nan', 'nan:canonical'):
            return CanonicalNaN(sign)
        elif content in ('nan:arithmetic',):
            return ArithmeticNaN(sign)
        elif content in ('inf',):
            return Infinity(sign)
        elif content[:6] in ('f32:0x', 'f64:0x'):
            assert sign == '+'
            return NaNAsRawHex(s[:3], s[6:])
        else:
            f = float.fromhex(s) if content.startswith('0x') else float(s)
            assert math.isfinite(f)
            return Finite(f.hex())

    @staticmethod
    def fromfloat(typ, f):
        if math.isfinite(f):
            return (F32Finite if typ == 'f32' else F64Finite)(f.hex())
        elif math.isinf(f):
            return (F32Infinity if typ == 'f32' else F64Infinity)('+' if f > 0 else '-')
        elif math.isnan(f):
            byts = struct.pack('>f' if typ == 'f32' else '>d', f)
            return (F32NaN if typ == 'f32' else F64NaN)(int.from_bytes(byts, byteorder='big'))
        else:
            assert False

    @staticmethod
    def frombytes(byts):
        f = struct.unpack('>f' if len(byts) == 4 else '>d', byts)[0]

        #nan payload not maintained
        if math.isnan(f):
            return (F32NaN if len(byts) == 4 else F64NaN)(int.from_bytes(byts, byteorder='big'))
        else:
            return Float.fromfloat('f32' if len(byts) == 4 else 'f64', f)

    def __init__(self, byts):
        assert isinstance(byts, bytes) and len(byts) in (4, 8)
        self.byts = byts
        self.hexval = struct.unpack('>f' if self.TYPE == 'f32' else '>d', byts)[0].hex()

    @property
    def bits(self):
        return int.from_bytes(self.byts, byteorder='big')

    @property
    def sign(self):
        return '-' if self.byts[0] & 0x80 else '+'

    @property
    def exp(self):
        return (self.bits >> self.SIGNIF) & ones(self.EXP)

    @property
    def payload(self):
        return self.bits & ones(self.SIGNIF)

    def to_float(self):
        return struct.unpack('>f' if self.TYPE == 'f32' else '>d', self.byts)[0]

    def __eq__(self, f):
        return isinstance(f, Float) and self.byts == f.byts

    def __str__(self):
        return self.hexval



class FiniteClass:
    pass

class InfinityClass:
    pass

class NaNClass:
    pass


""" Literals """
class Finite(Float.Literal, FiniteClass):
    def __init__(self, hexval):
        self.hexval = hexval

    def with_type(self, typ):
        if typ == 'f32':
            return F32Finite(self.hexval)
        elif typ == 'f64':
            return F64Finite(self.hexval)
        else:
            assert False

    def __str__(self):
        return self.hexval


class Infinity(Float.Literal, InfinityClass):
    def __init__(self, sign):
        self.sign = sign

    def with_type(self, typ):
        return (F32Infinity if typ == 'f32' else F64Infinity).INFS[self.sign == '-']

    def __str__(self):
        return "-inf" if self.sign == '-' else "inf"


class NaNAsRawHex(Float.Literal, NaNClass):
    def __init__(self, typ, hexraw):
        self.typ = typ
        self.hexraw = hexraw

        assert len(self.hexraw) == 2 * (4 if typ == 'f32' else 8)

    def with_type(self, typ):
        assert self.typ == typ

        return Float.frombytes(bytes.fromhex(self.hexraw))

    def __str__(self):
        return f"{self.typ}:0x{self.hexraw}"


class NaNWithPayload(Float.Literal, NaNClass):
    def __init__(self, sign, payload):
        self.sign = sign
        self.payload = payload

    def with_type(self, typ):
        assert typ == 'f64' or len(self.payload) <= 8

        cls = (F32NaN if typ == 'f32' else F64NaN)

        return cls(cls.NANS[0 if self.sign == '+' else 1] | int(self.payload, 16))

    def __str__(self):
        return str(self.with_type('f32' if len(self.payload) <= 8 else 'f64'))


class CanonicalNaN(Float.Literal, NaNClass):
    def __init__(self, sign, name=None):
        self.sign = sign

    def with_type(self, typ):
        return (F32NaN if typ == 'f32' else F64NaN).CANONS[0 if self.sign == '+' else 1]

    def __str__(self):
        return f"{self.sign if self.sign == '-' else ''}nan"


class ArithmeticNaN(Float.Literal, NaNClass):
    def __init__(self, sign, name=None):
        self.sign = sign

    def with_type(self, typ):
        return (F32NaN if typ == 'f32' else F64NaN).ARITHMETICS[0 if self.sign == '+' else 1]

    def __str__(self):
        return "nan:arithmetic"


""" Fixed Width """

class F32Finite(Float, FiniteClass):
    TYPE = 'f32'

    def __init__(self, hexval):
        super().__init__(struct.pack('>f', float.fromhex(hexval)))


class F64Finite(Float, FiniteClass):
    TYPE = 'f64'

    def __init__(self, hexval):
        super().__init__(struct.pack('>d', float.fromhex(hexval)))


class F32Infinity(Float, InfinityClass):
    TYPE = 'f32'

    def __init__(self, sign):
        super().__init__(struct.pack('>f', float(f"{sign}inf")))

F32Infinity.INFS = [F32Infinity('+'), F32Infinity('-')]


class F64Infinity(Float, InfinityClass):
    TYPE = 'f64'

    def __init__(self, sign):
        super().__init__(struct.pack('>d', float(f"{sign}inf")))

F64Infinity.INFS = [F64Infinity('+'), F64Infinity('-')]

class F32NaN(Float, NaNClass):
    TYPE = 'f32'
    SIGNIF = 23
    EXP    = 8
    NANS = [ones(EXP) << SIGNIF, ones(1 + EXP) << SIGNIF] # 0x7f800000         0xff800000
    CANON_PL = _2__(SIGNIF - 1)                              # 0x00400000
    ARITH_PL = CANON_PL + 5

    def __init__(self, bits):
        super().__init__(bits.to_bytes(4, byteorder='big'))

    def is_canonical(self):
        return self in self.CANONS

    def __str__(self):
        return f"{super().__str__()}:0x{self.byts.hex()}"

F32NaN.CANONS =      [F32NaN(F32NaN.NANS[0] | F32NaN.CANON_PL), F32NaN(F32NaN.NANS[1] | F32NaN.CANON_PL)]
F32NaN.ARITHMETICS = [F32NaN(F32NaN.NANS[0] | F32NaN.ARITH_PL), F32NaN(F32NaN.NANS[1] | F32NaN.ARITH_PL)]


class F64NaN(Float, NaNClass):
    TYPE = 'f64'
    SIGNIF = 52
    EXP    = 11
    NANS = [ones(EXP) << SIGNIF, ones(1 + EXP) << SIGNIF] # 0x7ff0000000000000 0xfff0000000000000
    CANON_PL = _2__(SIGNIF - 1)                              # 0x0008000000000000
    ARITH_PL = CANON_PL + 5

    def __init__(self, bits):
        super().__init__(bits.to_bytes(8, byteorder='big'))

    def is_canonical(self):
        return self in self.CANONS

    def __str__(self):
        return f"{super().__str__()}:0x{self.byts.hex()}"

F64NaN.CANONS =      [F64NaN(F64NaN.NANS[0] | F64NaN.CANON_PL), F64NaN(F64NaN.NANS[1] | F64NaN.CANON_PL)]
F64NaN.ARITHMETICS = [F64NaN(F64NaN.NANS[0] | F64NaN.ARITH_PL), F64NaN(F64NaN.NANS[1] | F64NaN.ARITH_PL)]


assert isinstance(Float.parse('3.14'), Finite)
assert isinstance(Float.parse('-nan'), CanonicalNaN)
assert isinstance(Float.parse('-nan:canonical'), CanonicalNaN)
assert isinstance(Float.parse('nan:arithmetic'), ArithmeticNaN)
assert isinstance(Float.parse('-0x34.adfe34'), Finite)

assert isinstance(Float.parse('nan:0x0034'), NaNWithPayload)
assert isinstance(Float.parse('nan:0x0034').with_type('f32'), F32NaN)
assert isinstance(Float.parse('-nan:0x0034'), NaNWithPayload)
assert isinstance(Float.parse('-nan:0x0034').with_type('f32'), F32NaN)
assert str(Float.parse('nan:0x0034')) == 'nan:0x7f800034'
assert str(Float.parse('nan:0x0000000034')) == 'nan:0x7ff0000000000034'
assert str(Float.parse('-nan:0x0034')) == 'nan:0xff800034'
assert str(Float.parse('-nan:0x0000000034')) == 'nan:0xfff0000000000034'

assert isinstance(Float.parse('nan:0x2000000034'), NaNWithPayload)
assert isinstance(Float.parse('nan:0x2000000034').with_type('f64'), F64NaN)


#3.14
assert isinstance(Float.parse('f32:0x4048f5c3'), NaNAsRawHex)
assert Float.parse('f32:0x4048f5c3').with_type('f32') == F32Finite(hexval='0x1.91eb86p+1')
assert f"{F32Finite(hexval='0x1.91eb860000000p+1')} {F64Finite(hexval='-0x1.91eb8p+1')}" == "0x1.91eb860000000p+1 -0x1.91eb800000000p+1"


#-inf
assert isinstance(Float.parse('f32:0xff800000'), NaNAsRawHex)
assert Float.parse('f32:0xff800000').with_type('f32') == F32Infinity('-')
assert f"{F32Infinity('+')} {F64Infinity('-')}" == "inf -inf"

#nan with payload
assert isinstance(Float.parse('f32:0x7f800034'), NaNAsRawHex)
assert Float.parse('f32:0x7f800034').with_type('f32') == F32NaN(0x7f800034)
assert f"{F32NaN.CANONS[0]} {F64NaN.CANONS[1]}" == "nan:0x7fc00000 nan:0xfff8000000000000"
assert f"{F32NaN.ARITHMETICS[0]} {F64NaN.ARITHMETICS[1]}" == "nan:0x7fc00005 nan:0xfff8000000000005"
assert f"{F32NaN(0x7f800034)} {F64NaN(0xfffc000000000034)}" == "nan:0x7f800034 nan:0xfffc000000000034"

assert F32NaN(0x7fc00000).is_canonical()
assert not F32NaN(0x7fc00001).is_canonical()
assert not F32NaN(0x7f800034).is_canonical()