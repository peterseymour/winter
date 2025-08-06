import struct
from float import Float, NaNClass


class Vec128:
    TYPES = ('i8x16', 'i16x8', 'i32x4', 'i64x2', 'f32x4', 'f64x2')

    @staticmethod
    def parse(typ, xs):
        assert isinstance(xs, list)
        return (IVec128 if typ[0] == 'i' else FVec128 if typ[0] == 'f' else None)(typ, xs)

    @staticmethod
    def decode(typ, i):
        assert isinstance(i, int)

        byts = i.to_bytes(length=16, byteorder='little')

        sz = int(typ[1:]) // 8
        lanes = []
        for i in range(16 // sz):
            lanes.append(bytes(reversed(byts[i*sz:(i+1)*sz])))

        return (IVec128 if typ[0] == 'i' else FVec128 if typ[0] == 'f' else None).decode(typ, lanes)

    def __init__(self, typ, lanes):
        assert isinstance(lanes, list)
        self.typ = typ
        self.lanes = lanes
        self.width = int(typ[1:])

    def bytes_to_int(self, byts):
        i = 0
        for n in reversed(byts):
            i = (i << 8) + n

        if i >= 2 ** 127:
            i -= 2 ** 128

        return i

    def __len__(self):
        return len(self.lanes)

    def __eq__(self, v):
        assert isinstance(v, Vec128)

        return v.typ == self.typ and all(self.lane_equiv(x, y) for x, y in zip(v.lanes, self.lanes))

    def __str__(self):
        return f"<{self.typ} {' '.join(map(str, self.lanes))}>"


class IVec128(Vec128):
    CODES = {'i8': 'B', 'i16': 'H', 'i32': 'I', 'i64': 'Q'}

    @staticmethod
    def decode(typ, lanes):
        return IVec128(typ, [int.from_bytes(x, byteorder='big', signed=False) for x in lanes])

    def norm(self, x):
        return x if x >= 0 else x + (2**self.width)

    def lane_equiv(self, x, y):
        return self.norm(x) == self.norm(y)

    def to_int(self):
        code = self.CODES[self.typ]

        byts = struct.pack("<" + ''.join((code.lower() if isinstance(x, int) and x < 0 else code) for x in self.lanes), *self.lanes)

        return self.bytes_to_int(byts)


class FVec128(Vec128):
    @staticmethod
    def decode(typ, lanes):
        return FVec128(typ, list(map(Float.frombytes, lanes)))

    def lane_equiv(self, x, y):
        if isinstance(x, NaNClass) and isinstance(y, NaNClass):
            return x.equiv(y)

        return x.with_type(self.typ) == y

    def to_int(self):
        byts = b''

        for x in self.lanes:
            byts += bytes(reversed(x.with_type(self.typ).byts))

        return self.bytes_to_int(byts)