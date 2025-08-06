#! /usr/bin/python3
#python3 build.py > WebAssembly.json

from pathlib import Path
from html.parser import HTMLParser
from itertools import chain
import re, json, sys

def debug(*args):
    print(*(json.dumps(arg, indent=2) if isinstance(arg, (list, dict)) else arg for arg in args), file=sys.stderr)


URL = "https://github.com/sunfishcode/wasm-reference-manual/blob/master/WebAssembly.md"
FILE = Path(Path(URL).name).with_suffix('.html')
STYLE = """
table {
  margin: 0 auto;
}

table {
  color: #333;
  background: white;
  border: 1px solid grey;
  font-size: 12pt;
  border-collapse: collapse;
}
table thead th,
table tfoot th {
  color: #777;
  background: rgba(0,0,0,.1);
}
table caption {
  padding:.5em;
}
table th,
table td {
  padding: .5em;
  border: 1px solid lightgrey;
}
"""


def download():
    import requests

    return requests.get(URL).text

def merge(d1, d2):
    d = {}

    for k, v in d2.items():
        if k in d1:
            d[k] = merge(d1[k], d2[k])
        else:
            d[k] = v

    for k, v in d1.items():
        if k not in d2:
            d[k] = v

    return d


class SpecParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.path = []
        self.data = None

    def handle_starttag(self, tag, attrs):
        while self.path and tag in self.AUTOCLOSE and tag in self.path:
            self.handle_endtag(self.path[-1])

        if tag in self.TAGS:
            handler = f"handle_starttag_{tag}"

            if hasattr(self, handler):
                getattr(self, handler)()

            assert tag not in self.path
            self.data = []
            self.path.append(tag)

            self.handle()

    def handle_data(self, data):
        if self.data is not None:
            self.data.append(data.strip())

    def handle_endtag(self, tag):
        if tag in self.TAGS:
            if tag in self.AUTOCLOSE and (not self.path or self.path[-1] != tag):
                assert self.data is None
                return

            if self.data:
                self.handle(' '.join(self.data))

            handler = f"handle_endtag_{tag}"

            if hasattr(self, handler):
                getattr(self, handler)()

            assert self.path[-1] == tag
            self.path.pop()
            self.data = None


class SpecHandler(SpecParser):
    TAGS = {'h3', 'h4', 'table', 'thead', 'tbody', 'tr', 'th', 'td', 'p', 'ol', 'ul', 'li'}
    AUTOCLOSE = {'li', 'ol', 'ul'}

    PRIM_TYPES = ['Name', 'Size (in bytes)', 'Description']
    FIELD = ['Field Name', 'Type', 'Description']
    OPCODE = ['Mnemonic', 'Opcode', 'Depth', 'Immediates', 'Name', 'Families', 'Signature']

    DEPTHS = {
        'block': '+1',
        'loop': '+1',
        'if': '+1',
        'else': '-1/+1',
        'end': '-1',
    }

    OVERRIDE = [
        "table_initializer_t",
        "data_initializer_t",
        "block_signature_type_t",
        "table_description_t",
    ]

    REMOVE = [
        "table_element_type_t",
    ]

    OVERRIDE_INSTRS = [
        '0x11',
    ]

    BASE_PRIM_TYPES = {
        "byte_t": {
            "Size (in bytes)": "1",
            "Desc": "unsigned; value limited to 8 bits",
        },
        "v128_t": {
            "Size (in bytes)": "16",
            "Desc": "vector; value limited to 128 bits"
        },
    }

    BASE_ENC_TYPES = {
        'type_encoding_t': {
            "Enum": {
                'v128': "-0x05",
                'externref': "-0x11",
            }
        }
    }

    BASE_COMPLEX_TYPES = {
        "[T]": {
            "Fields": {
                "size": {
                    "Type": "varuint32_t",
                    "Desc": "The number of elements in the array",
                },
                "elems": {
                    "Type": "T",
                    "Count": "size",
                    "Desc": "The elements",
                },
            },
        },
        "instantiation_time_initializer_t": {
            "Fields": {
                "instruction": {
                    "Type": "instruction_t",
                    "Count": "*",
                },
            }
        },
        "table_initializer_t": {
            "Fields": {
                "flags": {
                    "Type": "varuint32_t",
                    "Desc": "bit-packed flags",
                },
                "*?": {
                    "flags=0x00": {
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the element in table 0 to start at",
                        },
                        "elems": {
                            "Type": ["varuint32_t"],
                            "Desc": "indices into the function index space",
                        },
                    },
                    "flags=0x01": {
                        "elemkind": {
                            "Type": "byte_t",
                            "Desc": "the kind of table element",
                        },
                        "elems": {
                            "Type": ["varuint32_t"],
                            "Desc": "indices into the function index space",
                        },
                    },
                    "flags=0x02": {
                        "tableidx": {
                            "Type": "varuint32_t",
                            "Desc": "the index of the table to initializer",
                        },
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the element in the table to start at",
                        },
                        "elemkind": {
                            "Type": "byte_t",
                            "Desc": "the kind of table element",
                        },
                        "elems": {
                            "Type": ["varuint32_t"],
                            "Desc": "indices into the function index space",
                        },
                    },
                    "flags=0x03": {
                        "elemkind": {
                            "Type": "byte_t",
                            "Desc": "the kind of table element",
                        },
                        "elems": {
                            "Type": ["varuint32_t"],
                            "Desc": "indices into the function index space",
                        },
                    },
                    "flags=0x04": {
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the element in table 0 to start at",
                        },
                        "elems": {
                            "Type": ["instantiation_time_initializer_t"],
                            "Desc": "element values",
                        },
                    },
                    "flags=0x05": {
                        "elemtype": {
                            "Type": "reference_type_t",
                            "Desc": "the type of table element",
                        },
                        "elems": {
                            "Type": ["instantiation_time_initializer_t"],
                            "Desc": "element values",
                        },
                    },
                    "flags=0x06": {
                        "tableidx": {
                            "Type": "varuint32_t",
                            "Desc": "the index of the table to initializer",
                        },
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the element in the table to start at",
                        },
                        "elemtype": {
                            "Type": "reference_type_t",
                            "Desc": "the type of table element",
                        },
                        "elems": {
                            "Type": ["instantiation_time_initializer_t"],
                            "Desc": "element values",
                        },
                    },
                    "flags=0x07": {
                        "elemtype": {
                            "Type": "reference_type_t",
                            "Desc": "the type of table element",
                        },
                        "elems": {
                            "Type": ["instantiation_time_initializer_t"],
                            "Desc": "element values",
                        },
                    },
                }
            }
        },
        "data_initializer_t": {
            "Fields": {
                "flags": {
                    "Type": "varuint32_t",
                    "Desc": "bit-packed flags",
                },
                "*?": {
                    "flags=0x00": {
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the byte in memory 0 to start at",
                        },
                        "data": {
                            "Type": ["byte_t"],
                            "Desc": "byte data",
                        },
                    },
                    "flags=0x01": {
                        "data": {
                            "Type": ["byte_t"],
                            "Desc": "byte data",
                        },
                    },
                    "flags=0x02": {
                        "memidx": {
                            "Type": "varuint32_t",
                            "Desc": "the index in the memory space",
                        },
                        "offset": {
                            "Type": "instantiation_time_initializer_t",
                            "Desc": "the index of the byte in memory 0 to start at",
                        },
                        "data": {
                            "Type": ["byte_t"],
                            "Desc": "byte data",
                        },
                    },
                }
            }
        },
        "table_description_t": {
            "Fields": {
                "element_type": {
                    "Type": "reference_type_t",
                    "Desc": "the element type of the table"
                },
                "resizable": {
                    "Type": "resizable_limits_t",
                    "Desc": "table flags and sizes in units of elements"
                }
            }
        },
    }

    memarg_t = [["flags", "memflags_t"], ["offset", "varuptr_t"]]

    BASE_LANG_TYPES = {
        "value_type_t": {
        },
        "reference_type_t": {
            "Alias": "varsint7_t",
            "Enum": [
                {
                    "Type": "type_encoding_t",
                    "Value": "funcref",
                },
                {
                    "Type": "type_encoding_t",
                    "Value": "externref",
                },
            ],
        },
        "vector_value_type_t": {
            "Alias": "varsint7_t",
            "Enum": [
                {
                    "Type": "type_encoding_t",
                    "Value": "v128",
                },
            ],
        },
        "block_signature_type_t": {
            "Fields": {
                "*?": {
                    "true": {
                        "type_value": {
                            "Type": "varsint32_t",
                            "Desc": "type index: negative a type_encoding_t enum value else a type section index",
                        },
                    },
                    "false": {
                        "enc_type": {
                            "Type": "type_encoding_t",
                        },
                    },
                    "false": {
                        "type_index": {
                            "Type": "varuint32_t",
                        }
                    },
                }
            }
        },
    }

    BASE_EXEC_TYPES = {
        'instruction_t': {
            "0x11": [
                ["signature", "varuint32_t"],
                ["tableidx", "varuint32_t"],
                {"stack_delta": None},
            ],
            '0x12': [
                ["callee", "varuint32_t"],
                {"stack_delta": None},
            ],
            '0x1C': [
                ["valtypes", ["value_type_t"]],
                {"stack_delta": -2},
            ],
            '0x25': [
                ["tableidx", "varuint32_t"],
            ],
            '0x26': [
                ["tableidx", "varuint32_t"],
                {"stack_delta": -2},
            ],
            '0xD0': [
                ['reftype', 'reference_type_t'],
                {"stack_delta": +1},
            ],
            '0xD1': [],
            '0xD2': [
                ['func', 'varuint32_t'],
                {"stack_delta": +1},
            ],
            '0xFC': {
                '0x00': [],
                '0x01': [],
                '0x02': [],
                '0x03': [],
                '0x04': [],
                '0x05': [],
                '0x06': [],
                '0x07': [],
                '0x08': [
                    ["x", "varuint32_t"],
                    ["r", "byte_t"],
                    {"stack_delta": -3},
                ],
                '0x09': [
                    ["x", "varuint32_t"],
                ],
                '0x0a': [
                    ["r", "byte_t"],
                    ["s", "byte_t"],
                    {"stack_delta": -3},
                ],
                '0x0b': [
                    ["r", "byte_t"],
                    {"stack_delta": -3},
                ],
                '0x0c': [
                    ["y", "varuint32_t"],
                    ["x", "varuint32_t"],
                    {"stack_delta": -3},
                ],
                '0x0d': [
                    ["x", "varuint32_t"],
                ],
                '0x0e': [
                    ["x", "varuint32_t"],
                    ["y", "varuint32_t"],
                    {"stack_delta": -3},
                ],
                '0x0f': [
                    ["x", "varuint32_t"],
                    {"stack_delta": -1},
                ],
                '0x10': [
                    ["x", "varuint32_t"],
                    {"stack_delta": 1},
                ],
                '0x11': [
                    ["x", "varuint32_t"],
                    {"stack_delta": -3},
                ],
            },
            '0xFD': {
                '00': [*memarg_t],
                '01': [*memarg_t],
                '02': [*memarg_t],
                '03': [*memarg_t],
                '04': [*memarg_t],
                '05': [*memarg_t],
                '06': [*memarg_t],
                '07': [*memarg_t],
                '08': [*memarg_t],
                '09': [*memarg_t],
                '10': [*memarg_t],
                '11': [
                    *memarg_t,
                    {"stack_delta": -2}],
                '12': [
                    ["value", "v128_t"],
                    {"stack_delta": 1},
                ],
                '13': [
                    ["value", "v128_t"],
                    {"stack_delta": -1},
                ],
                '14': [{"stack_delta": -1}],
                '15': [],
                '16': [],
                '17': [],
                '18': [],
                '19': [],
                '20': [],
                '21': [["laneidx", "byte_t"]],
                '22': [["laneidx", "byte_t"]],
                '23': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '24': [["laneidx", "byte_t"]],
                '25': [["laneidx", "byte_t"]],
                '26': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '27': [["laneidx", "byte_t"]],
                '28': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '29': [["laneidx", "byte_t"]],
                '30': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '31': [["laneidx", "byte_t"]],
                '32': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '33': [["laneidx", "byte_t"]],
                '34': [["laneidx", "byte_t"], {"stack_delta": -1}],
                '35': [{"stack_delta": -1}],
                '36': [{"stack_delta": -1}],
                '37': [{"stack_delta": -1}],
                '38': [{"stack_delta": -1}],
                '39': [{"stack_delta": -1}],
                '40': [{"stack_delta": -1}],
                '41': [{"stack_delta": -1}],
                '42': [{"stack_delta": -1}],
                '43': [{"stack_delta": -1}],
                '44': [{"stack_delta": -1}],
                '45': [{"stack_delta": -1}],
                '46': [{"stack_delta": -1}],
                '47': [{"stack_delta": -1}],
                '48': [{"stack_delta": -1}],
                '49': [{"stack_delta": -1}],
                '50': [{"stack_delta": -1}],
                '51': [{"stack_delta": -1}],
                '52': [{"stack_delta": -1}],
                '53': [{"stack_delta": -1}],
                '54': [{"stack_delta": -1}],
                '55': [{"stack_delta": -1}],
                '56': [{"stack_delta": -1}],
                '57': [{"stack_delta": -1}],
                '58': [{"stack_delta": -1}],
                '59': [{"stack_delta": -1}],
                '60': [{"stack_delta": -1}],
                '61': [{"stack_delta": -1}],
                '62': [{"stack_delta": -1}],
                '63': [{"stack_delta": -1}],
                '64': [{"stack_delta": -1}],

                '65': [{"stack_delta": -1}],
                '66': [{"stack_delta": -1}],
                '67': [{"stack_delta": -1}],
                '68': [{"stack_delta": -1}],
                '69': [{"stack_delta": -1}],
                '70': [{"stack_delta": -1}],
                '71': [{"stack_delta": -1}],
                '72': [{"stack_delta": -1}],
                '73': [{"stack_delta": -1}],
                '74': [{"stack_delta": -1}],
                '75': [{"stack_delta": -1}],
                '76': [{"stack_delta": -1}],
                '77': [],
                '78': [{"stack_delta": -1}],
                '79': [{"stack_delta": -1}],
                '80': [{"stack_delta": -1}],
                '81': [{"stack_delta": -1}],
                '82': [{"stack_delta": -2}],
                '83': [],
                '84': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -1}],
                '85': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -1}],
                '86': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -1}],
                '87': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -1}],
                '88': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -2}],
                '89': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -2}],
                '90': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -2}],
                '91': [*memarg_t, ["laneidx", "byte_t"], {"stack_delta": -2}],
                '92': [*memarg_t],
                '93': [*memarg_t],
                '94': [],
                '95': [],
                '96': [],
                '97': [],
                '98': [],
                '99': [],
                '100': [],
                '101': [{"stack_delta": -1}],
                '102': [{"stack_delta": -1}],
                '103': [],
                '104': [],
                '105': [],
                '106': [],
                '107': [{"stack_delta": -1}],
                '108': [{"stack_delta": -1}],
                '109': [{"stack_delta": -1}],
                '110': [{"stack_delta": -1}],
                '111': [{"stack_delta": -1}],
                '112': [{"stack_delta": -1}],
                '113': [{"stack_delta": -1}],
                '114': [{"stack_delta": -1}],
                '115': [{"stack_delta": -1}],
                '116': [],
                '117': [],
                '118': [{"stack_delta": -1}],
                '119': [{"stack_delta": -1}],
                '120': [{"stack_delta": -1}],
                '121': [{"stack_delta": -1}],
                '122': [],
                '123': [{"stack_delta": -1}],
                '124': [],
                '125': [],
                '126': [],
                '127': [],
                '128': [],
                '129': [],
                '130': [{"stack_delta": -1}],
                '131': [],
                '132': [],
                '133': [{"stack_delta": -1}],
                '134': [{"stack_delta": -1}],
                '135': [],
                '136': [],
                '137': [],
                '138': [],
                '139': [{"stack_delta": -1}],
                '140': [{"stack_delta": -1}],
                '141': [{"stack_delta": -1}],
                '142': [{"stack_delta": -1}],
                '143': [{"stack_delta": -1}],
                '144': [{"stack_delta": -1}],
                '145': [{"stack_delta": -1}],
                '146': [{"stack_delta": -1}],
                '147': [{"stack_delta": -1}],
                '148': [],
                '149': [{"stack_delta": -1}],
                '150': [{"stack_delta": -1}],
                '151': [{"stack_delta": -1}],
                '152': [{"stack_delta": -1}],
                '153': [{"stack_delta": -1}],

                '155': [{"stack_delta": -1}],
                '156': [{"stack_delta": -1}],
                '157': [{"stack_delta": -1}],
                '158': [{"stack_delta": -1}],
                '159': [{"stack_delta": -1}],
                '160': [],
                '161': [],
                '163': [],
                '164': [],

                '167': [],
                '168': [],
                '169': [],
                '170': [],
                '171': [{"stack_delta": -1}],
                '172': [{"stack_delta": -1}],
                '173': [{"stack_delta": -1}],
                '174': [{"stack_delta": -1}],

                '177': [{"stack_delta": -1}],

                '181': [{"stack_delta": -1}],
                '182': [{"stack_delta": -1}],
                '183': [{"stack_delta": -1}],
                '184': [{"stack_delta": -1}],
                '185': [{"stack_delta": -1}],
                '186': [{"stack_delta": -1}],

                '188': [{"stack_delta": -1}],
                '189': [{"stack_delta": -1}],
                '190': [{"stack_delta": -1}],
                '191': [{"stack_delta": -1}],
                '192': [],
                '193': [],

                '195': [],
                '196': [],

                '199': [],
                '200': [],
                '201': [],
                '202': [],
                '203': [{"stack_delta": -1}],
                '204': [{"stack_delta": -1}],
                '205': [{"stack_delta": -1}],
                '206': [{"stack_delta": -1}],

                '209': [{"stack_delta": -1}],

                '213': [{"stack_delta": -1}],
                '214': [{"stack_delta": -1}],
                '215': [{"stack_delta": -1}],
                '216': [{"stack_delta": -1}],
                '217': [{"stack_delta": -1}],
                '218': [{"stack_delta": -1}],
                '219': [{"stack_delta": -1}],
                '220': [{"stack_delta": -1}],
                '221': [{"stack_delta": -1}],
                '222': [{"stack_delta": -1}],
                '223': [{"stack_delta": -1}],
                '224': [],
                '225': [],

                '227': [],
                '228': [{"stack_delta": -1}],
                '229': [{"stack_delta": -1}],
                '230': [{"stack_delta": -1}],
                '231': [{"stack_delta": -1}],
                '232': [{"stack_delta": -1}],
                '233': [{"stack_delta": -1}],
                '234': [{"stack_delta": -1}],
                '235': [{"stack_delta": -1}],
                '236': [],
                '237': [],

                '239': [],
                '240': [{"stack_delta": -1}],
                '241': [{"stack_delta": -1}],
                '242': [{"stack_delta": -1}],
                '243': [{"stack_delta": -1}],
                '244': [{"stack_delta": -1}],
                '245': [{"stack_delta": -1}],
                '246': [{"stack_delta": -1}],
                '247': [{"stack_delta": -1}],
                '248': [],
                '249': [],
                '250': [],
                '251': [],
                '252': [],
                '253': [],
                '254': [],
                '255': [],
            }
        }
    }

    BASE_SECTION_TYPES = {
        'DataCountSection': {
            'OpCode': '0x0c',
            'Fields': {
                'size': {
                    "Type": 'varuint32_t',
                    "Desc": "Section size in bytes",
                },
                'seccount': {
                    "Type": 'varuint32_t',
                    "Desc": "The number of data sections",
                }
            }
        },
        'CustomSection': {
            'OpCode': '0x00',
            'Fields': {
                'bytes': {
                    "Type": ["byte_t"],
                    "Desc": "Custom bytes",
                },
            }
        }
    }

    BASE_MODULE_TYPE = {
        'Fields': {
            'DataCountSection': {
                'Type': 'DataCountSection',
                'Count': '?',
            },
        },
    }

    @classmethod
    def concise_name(Self, name):
        assert '_' not in name
        if not name.startswith("array of "):
            return '_'.join(name.split(' ')).replace('-', '_') + '_t' if name[0] == name[0].lower() else ''.join(name.split(' ')).replace('-', '')
        else:
            return f'[{Self.concise_name(name[len("array of "):])}]'

    @classmethod
    def verbose_name(Self, name):
        assert ' ' not in name
        assert not name.lower().startswith('array')
        return name[:-2].replace('_', ' ') if name[0] == name[0].lower() else ' '.join(re.findall(r"([A-Z][a-z]*)", name))

    @classmethod
    def type_clean(Self, name):
        if name == "byte_array_t":
            return ["byte_t"]
        elif isinstance(name, str):
            if name.startswith('['):
                return [name[1:-1]]
            else:
                return name.lower()

        return name

    def __init__(self):
        super().__init__()
        self.section = None
        self.paragraphs = []

        self.prim_types = self.BASE_PRIM_TYPES
        self.enc_types = self.BASE_ENC_TYPES
        self.complex_types = self.BASE_COMPLEX_TYPES
        self.lang_types = self.BASE_LANG_TYPES
        self.exec_types = self.BASE_EXEC_TYPES
        self.section_types = self.BASE_SECTION_TYPES
        self.module_type = self.BASE_MODULE_TYPE
        self.instrs = []
        self.families = {}
        self.new_table()

    def new_table(self):
        self.heading = None
        self.colnames = None
        self.rows = []

    def handle(self, data=None):
        if self.path == ['p'] and data is not None:
            self.paragraphs.append(data)
        if self.path == ['h3'] and data is not None:
            self.section = data
        elif self.path == ['h4'] and data is not None:
            m = re.match(r"^([A-Z])\: (.*)", data)
            if m and 'family' in m.group(2).lower():
                self.families[m.group(1)] = m.group(2)
            else:
                self.heading = data
        elif self.path == ['table', 'thead']:
            self.colnames = None
        elif self.path == ['table', 'thead', 'tr']:
            self.colnames = []
        elif self.path == ['table', 'thead', 'tr', 'th']:
            if data is None:
                self.colnames.append("")
            else:
                self.colnames[-1] += data
        elif self.path == ['table', 'tbody']:
            self.rows = []
        elif self.path == ['table', 'tbody', 'tr']:
            self.rows.append([])
        elif self.path == ['table', 'tbody', 'tr', 'td']:
            if data is None:
                self.rows[-1].append("")
            else:
                self.rows[-1][-1] = (self.rows[-1][-1] + ' ' + data).strip()
        elif self.path in (['ol'], ['ul']):
            self.rows = []
        elif self.path in (['ol', 'li'], ['ul', 'li']):
            if data is not None:
                self.rows.append(data)
        elif self.section == "Encoding Types":
            if self.heading and data is not None:
                lead, *rest = data.split('\n')
                m = re.match(r"^An? ([a-zA-Z0-9 ]+?) (immediate )?is (.*)$", lead)
                if m:
                    name, _, desc = m.groups()

                    if name not in ("array of a given type", "byte array"):
                        name = self.concise_name(name).lower()

                        assert name not in self.enc_types or name in SpecHandler.BASE_ENC_TYPES
                        enc_type = {'Alias': None, 'Desc': ' '.join([desc.rstrip(':. ')] + rest).rstrip(':. ')}

                        aliases = set(enc_type_name for enc_type_name in chain(self.prim_types, self.enc_types, ['byte_array_t']) if ' ' + self.verbose_name(enc_type_name) + ' ' in enc_type['Desc'])

                        for alias in list(aliases):
                            for alias2 in list(aliases):
                                if alias != alias2 and alias2.startswith(alias[:-2]):
                                    aliases.remove(alias)

                        if aliases:
                            enc_type['Alias'] = self.type_clean(aliases.pop()) if len(aliases) == 1 else {"Choice": list(sorted(map(self.type_clean, aliases)))}

                        if name in self.enc_types:
                            self.enc_types[name].update(merge(enc_type, self.enc_types[name]))
                        else:
                            self.enc_types[name] = enc_type
            elif not self.heading and not data and "type encodings" in self.paragraphs[-1].lower():
                for name in self.prim_types:
                    if self.verbose_name(name) in self.paragraphs[-1]:
                        enc_type = list(self.enc_types.values())[0]
                        assert enc_type['Alias'] is None
                        enc_type['Alias'] = name

                        nbits = int(re.search(r"(\d+) bits", self.prim_types[name]["Desc"]).group(1))
                        enc_type["Enum"] = dict(sorted(((k, f"{int(v, 16):#05x}") for k, v in enc_type["Enum"].items()), key=lambda item: int(item[1], 16), reverse=True))

        elif self.section == "Language Types" and data is not None:
            lead, *rest = data.split('\n')
            m = re.match(r"^([-a-zA-Z0-9 ]+? type)s(.*)$", lead)
            if m:
                name, desc = m.groups()

                name = self.concise_name(name.lower())

                if name not in self.lang_types and name != "language_type_t":
                    lang_type = {}

                    self.lang_types[name.lower()] = lang_type

    def handle_starttag_h4(self):
        last_complex_type = list(self.complex_types.values())[-1] if self.complex_types else None

        if last_complex_type and {'kind', 'index'}.issubset(last_complex_type.get('Fields', ())):
            found = []
            for para in self.paragraphs[-10:]:
                m = re.match(r"If (\w+) is (\w+) , (\w+) identifies an element in the ([-\w]+) .", para)
                if m:
                    found.append(m.groups())

            if found:
                index = last_complex_type['Fields'].pop('index')
                description = index.pop('Desc')
                last_complex_type['Fields']['*?'] = {}
                for cond_field, cond_value, index_field, base_table in found:
                    assert cond_field == "kind" and index_field == "index"
                    last_complex_type['Fields']['*?'][f"{cond_field}={cond_value}"] = {index_field: {**index, 'Desc': description.replace('an index space', f"the {base_table} space")}}

        if self.section == "Known Sections" and self.heading and self.heading.endswith("Section"):
            type_name = self.concise_name(self.heading)

            kwargs = {}
            for para in self.paragraphs[-4:]:
                m = re.search(r"Opcode:\s*(0x[0-9a-zA-Z]+)", para)
                if m:
                    kwargs['OpCode'] = m.group(1)

                    self.module_type['Fields'][type_name] = {
                        "Type": type_name,
                        "Count": "?",
                    }
                    break
            else:
                raise ValueError(f"Failed to parse {self.heading!r} section OpCode {self.paragraphs[-4:]!r}")


            kwargs['Fields'] = {
                'size': {
                    "Type": 'varuint32_t',
                    "Desc": "Section size in bytes",
                },
            }
            for line in self.paragraphs[-4:]:
                m = re.match(f"^The {self.heading.split('-')[-1]} consists of an array of ([-\\w ]+)s", line.rstrip(':. '))

                if m:
                    element_type_name = self.concise_name(m.group(1))

                    assert type_name not in self.section_types
                    kwargs['Fields']['*'] = [element_type_name]

                    break

                m = re.match(f"^The {self.heading.split('-')[-1]} consists of a (\\w+) (\\w+) into the ([\\w ]+)", line)

                if m:
                    fieldtype, fieldname, description = m.groups()

                    kwargs['Fields'][fieldname] = {
                        "Type": self.concise_name(fieldtype),
                        "Desc": f"{fieldname} into the {description.strip()}",
                    }
                    break
            else:
                raise ValueError(f"Failed to parse {self.heading!r} section consists of {self.paragraphs[-4:]!r}")

            self.section_types[type_name] = kwargs

    def handle_endtag_h4(self):
        if self.section == "Module Types":
            if self.heading == "Instantiation-Time Initializers":
                assert self.concise_name(self.heading.lower().rstrip('s')) in self.complex_types


    def handle_endtag_ol(self):
        if self.section == "Language Types":
            if len(self.lang_types) == len(self.BASE_LANG_TYPES):
                value_type_t = list(self.lang_types.values())[0]
                value_type_t["Alias"] = self.enc_types["type_encoding_t"]["Alias"]
                clss = [cls for cls in self.rows if cls != "Booleans"] + ["Vector Value Types", "Reference Types"]
                value_type_t["Enum"] = [{"Type": self.concise_name(name.lower().rstrip('s') if 'Value' in name else name.lower().rstrip('s')), "Value": "*"} for name in clss]

    def handle_endtag_ul(self):
        if self.section == 'Known Sections' and self.heading and self.heading.endswith("Section"):
            prelim = self.paragraphs[-1]

            #Complex Type
            if 'consists' in prelim:
                m = re.match(r"^An? (.+) consists of$", prelim.rstrip(':. '))

                if not m: raise ValueError(f"Failed to parse type consists of condition {prelim!r}")

                type_name = self.concise_name(m.group(1))

                fields = {}
                for row in self.rows:
                    if row == r"an index in the Type Section of the signature of the function.":
                        fields['sig_index'] = {
                            "Type": 'varuint32_t',
                            "Desc": "signature index into the Type Section",
                        }
                    else:
                        raise ValueError(f"Unhandled field row {row!r}")

                assert type_name not in self.complex_types
                self.complex_types[type_name] = {"Fields": fields}

    def handle_endtag_table(self):
        if self.section == "Encoding Types" and self.colnames == self.PRIM_TYPES:
            for row in self.rows:
                if sum(map(len, row)) == 0:
                    continue

                assert len(self.colnames) == len(row)
                prim_type = dict(zip(self.colnames, row))
                prim_type['Desc'] = prim_type.pop('Description')
                name = self.concise_name(prim_type.pop('Name'))
                assert name not in self.prim_types
                self.prim_types[name] = prim_type
        elif self.section == "Encoding Types":
            details = {}
            for row in self.rows:
                if sum(map(len, row)) == 0:
                    continue

                assert self.colnames[0] == 'Name'
                assert len(self.colnames) == len(row) == 2
                assert row[0] not in details
                details[row[0]] = row[1]

            last = list(self.enc_types.values())[-1 if len(self.enc_types) < 4 else 0]

            details_type = 'BitFields' if any(fieldname.startswith('$') for fieldname in details) else 'Enum'
            if details_type == 'BitFields':
                assert len(details) == 1 and all(fieldname.startswith('$') for fieldname in details)
                details = {fieldname[1:]: {
                    'Range': (0, int(re.search(r"limited to (\d+) bits", self.prim_types[last['Alias']]['Desc']).group(1))),
                    'Desc': description,
                } for fieldname, description in details.items()}

            last[details_type] = {**details, **last.get(details_type, {})}
        elif self.section == "Language Types":
            details = []

            para = self.paragraphs[-1]
            m = re.search(r"include( the)? ([-0-9a-zA-Z ]+ type)s", para)
            if m:
                details.append({"Type": self.concise_name(m.group(2)), "Value": "*"})

            for row in self.rows:
                details.append({"Type": "type_encoding_t", "Value": row[0]})


            if list(self.lang_types.keys())[-1] not in self.OVERRIDE and self.heading != "Block Signature Types":
                last_type_name, last = list(self.lang_types.items())[-1]

                last["Alias"] = self.enc_types["type_encoding_t"]["Alias"]

                details_type = "Enum"
                if last_type_name in self.REMOVE:
                    self.lang_types.pop(last_type_name)
                else:
                    assert details_type not in last
                    last[details_type] = details
        elif self.section in ('Known Sections', 'Module Contents') and self.colnames == self.FIELD:
            fields = {}
            for row in self.rows:
                assert len(self.colnames) == len(row)
                field = dict(zip(self.colnames, row))
                field['Desc'] = field.pop('Description').rstrip(':. ')
                field['Type'] = self.type_clean(self.concise_name(field['Type']))
                if 'sequence' in field['Type']:
                    field['Type'] = re.match(r"sequence_of_(\w+)s_t", field['Type']).group(1) + '_t'
                    field['Count'] = '*'
                fieldname = field.pop('Field Name')
                fields[fieldname] = field

            preprelim = self.paragraphs[-2]
            prelim = self.paragraphs[-1]
            kwargs = {}

            #Module
            if self.section.startswith('Module'):
                assert self.heading is None

                self.module_type.update({"Fields": {**fields, **self.module_type['Fields']}})

            #Section Types + Complex Element Types
            elif self.heading and self.heading.endswith("Section"):
                is_module_type, is_section_type, type_name = False, True, self.concise_name(self.heading)

                for para in self.paragraphs[-6:]:
                    m = re.search(r"Opcode:\s*(0x[0-9a-zA-Z]+)", para)
                    if m:
                        kwargs['OpCode'] = m.group(1)

                        self.module_type['Fields'][type_name] = {
                            "Type": type_name,
                            "Count": "?",
                        }

                #Complex Element Type
                m = re.match(f"^The {self.heading} consists of an array of (.+)s$", preprelim.rstrip(':. '))

                if not m: raise ValueError(f"Failed to parse section consists of {preprelim!r}")

                element_type_name = m.group(1).replace('bodie', 'body')
                assert prelim.endswith(f"{element_type_name} consists of:")
                element_type_name = self.concise_name(element_type_name)

                kwargs['Fields'] = {
                    'size': {
                        "Type": 'varuint32_t',
                        "Desc": "Section size in bytes",
                    },
                    '*': [element_type_name],
                }

                assert type_name not in self.section_types
                self.section_types[type_name] = kwargs

                if element_type_name in self.OVERRIDE:
                    self.complex_types[element_type_name] = self.complex_types.pop(element_type_name)
                else:
                    assert element_type_name not in self.complex_types
                    self.complex_types[element_type_name] = {"Fields": fields}

            #Complex Type
            elif 'consists' in prelim:
                m = re.match(r"^An? (.+) consists of$", prelim.rstrip(':. '))

                if not m: raise ValueError(f"Failed to parse type consists of condition {prelim!r}")

                type_name = self.concise_name(m.group(1))

                assert type_name not in self.complex_types
                self.complex_types[type_name] = {"Fields": fields}

            #Additional table
            else:
                last_type_name, last_type = list(self.complex_types.items())[-1]

                m = re.match(r"^If (.*?)(\w+) is (.+?) .* appended.$", prelim)
                if not m: raise ValueError(f"Failed to parse condition for complex type {last_type_name!r} [{', '.join(last_type)}] ({condition!r})")

                prefix, fieldname, value = m.groups()

                if fieldname in last_type['Fields']:
                    pass
                elif fieldname == "element_type" and 'table' in prefix:
                    assert last_type_name in self.OVERRIDE
                else:
                    raise ValueError(f"Failed to determine field of condition for complex type {last_type_name!r} [{', '.join(last_type)}] ({condition!r})")

                if last_type_name not in self.OVERRIDE:
                    assert len(kwargs) == 0
                    last_type['Fields'].setdefault('*?', {})
                    last_type['Fields']['*?'][f"{fieldname}={value}"] = fields

        elif self.section == "Module Types":
            if self.heading:
                type_name = self.concise_name(re.sub(r"\W", " ", self.heading.rstrip('s' if 'Limits' not in self.heading else '')).lower())

                fields = {}
                is_enum = False
                for row in self.rows:
                    assert len(self.colnames) == len(row)
                    field = dict(zip(self.colnames, row))

                    if {'Field Name', 'Type', 'Description'}.issubset(self.colnames):
                        field['Desc'] = field.pop('Description').rstrip(':. ')
                        field['Type'] = self.type_clean(self.concise_name(field['Type']))
                        fieldname = field.pop('Field Name')
                        fields[fieldname] = field
                    elif {'Name', 'Binary Encoding'}.issubset(self.colnames):
                        is_enum = True
                        fields[field['Name']] = field['Binary Encoding']
                    else:
                        raise ValueError(F"Unexpected fields {self.colnames}")

                if type_name not in self.OVERRIDE:
                    assert type_name not in self.complex_types

                    if is_enum:
                        description = self.paragraphs[-1]
                        aliases = set(enc_type_name for enc_type_name in chain(self.prim_types, self.enc_types) if ' ' + self.verbose_name(enc_type_name) + ' ' in description)
                        assert len(aliases) == 1
                        self.complex_types[type_name] = {
                            'Alias': aliases.pop(),
                            'Desc': description,
                            'Enum': fields,
                        }
                    else:
                        self.complex_types[type_name] = {"Fields": fields}

            #Additional table
            else:
                prelim = self.paragraphs[-1]
                last_type_name, last_type = list(self.complex_types.items())[-1]

                fields = {}
                for row in self.rows:
                    assert len(self.colnames) == len(row)
                    field = dict(zip(self.colnames, row))

                    if {'Field Name', 'Type', 'Description'}.issubset(self.colnames):
                        field['Desc'] = field.pop('Description').rstrip(':. ')
                        field['Type'] = self.type_clean(self.concise_name(field['Type']))
                        fieldname = field.pop('Field Name')
                        fields[fieldname] = field
                    else:
                        raise ValueError(F"Unexpected fields {self.colnames}")

                m = re.match(r"^If bit (\w+) is set in (\w+) , the following fields are appended.$", prelim)
                if not m: raise ValueError(f"Failed to parse condition for complex type {last_type_name!r} [{', '.join(last_type)}] ({condition!r})")

                bit, fieldname = m.groups()

                common_fields = {common_field: last_type['Fields'].pop(common_field) for common_field in list(last_type['Fields'])[list(last_type['Fields']).index(fieldname)+1:]}

                last_type['Fields'].setdefault('*?', {})
                last_type['Fields']['*?'][f"{fieldname}={0 << (int(bit, 16)-1):#04x}"] = {**common_fields}
                last_type['Fields']['*?'][f"{fieldname}={1 << (int(bit, 16)-1):#04x}"] = {**common_fields, **fields}

        elif self.colnames[0] == self.OPCODE[0]:
            assert self.section.endswith('Instructions')

            for row in self.rows:
                if sum(map(len, row)) == 0:
                    continue

                assert len(self.colnames) == len(row)
                instr = {
                    'Immediates': "",
                    'Name': self.heading,
                }
                instr.update(dict(zip(self.colnames, row)))
                instr['Depth'] = self.DEPTHS.get(instr['Mnemonic'], '')
                assert set(instr) == set(self.OPCODE)
                self.instrs.append(enrich_instr(instr))

                instr_type = self.exec_types['instruction_t']
                if instr['Opcode'] in self.OVERRIDE_INSTRS:
                    continue

                assert instr['Opcode'] not in instr_type

                imms = []
                for name, typ in map(lambda imm: tuple(map(str.strip, imm.split(':'))), filter(lambda x: len(x) > 0, map(str.strip, instr['Immediates'].split(',')))):
                    assert name.startswith('$')
                    imms.append((name[1:], self.concise_name(typ.lower())))

                instr_type[instr['Opcode']] = imms

                if instr['Depth'] != '':
                    instr_type[instr['Opcode']].append({'depth': int(instr['Depth']) if instr['Depth'] != '-1/+1' else None})

                def list_types(l):
                    assert l[0] + l[-1] == "()"
                    args = l[1:-1].split(', ')
                    assert all(',' not in arg for arg in args)
                    return tuple(filter(lambda arg: len(arg) > 0, (arg.split(': ')[-1] for arg in args)))

                def args_count(args):
                    nargs = 0
                    for arg in args:
                        if arg in ('i32', 'i64', 'f32', 'f64', 'iPTR'):
                            nargs += 1
                        elif re.match(r"\$T\[\s?([0-9]+)\s?\]", arg):
                            nargs += int(re.match(r"\$T\[\s?([0-9]+)\s?\]", arg).group(1))
                        elif re.match(r"\$T\[\s?[$a-z_]+\s?\]", arg):
                            nargs += float('inf')
                        else:
                            raise ValueError(f"Bad value stack arg type {arg!r}")
                    return nargs

                stack_args, stack_rets = tuple(map(list_types, instr['Signature'].split(' : ')))
                stack_delta = args_count(stack_rets) - args_count(stack_args)
                stack_delta = stack_delta if isinstance(stack_delta, int) else None

                if stack_delta is None:
                    debug(instr['Opcode'], instr['Mnemonic'], instr['Signature'], stack_delta)

                if stack_delta != 0:
                    if len(instr_type[instr['Opcode']]) == 0 or not isinstance(instr_type[instr['Opcode']][-1], dict):
                        instr_type[instr['Opcode']].append({})

                    instr_type[instr['Opcode']][-1]['stack_delta'] = stack_delta
        else:
            raise NameError(f"Unsupported section table {self.section}")

        self.new_table()


def enrich_instr(instr):
    assert instr['Opcode'].startswith('0x')
    instr['Opcode'] = f"0x{instr['Opcode'][2:].upper()}"
    return instr


def get_spec():
    if not FILE.exists():
        with open(FILE, "w") as f:
            f.write(download())

    with open(FILE, "r") as f:
        return f.read()

def dumps(x, indent=4, depth=0):
    DELIM = ',\n'

    if isinstance(x, (type(None), int, str, float)):
        return json.dumps(x)
    elif isinstance(x, dict):
        if all(isinstance(v, (int, type(None))) for v in x.values()) and len(x) == 1:
            return f"{{{', '.join(json.dumps(k) + ': ' + json.dumps(v) for k, v in x.items())}}}"

        prefix = ' ' * indent * (depth+1)

        lines = []
        for k, v in x.items():
            lines.append(f"{' ' * indent * (depth+1)}{json.dumps(k)}: {dumps(v, indent=indent, depth=depth+1)}")

        return f"{{\n{DELIM.join(lines)}\n{' ' * indent * depth}}}"
    elif isinstance(x, (tuple, list)):
        if all(not isinstance(v, (list, dict)) for v in x):
            return f"[{', '.join(json.dumps(v) for v in x)}]"

        lines = []
        for v in x:
            lines.append(f"{' ' * indent * (depth+1)}{dumps(v, indent=indent, depth=depth+1)}")

        return f"[\n{DELIM.join(lines)}\n{' ' * indent * depth}]"
    else:
        raise NotImplementedError(f"Dumps {type(x)}")





if __name__ == "__main__":
    handler = SpecHandler()
    handler.feed(get_spec())
    handler.close()

    #put newer data count section before code and data
    handler.lang_types = dict(list(handler.lang_types.items())[1:] + list(handler.lang_types.items())[:1])
    handler.module_type['Fields'] = dict(sorted(handler.module_type['Fields'].items(), key=lambda item: (int(handler.section_types[item[0]]['OpCode'], 16) - 0x0c) or -2.5 if item[0].endswith('Section') else -999))
    handler.instrs.sort(key=lambda instr: int(instr['Opcode'][2:], 16))
    handler.exec_types['instruction_t'] = dict(sorted(handler.exec_types['instruction_t'].items()))

    if False:
        attrnames = SpecHandler.OPCODE

        with open(f"instructions.html", "w") as f:
            f.write("<html>")
            f.write("<head>")
            f.write(f"<style>{STYLE}</style>")
            f.write("</head>")
            f.write("<body>")
            f.write(f'<h2 style="text-align: center">Instructions</h2>')

            f.write("<table>")
            f.write(f"<thead><tr><th>{'</th><th>'.join(attrnames)}</th></tr></thead>")
            for instr in handler.instrs:
                f.write(f"<tr><td>{'</td><td>'.join(instr[name] for name in attrnames)}</td></tr>")
            f.write("</table>")

            f.write("<br/>")

            f.write('<table style="margin-right: 200px">')
            for code, name in handler.families.items():
                f.write(f"<tr><td>{code}</td><td>{name}</td></tr>")
            f.write("</table>")

            f.write("</body>")
            f.write("</html>")

    assert [(name, handler.section_types[field["Type"]]["OpCode"]) for name, field in handler.module_type["Fields"].items() if field["Type"].endswith("Section")] == [
        ("TypeSection", "0x01"),
        ("ImportSection", "0x02"),
        ("FunctionSection", "0x03"),
        ("TableSection", "0x04"),
        ("LinearMemorySection", "0x05"),
        ("GlobalSection", "0x06"),
        ("ExportSection", "0x07"),
        ("StartSection", "0x08"),
        ("ElementSection", "0x09"),
        ("DataCountSection", "0x0c"),
        ("CodeSection", "0x0a"),
        ("DataSection", "0x0b"),
    ]


    print(dumps({
            "PrimitiveTypes": handler.prim_types,
            "EncodingTypes": handler.enc_types,
            "ComplexTypes": handler.complex_types,
            "LanguageTypes": handler.lang_types,
            "ExecutionTypes": handler.exec_types,
            "SectionTypes": handler.section_types,
            "ModuleTypes": {
                "Module": handler.module_type,
            }
        }, indent=4))
