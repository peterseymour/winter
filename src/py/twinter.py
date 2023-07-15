import re
import sys
from pathlib import Path
import subprocess, time, os
from float import Float, FiniteClass, InfinityClass, NaNClass


class Reader:
    EOF = '\x00'

    def __init__(self, content):
        self.content = content
        self.i = 0

    def peek(self, n=0):
        if isinstance(n, int):
            return self.content[self.i+n] if 0 <= self.i+n < len(self.content) else self.EOF
        else:
            cs = []
            i = 0
            while n.match(self.peek(i)):
                cs.append(self.peek(i))
                i += 1
            return ''.join(cs)

    def read(self, rx=None):
        if isinstance(rx, int) and rx != 1:
            return ''.join(self.read() for _ in range(rx))
        elif rx is not None:
            cs = []
            while rx.match(self.peek()):
                cs.append(self.read())
            return ''.join(cs)
        else:
            c = self.peek()
            self.i += 1
            return c


class Sym(str):
    pass


class Lit(str):
    pass


class RawBytes(bytes):
    pass


SYM = re.compile('[a-zA-Z0-9_.>]')
SEP = re.compile('[=]')
LAB = re.compile('[a-zA-Z0-9-._]')
INT = re.compile('[0-9-+]')
DEC = re.compile('[.0-9-+]')
HEX = re.compile('[0-9a-fA-F_]')


lineno = 1

def tokens(rdr):
    global lineno
    lineno = 1

    while True:
        while rdr.peek() in " \t\n":
            if rdr.read() == '\n': lineno += 1

        if rdr.peek() == Reader.EOF:
            break
        elif rdr.peek() + rdr.peek(1) == '(;':
            depth = 0
            while True:
                cc = rdr.peek() + rdr.peek(1)
                if cc == '(;':
                    depth += 1
                    assert rdr.read() + rdr.read() == '(;'
                elif cc == ';)':
                    depth -= 1
                    assert rdr.read() + rdr.read() == ';)'
                elif rdr.read() == '\n':
                    lineno += 1

                if depth == 0:
                    break
                assert depth > 0
        elif rdr.peek() + rdr.peek(1) == ';;':
            while rdr.peek() != '\n':
                rdr.read()
        elif rdr.peek() in "()":
            yield Sym(rdr.read())
        elif DEC.match(rdr.peek()):
            if rdr.peek() + rdr.peek(1) == '0x' or rdr.peek() + rdr.peek(1) + rdr.peek(2) in ('+0x', '-0x'):
                if rdr.peek() in ('+', '-'):
                    sign = rdr.read()
                else:
                    sign = '+'
                assert rdr.read() + rdr.read() == '0x'

                hexint = sign + '0x' + rdr.read(HEX).replace('_', '')
                hexfrac = None
                decexp = None

                if rdr.peek() == '.':
                    rdr.read()

                if rdr.peek(HEX):
                    hexfrac = rdr.read(HEX).replace('_', '')

                if rdr.peek().lower() == 'p':
                    assert rdr.read().lower() == 'p'
                    decexp = rdr.read(DEC)

                hexval = hexint
                if hexfrac is not None:
                    hexval += '.' + hexfrac

                if decexp is not None:
                    hexval += 'p' + decexp

                if hexval != hexint:
                    num = Float.parse(hexval)
                else:
                    num = int(hexint, 16)
            else:
                num = rdr.read(DEC)

                if num in ('-', '+') and (rdr.peek() + rdr.peek(1) + rdr.peek(2)) in ('inf', 'nan'):
                    sign = num
                    rep = rdr.read(3)

                    if rep == "nan" and rdr.peek() == ':':
                        rdr.read()
                        if (rdr.peek() + rdr.peek(1)) in ('0x', '0X'):
                            rdr.read(2)
                            payload = rdr.read(HEX)

                            num = Float.parse(f"{sign}{rep}:0x{payload}")
                        else:
                            name = rdr.read(SYM)

                            yield Float.parse(f"{sign}{rep}:{name}")
                    else:
                        num = Float.parse(f"{sign}{rep}")
                else:
                    if rdr.peek() in "eE":
                        num += rdr.read()
                        num += rdr.read(INT)

                    try:
                        num = int(num, 10)
                    except ValueError:
                        num = Float.parse(num)

            yield num
        elif ''.join(rdr.peek(i) for i in range(3)) in ('nan', 'inf'):
            rep = rdr.read(3)
            if rep == "nan" and rdr.peek() == ':':
                sign = '+'
                rdr.read()
                if (rdr.peek() + rdr.peek(1)) in ('0x', '0X'):
                    rdr.read(2)
                    payload = rdr.read(HEX)

                    yield Float.parse(f"{sign}{rep}:0x{payload}")
                else:
                    name = rdr.read(SYM)

                    yield Float.parse(f"{rep}:{name}")
            else:
                yield Float.parse(rep)
        elif rdr.peek() == '$':
            yield Sym(rdr.read() + rdr.read(LAB))
        elif rdr.peek() == '"':
            byts = []

            assert rdr.read() == '"'

            while rdr.peek() not in ('"', Reader.EOF):
                c = rdr.read()
                if c == '\n':
                    lineno += 1
                    byts.append(c.encode('utf-8'))
                elif c == '\\':
                    if HEX.match(rdr.peek()) and HEX.match(rdr.peek(1)):
                        byts.append(int(rdr.read(2), 16).to_bytes(1, byteorder='big'))
                    elif rdr.peek() in "\"\\":
                        byts.append({
                            '"':  '"',
                            '\\': '\\',
                        }[rdr.read()].encode('utf-8'))
                    else:
                        raise ValueError(f"line {lineno}: Unrecognized literal escape '{c}{rdr.peek()}'")
                else:
                    byts.append(c.encode('utf-8'))

            assert rdr.read() == '"'

            byts = b''.join(byts)

            try:
                yield Lit(byts.decode('utf-8'))
            except UnicodeDecodeError:
                yield RawBytes(byts)
        elif SYM.match(rdr.peek()):
            yield Sym(rdr.read(SYM))
        elif SEP.match(rdr.peek()):
            yield Sym(rdr.read(SEP))
        else:
            raise ValueError(f"Line {lineno}: Unexpected character {rdr.peek()!r}")

    yield Reader.EOF


def parse(toks, tok=None):
    if tok is None:
        tok = next(toks)

    if isinstance(tok, Sym) and tok == '(':
        parts = []
        while True:
            tok = next(toks)
            if isinstance(tok, Sym) and tok == ')':
                break
            else:
                parts.append(parse(toks, tok=tok))

        assert isinstance(tok, Sym) and tok == ')'
        return tuple(parts)
    else:
        return tok


def valof(expr):
    if isinstance(expr, str):
        return str(expr)

    typ, val = expr

    if typ in ('i32.const', 'i64.const'):
        assert isinstance(val, int)
        return val
    elif typ in ('f32.const', 'f64.const'):
        assert isinstance(val, (Float.Literal, int))

        #extend the payload so the width of the resulting float is known when converting to a string argument
        if isinstance(val, Float.Literal) and 'nan:0x' in str(val):
            return Float.parse(f"{val.sign}nan:0x{'0' * ({'f32.const': 8, 'f64.const': 16}[typ] - len(val.payload))}{val.payload}")

        return val if isinstance(val, Float.Literal) else Float.parse(str(val))
    elif typ == "ref.extern":
        assert isinstance(val, int)
        return val
    elif expr == ('ref.null', 'extern'):
        return "null"
    elif expr == ('ref.null', 'func'):
        return "null"
    else:
        raise NotImplementedError(f"valof({expr!r})")


def with_type(typed_value):
    typ, value = typed_value

    if isinstance(value, Float.Literal):
        assert typ in ('f32', 'f64')
        return value.with_type(typ)

    return value


def typeof(expr):
    if isinstance(expr, tuple):
        typ, *_ = expr
        if typ.endswith(".const"):
            return typ.split('.')[0]
        elif typ == "ref.extern" or expr == ('ref.null', 'extern'):
            return "externref"
        elif typ == "ref.func" or expr == ('ref.null', 'func'):
            return "funcref"
        else:
            raise NotImplementedError(f"typeof({expr!r})")
    elif isinstance(expr, str):
        return "errmsg"
    else:
        assert False


def pretty(val, typ):
    if typ == 'errmsg':
        return repr(val)
    else:
        return f"{val}:{typ}"


def dispof(arg):
    return pretty(valof(arg), typeof(arg))


class GenericCommand:
    OPT = []

    def opts(self):
        return self.OPT

    def process_lines(self, std_lines, err_lines, nresults):
        if self.is_error:
            print("-----")
            for line in std_lines:
                print(line)
            print("----")
            for line in err_lines:
                print(line)
            print("----")
            print("Invoke failed")
            exit(1)
        elif nresults is not None:
            results = std_lines[len(std_lines) - nresults:]
        else:
            results = std_lines

        def parse(line):
            line = line.strip()

            #unlike WASM this is full byte representation of a NaN not just its sign and payload
            if line.startswith('nan:0x'):
                hexval = line.split(':0x')[-1]
                typ = {4: 'f32', 8: 'f64'}[len(hexval) // 2]
                return Float.parse(f"{typ}:0x{hexval}")
            elif line.startswith('externref:null'):
                return "null"
            elif line.startswith('externref:'):
                return int(line.split(':')[-1], 16)
            elif line.startswith('funcref:null'):
                return "null"
            elif line.startswith('externref:'):
                raise NotImplementedError

            try:
                return int(line, 16 if line.startswith('0x') or line.startswith('-0x') else 10)
            except ValueError:
                pass

            try:
                return Float.parse(line)
            except ValueError:
                pass

            return "** Unparsable result **"

        return tuple(map(parse, results)), std_lines, err_lines

    def process_errors(self, std_lines, err_lines, nresults):
        if nresults is not None:
            results = err_lines[len(err_lines) - nresults:]
        else:
            results = err_lines

        def parse(line):
            if line.startswith("TRAP: "):
                return line.split(': ', 1)[-1]

            return line

        return tuple(map(parse, results)), std_lines, err_lines


class POpenCommand(GenericCommand):
    def __init__(self):
        self.proc = None

    def send(self, command):
        self.proc.stdin.write(command.encode('utf-8') + b'\n')

        try:
            self.proc.stdin.flush()
        except BrokenPipeError:
            pass

    def recv(self):
        std_parts, err_parts = [], []

        while True:
            time.sleep(0)
            std_part = self.proc.stdout.readline()
            err_part = self.proc.stderr.readline()

            try:
                std_part = std_part.decode('utf-8')
            except UnicodeDecodeError:
                std_part = repr(std_part)

            try:
                err_part = err_part.decode('utf-8')
            except UnicodeDecodeError:
                err_part = repr(err_part)

            if err_part != '':
                err_parts.append(err_part)

            if std_part.strip() == '>' or err_part.startswith('ERROR:') or err_part.startswith('TRAP:'):
                if std_part.strip() != '':
                    std_parts.append(std_part)

                time.sleep(0.001)
                while True:
                    time.sleep(0)
                    err_part = self.proc.stderr.readline().decode('utf-8')
                    if err_part == '':
                        break

                    err_parts.append(err_part)

                while True:
                    time.sleep(0)
                    std_part = self.proc.stdout.readline().decode('utf-8')
                    if std_part == '':
                        break
                    std_parts.append(std_part)
                break
            elif std_part.strip() != '':
                std_parts.append(std_part)

        std_output = ''.join(std_parts).rstrip()
        std_lines = std_output.split('\n') if len(std_output) > 0 else []

        err_output = ''.join(err_parts).rstrip()
        err_lines = err_output.split('\n') if len(err_output) > 0 else []

        self.is_error = len(err_lines) > 0
        self.is_ready = len(std_lines) > 0 and std_lines[-1].strip() == '>'

        return (std_lines[:-1] if self.is_ready else std_lines), err_lines

    def start(self):
        assert self.proc is None

        self.proc = subprocess.Popen(
            [self.bin, *self.opts()],
            stdin = subprocess.PIPE,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE)

        try:
            os.set_blocking(self.proc.stdout.fileno(), False)
            os.set_blocking(self.proc.stderr.fileno(), False)
        except:
            self.proc.kill()
            raise

        self.check("start ", *self.recv())

    def stop(self):
        if self.proc:
            self.proc.kill()
            self.proc = None

    def restart(self):
        self.stop()
        self.start()

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, *args):
        self.stop()

    def check(self, action, std_lines, err_lines, ret_errors=False):
        if len(err_lines) > 0:
            if ret_errors:
                return err_lines

            for line in std_lines:
                print(line)
            for line in err_lines:
                print(line)
            print(f"Failed to {action}")
            exit(1)

    def invoke(self, namespace, funcname, args, expect_error=False, nresults=None):
        self.send(' '.join((self.scoped_arg(namespace, funcname), *map(str, args))))
        std_lines, err_lines = self.recv()

        if self.is_error and expect_error:
            return self.process_errors(std_lines, err_lines, nresults)
        else:
            return self.process_lines(std_lines, err_lines, nresults)


class Winter(POpenCommand):
    OPT = ['--repl']

    ESCAPES = {
        '\\'  : '\\\\',
        '"'   : '\\"',
        '\t'  : '\\t',
        '\n'  : '\\n',
        '\r'  : '\\r',
    }

    @classmethod
    def escape(Self, s):
        return '"' + ''.join(Self.ESCAPES[c] if c in Self.ESCAPES else c if ord(c) >= 32 else '\\x%02x' % ord(c) for c in s) + '"'

    @classmethod
    def scoped_arg(Self, namespace, name):
        return f"{Self.escape(namespace)}:{Self.escape(name)}" if namespace else Self.escape(name)

    def __init__(self, winter, trace=False, instantiate=()):
        super().__init__()
        self.bin = winter
        self.trace = trace
        self.instantiate = dict([item.split(':') for item in ([instantiate] if isinstance(instantiate, str) else instantiate)])

    def opts(self):
        return self.OPT + (['--trace'] if self.trace else []) + [f'--instantiate={name}:{path}' for name, path in {**self.instantiate}.items()]

    def load(self, wasm_path, ret_errors=False):
        self.send(f"instantiate {wasm_path}")
        return self.check(f"instantiate {wasm_path}", *self.recv(), ret_errors=ret_errors)

    def register(self, module_name, module_path=None):
        if module_path:
            self.send(f"instantiate {self.scoped_arg(module_name, module_path)}")
            self.check(f"instantiate {self.scoped_arg(module_name, module_pat)}", *self.recv())
        else:
            self.send(f"register {self.escape(module_name)}")
            self.check(f"register {self.escape(module_name)}", *self.recv())

    def get(self, namespace, global_name):
        self.send(f"get {self.scoped_arg(namespace, global_name)}")
        std_lines, err_lines = self.recv()

        return self.process_lines(std_lines, err_lines, 1)


def unsigned(v, base):
    return v + base if v < 0 else v


def equiv(args, ignore_nan_sign=True):
    typ, lhs, rhs = args

    cls = type(lhs)

    if typ == 'errmsg':
        assert cls is str
        return type(rhs) is cls and lhs.startswith(rhs)

    elif typ == 'i32':
        assert cls is int
        return type(rhs) is cls and unsigned(lhs, 2**32) == unsigned(rhs, 2**32)

    elif typ == 'i64':
        assert cls is int
        return type(rhs) is cls and unsigned(lhs, 2**64) == unsigned(rhs, 2**64)

    elif typ in ('f32', 'f64'):
        assert type(rhs) is cls and issubclass(cls, Float) and lhs.TYPE == rhs.TYPE == typ

        if issubclass(cls, (InfinityClass, FiniteClass)):
            return lhs == rhs
        elif issubclass(cls, NaNClass):
            return (ignore_nan_sign or lhs.sign == rhs.sign) and (lhs.is_canonical() == rhs.is_canonical())
        else:
            raise NotImplementedError

    elif typ == "externref":
        assert cls is int or lhs == "null"
        return type(rhs) is cls and lhs == rhs

    elif typ == "funcref":
        assert cls is int or lhs == "null"
        return type(rhs) is cls and lhs == rhs

    else:
        raise NotImplementedError


def has_passed(results, targets):
    if len(results) != len(targets):
        return False

    types, targets = tuple(map(typeof, targets)), tuple(map(valof, targets))

    targets = tuple(map(with_type, zip(types, targets)))
    results = tuple(map(with_type, zip(types, results)))

    if len(targets) > 0 and isinstance(targets[0], str) and isinstance(results[0], str):
        target = targets[0]
        result = results[0]

        if target == 'out of bounds memory access' and result.startswith('ERROR: Segmentation fault'):
            return True
        elif target.startswith(result) and len(target) - len(result) < 3 and len(result) > 16:
            return True

    return all(map(equiv, zip(types, results, targets)))


def test_file(wast_path, Invoker=Winter, kwargs={}):
    global lineno

    with Invoker(**kwargs) as invoker:
        print(f"Testing {Path(invoker.bin).name} on {wast_path}")

        curr_module_label = None

        nmodules = 0
        module_paths = {}
        registered = {}

        def compile_module(text):
            nonlocal nmodules
            output = wast_path.parent / "output"
            output.mkdir(parents=True, exist_ok=True)

            wasm_path = output / wast_path.with_suffix(".wasm" if nmodules == 0 else f".{nmodules}.wasm").name
            wat_path = output / wast_path.with_suffix(".wat" if nmodules == 0 else f".{nmodules}.wat").name

            if not wat_path.exists():
                with open(wat_path, "w") as f:
                    f.write(text)

            if not wasm_path.exists():
                proc = subprocess.run(["wat2wasm", str(wat_path), '-o', str(wasm_path)], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

                compiled = proc.returncode == 0

                if compiled:
                    print(f"Compiled {wat_path} -> {wasm_path}")
                else:
                    print(proc.stderr.decode('utf-8').split('\n'))
                    exit()
            else:
                compiled = True

            nmodules += 1

            return compiled, wasm_path

        def display(name):
            return invoker.escape(name) if invoker.escape(name)[1:-1] != name else name

        def display_scoped(namespace, name):
            return f"{display(namespace)}:{display(name)}" if namespace else display(name)

        def register_module(module_label, module_name, module_path=None):
            assert module_label not in registered or registered[module_label] == module_label[1:]
            assert module_label.startswith('$')
            assert not module_name.startswith('$')

            registered[module_label] = module_name

            print(f"Registering {display(module_name)} {display(module_path if module_path else '__main__')}")
            invoker.register(module_name, module_path)

        with open(wast_path, "r") as f:
            rdr = Reader(f.read())
            toks = tokens(rdr)

            while True:
                b = rdr.i
                expr = parse(toks)

                if expr == Reader.EOF:
                    break

                if invoker.is_error and not invoker.is_ready:
                    assert False
                    invoker.restart()

                if expr[0] in 'module':
                    curr_module_label = expr[1] if len(expr) > 1 and isinstance(expr[1], Sym) and expr[1] not in ("binary",) else None
                    assert curr_module_label is None or curr_module_label.startswith('$')
                    compiled, wasm_path = compile_module(text=rdr.content[b:rdr.i])

                    if not compiled:
                        print(proc.stderr)
                        print("Compilation failed")
                        exit(1)

                    if curr_module_label is not None:
                        assert curr_module_label not in module_paths
                        module_paths[curr_module_label] = wasm_path

                    invoker.load(wasm_path)

                    if curr_module_label is not None:
                        register_module(curr_module_label, curr_module_label[1:])

                elif expr[0] == 'register':
                    if len(expr) == 3:
                        _, module_name, module_label = expr
                    else:
                        _, module_name = expr
                        module_label = f"$dummy{len(registered)}"

                    if module_name != module_label[1:]:
                        register_module(module_label, module_name)

                elif expr[0] == 'invoke':
                    command, funcname, *args = expr

                    module_label, funcname, *args = (funcname, *args) if isinstance(funcname, Sym) else (curr_module_label, funcname, *args)

                    namespace = registered.get(module_label)

                    disp_args = '(' + ', '.join(map(dispof, args)) + ')' if len(args) != 1 else dispof(args[0])
                    args = tuple(map(valof, args))

                    print(f"{command} {display_scoped(namespace, funcname)}{disp_args}")

                    invoker.invoke(namespace, funcname, args, nresults=0)
                elif expr[0] in ('assert_return', 'assert_exhaustion', 'assert_trap', 'assert_unlinkable'):
                    assertion, subexpr, *targets = expr

                    if subexpr[0] == 'invoke':
                        _, funcname, *args = subexpr

                        module_label, funcname, *args = (funcname, *args) if isinstance(funcname, Sym) else (curr_module_label, funcname, *args)

                        namespace = registered.get(module_label)

                        disp_args = '(' + ', '.join(map(dispof, args)) + ')' if len(args) != 1 else dispof(args[0])
                        disp_targets = '(' + ', '.join(map(dispof, targets)) + ')' if len(targets) != 1 else dispof(targets[0])

                        if len(targets) > 0:
                            print(f"{assertion} {display_scoped(namespace, funcname)}{disp_args if len(args) != 1 else '(' + disp_args + ')'} == {disp_targets}")
                        else:
                            print(f"{assertion} {display_scoped(namespace, funcname)}{disp_args if len(args) != 1 else '(' + disp_args + ')'}")

                        results, std_lines, err_lines = invoker.invoke(namespace, funcname, tuple(map(valof, args)), expect_error=assertion not in ('assert_return',), nresults=len(targets))

                        if not has_passed(results, targets):
                            for line in std_lines:
                                print(line)

                            for line in err_lines:
                                print(line)

                            assert len(results) == len(targets)

                            disp_results = '(' + ', '.join(pretty(val, typ) for val, typ in zip(results, map(typeof, targets))) + ')' if len(results) != 1 else pretty(results[0], typeof(targets[0]))

                            print(f"Test failed {disp_results} != {disp_targets}")
                            exit(1)

                    elif subexpr[0] == 'module':
                        assert assertion in ['assert_trap', 'assert_unlinkable']

                        print(f"{assertion} module compilation", nmodules)

                        subrdr = Reader(rdr.content[b:rdr.i])
                        subtoks = tokens(subrdr)

                        assert next(subtoks) == '('
                        assert next(subtoks) == assertion

                        bb = subrdr.i
                        parse(subtoks)

                        compiled, wasm_path = compile_module(text=subrdr.content[bb:subrdr.i])

                        assert compiled

                        err_lines = invoker.load(wasm_path, ret_errors=True) or []

                        assert all(line.split(': ')[0] in ("TRAP", "ERROR") for line in err_lines)

                        err_lines = [line.split(': ', 1)[1] for line in err_lines] or ["No error reported"]

                        if not all(err_line.startswith(target_line) for err_line, target_line in zip(err_lines, targets)):
                            print(err_lines)
                            print(targets)
                            print(f"Test failed module instantiation did not fail in expected way")
                            exit(1)
                    elif subexpr[0] == 'get':
                        assert assertion == 'assert_return'
                        assert len(targets) == 1

                        _, *names = subexpr

                        if len(names) == 1:
                            namespace, global_name = None, names[0]
                        else:
                            assert names[0].startswith('$')
                            namespace, global_name = names[0][1:], names[1]

                        print(f"{assertion} get {display_scoped(namespace, global_name)} == {dispof(targets[0])}")

                        results, std_lines, err_lines = invoker.get(namespace, global_name)

                        if not has_passed(results, targets):
                            for line in std_lines:
                                print(line)

                            for line in err_lines:
                                print(line)

                            print(f"Test failed {results} != {targets}")
                            exit(1)
                    else:
                        raise ValueError(f"Unexpected command {subexpr[0]!r}")

                elif expr[0] in ('assert_malformed', 'assert_invalid'): #skip validation tests
                    continue
                elif expr[0] in ('func',): #inline module fields
                    assert nmodules == 0

                    compiled, wasm_path = compile_module(text=rdr.content)

                    assert compiled

                    invoker.load(wasm_path)
                    break
                else:
                    raise ValueError(f"Line {lineno}: Unexpected expr {expr[0]!r}")

def test_dir(dirpath, Invoker=Winter, kwargs={}):
    exclude_dirs = kwargs.pop('exclude-dir', [])

    if not isinstance(exclude_dirs, list):
        exclude_dirs = [exclude_dirs]

    exclude_dirs += ['_output', 'output']

    for path in sorted(dirpath.glob("*")):
        if path.is_file() and path.suffix == ".wast":
            test_file(path, Invoker=Invoker, kwargs=kwargs)
        elif path.is_dir() and path.name not in exclude_dirs:
            test_dir(path, Invoker=Invoker, kwargs=kwargs)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 twinter.py <path-to-wast-file/directory> <kwargs>")
        exit(1)

    args = []

    kwargs = {}
    for arg in sys.argv[1:]:
        if not arg.startswith('--'):
            args.append(arg)
        else:
            if '=' in arg:
                key, val = arg[2:].split('=')
            else:
                key, val = arg[2:], True

            if key not in kwargs:
                kwargs[key] = val
            else:
                kwargs[key] = [kwargs[key], val] if not isinstance(kwargs[key], list) else kwargs[key] + [val]

    kwargs.setdefault('winter', 'bin/winterd')

    for arg in args:
        wast_path = Path(arg)
        (test_dir if wast_path.is_dir() else test_file)(wast_path, kwargs=kwargs)

    print("-- All Tests Passed --")
