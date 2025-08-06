#include "common.hpp"
#include "os.hpp"
#include <cstring>
#include "wasm/binary.hpp"
#include "interfaces/default.hpp"
#include "interpreter.hpp"


static const char* USAGE =
                                "Usage: winter <wasmfile> [<options>] [--] ...args\n"
                                "       If no invocation point is provided and REPL is not activated\n"
                                "       then any uniquely exported function will be run.\n"
                                "         --help   Show this help\n"
                                "         --invoke         Name of exported function to run\n"
                                "         --instantiate    Name:path of WASM module to instantiate into registry\n"
    IF_TRACE_SWITCH(            "         --trace          Trace function building and evaluation\n")
    IF_RUNTIME_VALIDATE_SWITCH( "         --no-rt-validate Disable runtime validation\n")
                                "         --repl           Enter REPL after optionally invoking function, can take script path\n";


bool strstart(const char* s, const char* t) {
    return strncmp(s, t, strlen(t)) == 0;
}


std::pair<identifier_t, identifier_t> scope_split(const identifier_t& name);
const Module* load_module(const identifier_t& path);
int run(Interpreter& interpreter, const identifier_t& function_name, int nargs, const char* args[], bool repl, const char* repl_path);
void enter_repl(Interpreter& interp, const char* repl_path);

static const auto __main__ = str("__main__");
static const char DELIM = '\xc0'; //can't appear in valid utf-8 string


int main(int argc, const char* argv[]) {
    tracing_on = false;
    runtime_validating_on = true;

    try {
        Interpreter interp;

        /* Process args */
        check(argc >= 2, USAGE);

        auto module_path = str("");
        auto function_name = str("");
        bool repl = false;
        const char* repl_path = nullptr;

        int nargs = 0;
        const char** args = new const char*[argc];

        bool argmode = false;
        for (auto arg : range_t(argv + 1, argc - 1)) {
            if (not argmode) {
                if (strcmp(arg, "--help") == 0) {
                    std::cout << USAGE << std::endl;
                    return 0;
                } else if (strstart(arg, "--invoke=")) {
                    function_name = str(&arg[9]);
                } else if (strstart(arg, "--instantiate=")) {
                    const auto name_and_path = split(str(&arg[14]), ':');

                    check(name_and_path.first.size > 0, "instantiation requires path argument component");

                    interp.instantiate(name_and_path.first, load_module(name_and_path.second));
                }
                IF_TRACE_SWITCH(
                  else if (strcmp(arg, "--trace") == 0) {
                    tracing_on = true;
                })
                IF_RUNTIME_VALIDATE_SWITCH(
                  else if (strcmp(arg, "--no-rt-validate") == 0) {
                    runtime_validating_on = false;
                })else if (strstart(arg, "--repl")) {
                    repl = true;
                    if (strstart(arg, "--repl="))
                        repl_path = &arg[7];
                } else if (strcmp(arg, "--") == 0) {
                    argmode = true;
                } else if (strstart(arg, "--")) {
                    std::cerr << USAGE << std::endl;
                    return 1;
                } else if (module_path.size == 0) {
                    module_path = str(arg);
                } else {
                    args[nargs++] = arg;
                }
            } else {
                args[nargs++] = arg;
            }
        }

        if (module_path.size > 0)
            interp.instantiate(__main__, load_module(module_path));

        return run(interp, function_name, nargs, args, repl, repl_path);
    } catch (const Trap& trap) {
        std::cout << std::flush;
        std::cerr << "TRAP: " << trap.message << std::endl;
        return 1;
    } catch (const Exit&) {
        std::cout << std::flush;
        std::cerr << std::flush;
        return 1;
    } catch(...) {
        std::cout << std::flush;
        std::cerr << "Unexpected exception caught" << std::endl;
        return 1;
    }
}

std::pair<identifier_t, identifier_t> scope_split(const identifier_t& name) {
    return split(name, DELIM);
}

const Module* load_module(const identifier_t& path) {
    const auto file = os::filemap(c_str(path), "r");

    Reader rdr(file.begin(), file.end());

    const Module* module = read<Module>(rdr);

    check(module->magic_cookie == 0x6d736100, "Invalid magic cookie");
    check(module->version == 0x01, "Invalid version");
    check(rdr.relpos() == file.size(), "Failed to read all bytes");

    return module;
}

const Function* lookup_unique_export_function(const Instance* instance, external_kind_t kind) {
    const Function* fn = nullptr;
    size_t found = 0;
    for (auto exp : instance->module->export_section->_)
        if (exp->kind == kind) {
            fn = instance->get_function(exp->name);
            ++found;
        }

    return found == 1 ? fn : nullptr;
}

size_t count_exports(Interpreter& interp, external_kind_t kind) {
    size_t count = 0;

    for (auto entry : interp.registry) {
        auto external = entry.second;

        if (not external->is<Instance>())
            continue;

        auto instance = external->as<Instance>();

        if (instance->module->export_section != nullptr) {
            for (auto exp : instance->module->export_section->_) {
                if (exp->kind == kind)
                    ++count;
            }
        }
    }

    return count;
}

void list_exports(Interpreter& interp, external_kind_t kind) {
    for (auto entry : interp.registry) {
        auto external = entry.second;

        if (not external->is<Instance>())
            continue;

        auto instance = external->as<Instance>();

        if (instance->module->export_section != nullptr) {
            for (auto exp : instance->module->export_section->_) {
                if (exp->kind == kind) {
                    if (entry.first != __main__)
                        std::cout << entry.first << ':';
                    std::cout << exp->name << ": ";

                    if (kind == external_kind_t::FUNCTION)
                        std::cout << *instance->function_space[exp->function.index]->signature << std::endl;
                    else if (kind == external_kind_t::GLOBAL)
                        std::cout << instance->global_space[exp->global.index]->typed_value() << std::endl;
                    else
                        error("Not implemented");
                }
            }
        }
    }
}


int run(Interpreter& interp, const identifier_t& function_name, int nargs, const char* args[], bool repl, const char* repl_path) {
    const Function* fn = nullptr;

    if (function_name != "") {
        auto scoped_function_name = scope_split(function_name);

        auto module_name = scoped_function_name.first.size > 0 ? scoped_function_name.first : __main__;
        auto function_name = scoped_function_name.second;

        fn = interp.registry.lookup(module_name)->get_function(function_name);
    }
    else if (not repl) {
        fn = lookup_unique_export_function(interp.registry.lookup(__main__)->as<Instance>(), external_kind_t::FUNCTION);
    }

    if (fn != nullptr) {
        MainInterface* main_interface = nullptr;

        for (auto e : interp.registry) {
            if (e.second->is<MainInterface>()) {
                check(main_interface == nullptr, "Multiple interfaces support main");
                main_interface = e.second->as<MainInterface>();
            }
        }

        if (main_interface == nullptr)
            main_interface = interp.registry.lookup(str("default"))->as<MainInterface>();

        IF_TRACING(std::cout << "Invoke " << *fn->signature << " with " << nargs << " command line arg(s)" << std::endl;)

        main_interface->set_main_args(nargs, args)->init_main(*interp.threads[0], *fn->signature);

        const auto status = fn->invoke(&interp);

        if (status != nullptr)
            std::cerr << status << std::endl;

        if (not repl)
            return status != nullptr ? 1 : main_interface->exit_main(*interp.threads[0], *fn->signature);
    }
    else if (function_name != "") {
        std::cerr << "Invalid main function\nOptions are:" << std::endl;
        list_exports(interp, external_kind_t::FUNCTION);
        return 1;
    }
    else if (nargs > 0) {
        std::cerr << "Unexpected extra arguments with no main function" << std::endl;
        return 1;
    }

    if (repl)
        enter_repl(interp, repl_path);

    return 0;
}

#define MAX_ARGS 32

void enter_repl(Interpreter& interp, const char* repl_path) {
    auto fh = repl_path != nullptr ? fopen(repl_path, "r") : stdin;

    check(fh != NULL, "Bad REPL script path");

    char* command = nullptr;
    size_t bufsz = 0;

    while (true) {
        std::cout << "> " << std::flush;

        auto sz = getline(&command, &bufsz, fh);

        if (fh != NULL and sz < 0) {
            fclose(fh);
            if (command != nullptr)
                free(command);
            fh = stdin;
            std::cout << std::endl;
            continue;
        }
        else if (fh != stdin) {
            std::cout << command << std::endl;
        }
        else if (command[sz] == '\n') {
            command[sz] = '\0';
        }

        int nargs = 0;
        range_t<char> args[MAX_ARGS];
        bool quotes[MAX_ARGS];

        for (auto p = command; *p != '\0'; ) {
            for (;*p == ' '; ++p);
            if  ( *p == '\0' or *p == '\n') break;

            check(nargs < MAX_ARGS, "Too many args");

            auto q = p;

            const auto arg_begin = q;

            while (true) {
                if (*p == '"') {
                    quotes[nargs] = true;

                    check(*p++ == '"', "expected \"");
                    for (; *p != '"';)
                        if (*p == '\\')
                            switch (*++p) {
                                case '\\':
                                case '"':
                                    *q++ = *p++;
                                    break;
                                case 't':
                                    *q++ = '\t'; p++;
                                    break;
                                case 'n':
                                    *q++ = '\n'; p++;
                                    break;
                                case 'r':
                                    *q++ = '\r'; p++;
                                    break;
                                case 'x': {
                                    ++p;
                                    uint8_t b = 0;
                                    for (auto j = 0; j < 2; ++j) {
                                        char c = *p++;
                                        check(isxdigit(c), "Expected hex digit");

                                        b <<= 4;
                                        b |= (c >= 'A') ? (c >= 'a') ? (c - 'a' + 10) : (c - 'A' + 10) : (c - '0');
                                    }

                                    *q++ = b;
                                    break;
                                }
                                default:
                                    error("Unhandled character escape sequence \\", *p);
                                    break;
                            }
                        else
                            *q++ = *p++;
                    check(*p++ == '"', "unterminated \"");
                }
                else {
                    quotes[nargs] = false;

                    for (;*p != '\0' and *p != '\n' and *p != ' ' and *p != ':'; *q++ = *p++)
                        ;
                }

                if (*p != ':')
                    break;

                *q++ = DELIM;
                ++p;
            }

            const auto arg_end = q;

            args[nargs] = range_t<char>(arg_begin, arg_end);

            while (q < p)
                *q++ = '\0';

            ++nargs;

            if (*p == '\0')
                break;

            *p++ = '\0';
        }

        if (nargs == 0)
            continue;

        const auto cmd = args[0].begin();
        const bool unquoted = not quotes[0];

        try {
            if (unquoted and strstart(cmd, ";"))
                continue;
            else if (unquoted and strcmp(cmd, "?") == 0 and nargs == 1) {
                if (count_exports(interp, external_kind_t::GLOBAL)) {
                    std::cout << " -- Globals --" << std::endl;
                    list_exports(interp, external_kind_t::GLOBAL);
                    std::cout << std::endl;
                }

                if (count_exports(interp, external_kind_t::FUNCTION)) {
                    std::cout << " -- Functions --" << std::endl;
                    list_exports(interp, external_kind_t::FUNCTION);
                    std::cout << std::endl;
                }

            } else if (unquoted and strcmp(cmd, "instantiate") == 0 and nargs == 2) {
                auto name_and_path = scope_split(str(args[1]));

                auto module_name = name_and_path.first.size > 0 ? strnew(name_and_path.first) : __main__;
                auto module_path = name_and_path.second;

                interp.instantiate(module_name, load_module(module_path), true);
            } else if (unquoted and strcmp(cmd, "register") == 0 and nargs == 2) {
                auto module_name = strnew(str(args[1]));

                interp.registry.add(module_name, interp.registry.lookup(__main__), true);
            } else if (unquoted and strcmp(cmd, "get") == 0 and nargs == 2) {
                auto scoped_name = scope_split(str(args[1]));

                auto module_name = scoped_name.first.size > 0 ? strnew(scoped_name.first) : __main__;
                auto global_name = scoped_name.second;

                auto gbl = interp.registry.lookup(module_name)->get_global(global_name);

                if (gbl != nullptr) {
                    std::cout << gbl->typed_value() << std::endl;
                }
                else
                    std::cerr << "Global not found: " << str(args[1]) << std::endl;
            } else {
                auto scoped_function_name = scope_split(str(args[0]));

                auto module_name = scoped_function_name.first.size > 0 ? scoped_function_name.first : __main__;
                auto function_name = scoped_function_name.second;

                auto fn = interp.registry.lookup(module_name)->get_function(function_name);

                if (fn != nullptr) {
                    auto& stack = interp.threads[0]->stack;
                    auto& call_stack = interp.threads[0]->call_stack;

                    while (not stack.empty())
                        stack.pop();

                    while (not call_stack.empty())
                        call_stack.pop();

                    const auto& sig = *fn->signature;

                    if (sig.params.size != nargs - 1) {
                        std::cerr << "Wrong number of arguments for main: found " << sig.params.size << " expected " << nargs - 1 << std::endl;
                        continue;
                    }

                    const char* cargs[MAX_ARGS];

                    for (auto i = 1; i < nargs; ++i) {
                        cargs[i] = args[i].begin();

                        for (auto p = args[i].begin(); p != args[i].end(); ++p)
                            if (*p == DELIM) *p = ':';
                    }

                    if (not Default::push_args(stack, sig, cargs + 1, nargs - 1)) {
                        std::cerr << "Arguments do not match signature: " << sig << std::endl;
                        for (auto i = 1; i < nargs; ++i)
                            std::cout << "  [" << i - 1 << "] '" << cargs[i] << "'" << std::endl;
                        continue;
                    }

                    const char* trap_message = fn->invoke(&interp);

                    if (trap_message == nullptr) {
                        if (stack.size() != sig.returns.size) {
                            stack.debug();
                            std::cerr << "Invalid stack state exiting main" << std::endl;
                            continue;
                        }

                        auto fp = stack.frame(stack.size());

                        auto i = 0;
                        for (auto value_type : sig.returns)
                            std::cout << TypedValue(value_type, ValueStack::frame_elem(fp, i++)) << std::endl;
                    } else {
                        std::cout << std::flush;
                        std::cerr << "TRAP: " << trap_message << std::endl;
                    }
                }
                else
                    std::cerr << "Function not found: " << std::hex << str(args[0]) << std::endl;
            }
        } catch (const Exit& exit) {
            std::cout << std::flush;
            std::cerr << std::flush;
        } catch (const Trap& trap) {
            std::cout << std::flush;
            std::cerr << "TRAP: " << trap.message << std::endl;
        }
    }
}