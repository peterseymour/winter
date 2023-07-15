#include "interface.hpp"
#include "winter/interpreter.hpp"


Function* Interface::get_function(const identifier_t& name) const {
    const HostFns& host_fns = host_functions();

    auto hf = host_fns.find(name);

    if (hf == host_fns.end())
        return nullptr;

    return new HostFunction(const_cast<Interface*>(this), &this->name(), &name, &hf->second.second, hf->second.first);
}

Table* Interface::get_table(const identifier_t& name) const {
    error("Table import not found", this->name(), ":", name);

    return nullptr;
}

Memory* Interface::get_memory(const identifier_t& name) const {
    error("Memory import not found", this->name(), ":", name);

    return nullptr;
}

Global* Interface::get_global(const identifier_t& name) const {
    error("Global import not found", this->name(), ":", name);

    return nullptr;
}

MainInterface* MainInterface::set_main_args(int nargs, const char** args) {
    this->nargs = nargs;
    this->args = args;

    return this;
}