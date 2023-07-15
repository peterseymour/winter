#ifndef HEADER_INTERFACE
#define HEADER_INTERFACE

#include "winter/runtime.hpp"
#include <map>


struct Interface : External {
    typedef std::map<
            identifier_t,
            std::pair<HostFn, func_sig_t>> HostFns;

    virtual Function* get_function(const identifier_t& name) const override;
    virtual Table* get_table(const identifier_t& name) const override;
    virtual Memory* get_memory(const identifier_t& name) const override;
    virtual Global* get_global(const identifier_t& name) const override;
protected:
    virtual const HostFns& host_functions() const = 0;

    HostFn lookup(const HostFns& fns, const identifier_t& name, const func_sig_t& signature) const;
};


struct MainInterface : Interface {
protected:
    int nargs;
    const char** args;
public:
    virtual MainInterface* set_main_args(int nargs, const char** args);

    virtual void init_main(Thread& thread, const func_sig_t& signature) = 0;
    virtual int exit_main(Thread& thread, const func_sig_t& signature) const = 0;
};


#endif