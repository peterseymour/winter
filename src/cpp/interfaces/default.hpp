#ifndef HEADER_DEFAULT
#define HEADER_DEFAULT

#include "interface.hpp"


struct Default : MainInterface {
    static const identifier_t NAME;

    static bool push_arg(ValueStack& stack, value_type_t type, const char* arg);
    static bool push_args(ValueStack& stack, const func_sig_t& signature, const char* args[], int nargs);

    virtual const identifier_t& name() const override {return NAME;}
    virtual void init_main(Thread& thread, const func_sig_t& signature) override;
    virtual int exit_main(Thread& thread, const func_sig_t& signature) const override;
    virtual const HostFns& host_functions() const override;
};

#endif