#include "interface.hpp"


struct Wave : MainInterface {
    static const identifier_t NAME;

    virtual const identifier_t& name() const override {return NAME;}
    virtual void init_main(Thread& thread, const func_sig_t& signature) override;
    virtual int exit_main(Thread& thread, const func_sig_t& signature) const override;
    virtual const HostFns& host_functions() const override;

    static void arg_len(Interface* self, Instance& instance, Thread& thread);
    static void arg_copy(Interface* self, Instance& instance, Thread& thread);
    static void fs_write(Interface* self, Instance& instance, Thread& thread);
};