#include "interface.hpp"


struct Wasi : MainInterface {
    static const identifier_t NAME;

    virtual const identifier_t& name() const override {return NAME;}
    virtual void init_main(Thread& thread, const func_sig_t& signature) override;
    virtual int exit_main(Thread& thread, const func_sig_t& signature) const override;
    const HostFns& host_functions() const override;

    static void args_get(Interface* self, Instance& instance, Thread& thread);
    static void args_sizes_get(Interface* self, Instance& instance, Thread& thread);
    static void fd_close(Interface* self, Instance& instance, Thread& thread);
    static void fd_fdstat_get(Interface* self, Instance& instance, Thread& thread);
    static void fd_seek(Interface* self, Instance& instance, Thread& thread);
    static void fd_write(Interface* self, Instance& instance, Thread& thread);
    static void proc_exit(Interface* self, Instance& instance, Thread& thread);
};