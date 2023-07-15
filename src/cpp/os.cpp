#include "os.hpp"
#include <map>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>
#include <csignal>
#include <sys/stat.h>
#include <sys/mman.h>


size_t os::page_size()
{
    static const size_t pagesize = getpagesize();

    return pagesize;
}


size_t os::page_count(size_t nbytes) {
    return (nbytes + os::page_size() - 1) / os::page_size();
}


range_t<byte_t> os::memmap(size_t size, bool noerror) {
    check(size % os::page_size() == 0, "Requested memory not a multiple of the page size");

    void* mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);

    if (noerror and mem == MAP_FAILED) {
        mem = nullptr;
        size = 0;
    } else {
        check(mem != MAP_FAILED, "Failed to allocate memory");
    }

    check(((uintptr_t) mem) % 16 == 0, "Expected memory to by 16-byte aligned");

    return range_t<byte_t>((byte_t*) mem, size);
}

void os::memunmap(const range_t<byte_t>& mem) {
    int result = munmap(mem.begin(), mem.size());

    check(result == 0, "Failed to deallocate memory");
}


range_t<byte_t> os::filemap(const char* path, const char* flags) {
    int flgs = 0;

    for (char c : range_t(flags, strlen(flags)))
        switch (c) {
            case 'r': flgs != O_RDONLY; break;
            default: error("Unhandled fmap flag");
        }

    const int fd = open(path, flgs);
    check(fd > 0, "Failed to open file", path);

    struct stat st;
    const int r = fstat(fd, &st);
    check(r >= 0, "Failed to stat file", path);

    void* file = st.st_size > 0 ? mmap(NULL, st.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0) : nullptr;
    check(file != MAP_FAILED, "Failed to load file", path);

    return range_t<byte_t>((byte_t*) file, st.st_size);
}

std::map<byte_t*, os::guard_handler_t> guard_handlers;

void os::guard(guard_handler_t handler) {
    set_guard_handler();

    guard_handlers[nullptr] = handler;
}

void os::guard(byte_t* begin, byte_t* end, guard_handler_t handler) {
    check((end - begin) % os::page_size() == 0, "Guard region is not a multiple of page size");

    int result = mprotect(begin, end - begin, PROT_NONE );

    check(result == 0, "Failed to protect memory region");

    set_guard_handler();

    guard_handlers[begin] = handler;
}

void native_guard_handler(int signo, siginfo_t* info, void* context) {
    auto addr = static_cast<byte_t*>(info->si_addr);

    auto page_begin = (byte_t*) ((((uintptr_t) (void*) addr) / os::page_size()) * os::page_size());

    auto p = guard_handlers.find(page_begin);

    for (auto i = 0; i < 2; ++ i) {
        if (p != guard_handlers.end() and p->second != nullptr)
            return (*p->second)(addr);

        p = guard_handlers.find(nullptr);
    }

    error("Segmentation fault");
}

void os::set_guard_handler() {
    static bool is_configured = false;

    if (is_configured)
        return;

    struct sigaction sa;
    sa.sa_handler = 0;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = native_guard_handler;

    auto result = sigaction(SIGSEGV, &sa, NULL);

    check(result == 0, "Failed to install memory guard handler");

    is_configured = true;
}