#ifndef HEADER_OS

#include "common.hpp"

namespace os {

size_t page_size();
size_t page_count(size_t nbytes);
range_t<byte_t> memmap(size_t size, bool noerror);
void memunmap(const range_t<byte_t>& mem);
range_t<byte_t> filemap(const char* path, const char* flags);

typedef void (*guard_handler_t)(byte_t* addr);

void guard(guard_handler_t handler);
void guard(byte_t* begin, byte_t* end, guard_handler_t handler);
void set_guard_handler();

}

#endif