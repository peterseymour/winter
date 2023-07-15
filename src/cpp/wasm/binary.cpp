#include "binary.hpp"
#include "common.hpp"
#include <cstring>
#include <algorithm>


uint64_t Reader::read_leb128(uint16_t& nbits) {
    uint64_t result = 0;

    while (true) {
        uint64_t byte = read<uint8_t>();
        result |= (byte & 0x7f) << nbits;
        nbits += 7;
        if ((byte & 0x80) == 0) {
            if (nbits >= sizeof(uint64_t)*8 + 7)
                error("LEB128 overflow, could be padding bytes");
            break;
        }
    }

    return result;
}


template<>
uint64_t Reader::read_leb128<uint64_t>() {
    uint16_t nbits = 0;
    return read_leb128(nbits);
}

template<>
int64_t Reader::read_leb128<int64_t>() {
    uint16_t nbits = 0;
    uint64_t result = read_leb128(nbits);

    if (nbits >= 64)
        return result;
    else if (result >> (nbits-1))
        return uint64_t(-1) << nbits | result;
    else
        return result;
}

template<typename T>
void Writer::write_leb128(T v)
{
    const auto signbit = std::is_signed<T>::value ? 0x40 : 0x00;

    while (true) {
        byte_t b = v & 0x7f;
        v >>= 7;

        const auto p = ((b & signbit ? v == -1 : v == 0) ? 0x00 : 0x80);

        *fp++ = p | b;

        if (p == 0)
            break;
    }
}

template void Writer::write_leb128<uint64_t>(uint64_t);
template void Writer::write_leb128<int64_t>(int64_t);
template void Writer::write_leb128<uint32_t>(uint32_t);
template void Writer::write_leb128<int32_t>(int32_t);

#include "gen/spec.cpp"

identifier_t str(const char* begin, const char* end) {
    return identifier_t{.size=(uint32_t) (end - begin), .elems=(const byte_t*) begin};
}

identifier_t str(const range_t<const char>& chars) {
    return str(chars.begin(), chars.end());
}

identifier_t str(const char* s) {
    return str(s, s + strlen(s));
}

const char* c_str(const identifier_t& ident) {
    check(ident.elems[ident.size] == '\0', "Bad identifier for c_str");

    return (const char*) ident.elems;
}

identifier_t strnew(const identifier_t& ident) {
    auto len = ident.size;
    auto buf = new byte_t[len + 1];
    std::copy(begin(ident), end(ident), buf);
    buf[len] = '\0';
    return identifier_t{.size=len, .elems=buf};
}

std::ostream& operator<<(std::ostream& os, const identifier_t& ident) {
    for (auto c : ident)
        os << c;

    if (os.flags() & os.hex) {
        os << " (";
        for (auto c : ident)
            os << "\\x" << std::hex << std::setw(2) << std::setfill('0') << (int) (c & 0xff);
        os << ")" << std::dec;
    }

    return os;
}

bool operator==(const identifier_t& ident, const char* s) {
    return ident.size == strlen(s) and strncmp((const char*) ident.elems, s, ident.size) == 0;
}

bool operator<(const identifier_t& x, const identifier_t& y) {
    return std::lexicographical_compare(begin(x), end(x), begin(y), end(y));
}

std::pair<identifier_t, identifier_t> split(const identifier_t& ident, char delim) {
    auto p = (const unsigned char*) memchr(ident.elems, delim, ident.size);

    if (p == NULL)
        return std::make_pair(str(""), ident);
    else
        return std::make_pair(identifier_t{.size=(uint32_t) (p - begin(ident)), .elems=ident.elems}, identifier_t{.size=(uint32_t) (end(ident)-p-1), .elems=p+1});
}

std::ostream& operator<<(std::ostream& os, const function_signature_t& fn_sig) {
    if (fn_sig.form == signature_type_t::FUNC)
        return os << fn_sig.func;

    error("Invalid function_signature_t form");
    return os;
}

std::ostream& operator<<(std::ostream& os, const func_sig_t& fn_sig) {
    return os << fn_sig.params << " -> " << fn_sig.returns;
}

bool operator==(const func_sig_t& x, const func_sig_t& y) {
    return x.params == y.params and x.returns == y.returns;
}

func_sig_t sig(const std::initializer_list<value_type_t>& params, const std::initializer_list<value_type_t>& returns) {
    return func_sig_t{
        .params = new_Arr(params),
        .returns = new_Arr(returns),
    };
}

std::ostream& operator<<(std::ostream& os, block_type_t block_type) {
    for (auto i = 0; i < block_type.nargs; ++i) os << (i > 0 ? "," : "") << '?';
    os << "->";
    for (auto i = 0; i < block_type.nrets; ++i) os << (i > 0 ? "," : "") << '?';
    return os;
}