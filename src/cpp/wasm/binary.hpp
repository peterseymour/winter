#ifndef HEADER_WASM_BINARY
#define HEADER_WASM_BINARY


#include <cstdint>
#include <ostream>
#include "common.hpp"


template<typename T>
class Binary
{
protected:
    T* begin;
    T* end;
    T* fp;

    Binary(T* begin, T* end)
        : begin(begin), end(end), fp(begin)
    {
    }
public:
    T* pos() const {
        return fp;
    }

    T* pos(T* p) {
        auto old = fp;
        fp = p;
        return old;
    }

    void seek(size_t offset) {
        pos(begin + offset);
    }

    size_t size() const {
        return end - begin;
    }

    bool atend() const {
        return fp >= end;
    }

    bool overflow() const {
        return fp > end;
    }

    operator bool() const {
        return not overflow();
    }

    ptrdiff_t relpos() const {
        return fp - begin;
    }
};


class Reader : public Binary<const byte_t> {
    byte_t* image;

    uint64_t read_leb128(uint16_t& shift);
public:
    Reader(byte_t* begin, const byte_t* end)
        : Binary(begin, end), image(begin) {}

    byte_t* imgpos() const {
        return image + relpos();
    }

    template<typename T>
    T read() {
        return *pointer_cast<const T*>(fp)++;
    }

    template<typename T>
    T peek() {
        return *pointer_cast<const T*>(fp);
    }

    template<typename T>
    T read_leb128();

    Reader clone() const {
        return Reader(imgpos(), end);
    }
};


class Writer : public Binary<byte_t> {
public:
    Writer(byte_t* begin, byte_t* end)
        : Binary(begin, end) {}

    template<typename T>
    void write(T v) {
        *pointer_cast<T>(fp)++ = v;
    }

    template<typename T>
    void write_leb128(T v);
};


template<typename T>
struct VarType {
    using type = const T*;
};

template<typename T>
struct ReaderFor;

template<typename T>
typename VarType<T>::type read(Reader& rdr) {
    return ReaderFor<T>::read(rdr);
}


template<typename T>
bool isa(Reader& rdr);

template<typename T>
struct Maybe {
    using type = T;
};

template<typename T>
struct VarType<Maybe<T>> {
    using type = typename VarType<T>::type;
};

template<typename T>
struct ReaderFor<Maybe<T>> {
    static typename VarType<T>::type read(Reader& rdr) {
        return isa<T>(rdr) ? ::read<T>(rdr) : nullptr;
    }
};

template<typename T>
struct Many {
    const byte_t* begin;
    const byte_t* end;
};

template<typename T>
struct VarType<Many<T>> {
    using type = Many<T>;
};

template<typename T>
struct ReaderFor<Many<const T>> {
    static typename VarType<Many<const T>>::type read(Reader& rdr) {
        const auto begin = rdr.pos();

        while(isa<T>(rdr))
            ::read<T>(rdr);

        const auto end = rdr.pos();

        return {.begin=begin, .end=end};
    }
};

#include "gen/spec.hpp"
#include "gen/spec.tpp"

template<typename T>
Arr<T> new_Arr(const std::initializer_list<T>& elems) {
    return Arr<value_type_t>{
        .size = (uint32_t) elems.size(),
        .elems = new_array(elems),
    };
};

template<typename T>
auto begin(const Arr<T>& arr) {
    return arr.elems;
}

template<typename T>
auto end(const Arr<T>& arr) {
    return arr.elems + arr.size;
}

template<typename T>
auto operator==(const Arr<T>& a, const Arr<T>& b) {
    if (a.size != b.size)
        return false;

    for (auto i = 0; i < a.size; ++i)
        if (a.elems[i] != b.elems[i])
            return false;

    return true;
}

template<typename T>
std::ostream& operator<<(std::ostream& os, const Arr<T>& arr) {
    os << '(';
    bool first = true;
    for (auto elem : arr) {
        if (not first) os << ", ";
        os << elem;
        first = false;
    }
    return os << ')';
}

identifier_t str(const char* begin, const char* end);
identifier_t str(const range_t<const char>& chars);
identifier_t str(const char* s);
const char* c_str(const identifier_t& ident);
identifier_t strnew(const identifier_t& ident);

std::ostream& operator<<(std::ostream& os, const identifier_t& ident);

bool operator==(const identifier_t& ident, const char* s);
bool operator<(const identifier_t& x, const identifier_t& y);

std::pair<identifier_t, identifier_t> split(const identifier_t& ident, char delim);


typedef typeof(function_signature_t::func) func_sig_t;

std::ostream& operator<<(std::ostream& os, const function_signature_t& fn_sig);
std::ostream& operator<<(std::ostream& os, const func_sig_t& fn_sig);

bool operator==(const func_sig_t& x, const func_sig_t& y);

func_sig_t sig(const std::initializer_list<value_type_t>& params, const std::initializer_list<value_type_t>& returns);

static constexpr auto I32 = value_type_t::I32;
static constexpr auto I64 = value_type_t::I64;
static constexpr auto F32 = value_type_t::F32;
static constexpr auto F64 = value_type_t::F64;

struct block_type_t {
    uint32_t nargs;
    uint32_t nrets;

    int32_t nvals() const {return (int32_t) nrets - (int32_t) nargs;}
};


std::ostream& operator<<(std::ostream&, block_type_t);


#endif
