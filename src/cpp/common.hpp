#ifndef COMMON_HEADER
#define COMMON_HEADER

#include <cstdint>
#include <utility>
#include <type_traits>
#include <iostream>
#include <iomanip>


typedef uint8_t byte_t;
typedef __uint128_t uint128_t;
typedef __int128_t int128_t;

using std::size_t;
using std::ptrdiff_t;

static_assert(sizeof(byte_t) == 1, "Invalid byte size");

#define enumval(enumtype, name) (std::underlying_type<enumtype>::type(enumtype::name))

namespace std {
    ostream& operator<<(ostream& os, uint128_t v);
    ostream& operator<<(ostream& os, int128_t v);
}

template <typename... Ts>
const char* fstr(const char* format, Ts&&... args) {
    static char buffer[256];
    snprintf(buffer, 256, format, std::forward<Ts>(args)...);
    return buffer;
}

template <typename... Ts>
std::ostream& fprint(std::ostream& os, Ts&&... args) {
    int i = 0;

    ([&]{
        if (i != 0) os << ' ';
        os << args;
        ++i;
    } (), ...);

    return os;
}

struct Exit {};

template <typename... Ts>
void error(const char* message, Ts&&... args) {
    fprint(std::cerr, "ERROR:", message, std::forward<Ts>(args)...) << std::endl;
    throw Exit();
}

template <typename... Ts>
void warn(const char* message, Ts&&... args) {
    fprint(std::cerr, "WARN:", message, std::forward<Ts>(args)...) << std::endl;
}

template <typename... Ts>
void check(bool cond, Ts&&... args) {
    if (not cond)
        error(std::forward<Ts>(args)...);
}

template <typename T, typename U>
inline constexpr T reinterpret(const U& u) {
    static_assert(sizeof(T) == sizeof(U));
    return *reinterpret_cast<const T*>(&u);
}

template <typename PT, typename PU>
inline auto& pointer_cast(PU& p) {
    static_assert(sizeof(PT) == sizeof(PU));
    return reinterpret_cast<PT&>(p);
}

template <typename PT, typename PU>
inline const auto& pointer_cast(const PU& p) {
    static_assert(sizeof(PT) == sizeof(PU));
    return reinterpret_cast<const PT&>(p);
}

template <typename T, typename U>
inline constexpr const T& raw_read(const U& u) {
    return *reinterpret_cast<const T*>(&u);
}

template <typename T, typename U>
inline constexpr void raw_write(T& t, const U& u) {
    *reinterpret_cast<U*>(&t) = u;
}

template<typename T=void>
class range_t {
    T* b;
    T* e;
public:
    range_t() : b(nullptr), e(nullptr) {}
    range_t(T* begin, T* end) : b(begin), e(end) {}
    range_t(T* begin, size_t size) : range_t(begin, begin + size) {}

    template<typename S>
    range_t(const range_t<S>& r) : b(r.begin()), e(r.end()) {}

    T* begin() const {return b;}
    T* end() const {return e;}
    ptrdiff_t size() const {return e - b;}

    T& operator[](size_t n) {return b[n];}
    const T& operator[](size_t n) const {return b[n];}

    template<typename S>
    range_t<S> cast() const {
        return range_t<S>((S*) b, (S*) e);
    }

    size_t indexof(const T& t) const {
        return &t - b;
    }

    template<typename S>
    size_t indexof(const S* s) const {
        return reinterpret_cast<const T*>(s) - b;
    }
};

template<typename S>
range_t<S> make_range(S* begin, S* end) {
    return range_t<S>(begin, end);
}

template<typename S>
range_t<S> make_range(S* begin, size_t size) {
    return range_t<S>(begin, size);
}

template<typename T>
T* new_array(const std::initializer_list<T>& elems) {
    T* arr = new T[elems.size()];

    auto p = arr;
    for (const auto& elem : elems)
        *p++ = elem;

    return arr;
}

template<typename T>
bool parse_integer(const char* s, T& t) {
    char* endptr;

    if (std::numeric_limits<T>::is_signed) {
        const long long ll = strtoll(s, &endptr, 10);

        static_assert (sizeof(ll) >= sizeof(T));

        if (errno != ERANGE and *endptr == '\0' and std::numeric_limits<T>::min() <= ll and ll <= std::numeric_limits<T>::max()) {
            t = ll;
            return true;
        }
    }
    else {
        const unsigned long long ull = strtoull(s, &endptr, 10);

        static_assert (sizeof(ull) >= sizeof(T));

        if (errno != ERANGE and *endptr == '\0' and std::numeric_limits<T>::min() <= ull and ull <= std::numeric_limits<T>::max()) {
            t = ull;
            return true;
        }
    }

    return false;
}

template<typename T>
bool parse_decimal(const char* s, T& t);


template<typename T> struct float_uint;

template<> struct float_uint<float>  {static_assert (sizeof(float)  == sizeof(uint32_t)); using type = uint32_t;};
template<> struct float_uint<double> {static_assert (sizeof(double) == sizeof(uint64_t)); using type = uint64_t;};

inline constexpr auto float_bits(float f) {
    return reinterpret<typename float_uint<float>::type>(f);
}

inline constexpr auto float_bits(double d) {
    return reinterpret<typename float_uint<double>::type>(d);
}

template<typename F, typename U>
constexpr auto float_from_bits(U u) {
    static_assert(sizeof(typename float_uint<F>::type) == sizeof(U));
    return reinterpret<F>(u);
}

template<typename F>
constexpr auto fbits_or(F f1, F f2) {
    return float_from_bits<F>(float_bits(f1) | float_bits(f2));
}

template<typename F>
constexpr auto fbits_and(F f1, F f2) {
    return float_from_bits<F>(float_bits(f1) & float_bits(f2));
}


template<typename F>
struct nan_limits;

template<>
struct nan_limits<float> {
    static constexpr auto exp_nbits = 8;
    static constexpr auto significand_nbits = 23;
    static constexpr auto sign_bit = ((uint32_t) 1) << (exp_nbits + significand_nbits);
    static constexpr auto base = (((uint32_t) ((1 << exp_nbits) - 1)) << significand_nbits);    //0x7f800000 (infinity)
    static constexpr auto canonical_payload = (((uint32_t) 1) << (significand_nbits - 1));      //0x00400000 (makes a nan)
    //static constexpr auto arithmetic_payload = canonical_payload + 1;
};

template<>
struct nan_limits<double> {
    static constexpr auto exp_nbits = 11;
    static constexpr auto significand_nbits = 52;
    static constexpr auto sign_bit = ((uint64_t) 1) << (exp_nbits + significand_nbits);
    static constexpr auto base = (((uint64_t) ((1 << exp_nbits) - 1)) << significand_nbits);    //0x7ff0000000000000 (infinity)
    static constexpr auto canonical_payload = (((uint64_t) 1) << (significand_nbits - 1));      //0x0008000000000000 (makes a nan)
    //static constexpr auto arithmetic_payload = canonical_payload + 1;
};

/*
template<typename F>
constexpr F nan(typename float_uint<F>::type payload=nan_limits<F>::canonical_payload) {
    return float_from_bits<F>(nan_limits<F>::base | payload);
}
*/

template<typename F>
bool is_nan(F f) {
    return f != f;
}

template<typename F>
auto choose_nan(F f1, F f2) {
    return float_from_bits<F>(
        (is_nan(f1) ? float_bits(f1) : 0)
            |
        (is_nan(f2) ? float_bits(f2) : 0)
            |
        (nan_limits<F>::base | nan_limits<F>::canonical_payload)
    );
}

#endif