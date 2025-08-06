#ifndef HEADER_ARCH
#define HEADER_ARCH

#include <cmath>
#include <cfenv>
#include "common.hpp"


template<typename U>
uint8_t clz(U x);

template<>
inline uint8_t clz<uint32_t>(uint32_t x) {
    static_assert(sizeof(unsigned int) == sizeof(uint32_t));

    return x == 0 ? 32 : __builtin_clz(x);
}

template<>
inline uint8_t clz<uint64_t>(uint64_t x) {
    const auto c = clz<uint32_t>(x >> 32);
    return c < 32 ? c : c + clz<uint32_t>(x);
}

template<typename U>
uint8_t ctz(U x);

template<>
inline uint8_t ctz<uint32_t>(uint32_t x) {
    static_assert(sizeof(unsigned int) == sizeof(uint32_t));

    return x == 0 ? 32 : __builtin_ctz(x);
}

template<>
inline uint8_t ctz<uint64_t>(uint64_t x) {
    const auto c = ctz<uint32_t>(x);
    return c < 32 ? c : c + ctz<uint32_t>(x >> 32);
}

template<typename U>
uint8_t popcnt(U x);

template<>
inline uint8_t popcnt<uint8_t>(uint8_t x) {
    static_assert(sizeof(unsigned int) >= sizeof(uint8_t));

    return __builtin_popcount(x);
}

template<>
inline uint8_t popcnt<uint32_t>(uint32_t x) {
    static_assert(sizeof(unsigned int) == sizeof(uint32_t));

    return __builtin_popcount(x);
}

template<>
inline uint8_t popcnt<uint64_t>(uint64_t x) {
    return popcnt<uint32_t>(x >> 32) + popcnt<uint32_t>(x);
}

template<typename U>
inline U andnot(U x, U y) {
    return x & ~y;
}

template<typename U>
inline U shl(U x, auto n) {
    return x << (n % bit_size<U>);
}

template<typename U>
inline U shr(U x, auto n) {
    return x >> (n % bit_size<U>);
}

template<typename U>
inline U rotl(U x, int8_t r) {
    return (x << r) | (x >> (std::numeric_limits<U>::digits - r));
}

template<typename U>
inline U rotr(U x, int8_t r) {
    return (x >> r) | (x << (std::numeric_limits<U>::digits - r));
}

template<typename U>
inline U avgr(U x, U y) {
    auto sum = int2<U>(x) + int2<U>(y);
    return (sum >> 1) + (sum & 1);
}

template<typename W, typename N>
inline W extend(N x) {
    static_assert (std::numeric_limits<W>::digits >= std::numeric_limits<N>::digits);

    return x;
}

template<typename N, typename W>
inline N sat(W x) {
    static_assert (std::numeric_limits<W>::digits >= std::numeric_limits<N>::digits);

    if (x < std::numeric_limits<N>::min())
        return std::numeric_limits<N>::min();
    else if (x > std::numeric_limits<N>::max())
        return std::numeric_limits<N>::max();
    else
        return x;
}

template<typename U>
U add_sat(U x, U y) {
    return sat<U>(int2<U>(x) + int2<U>(y));
}

template<typename U>
U sub_sat(U x, U y) {
    return sat<U>(int2<U>(x) - int2<U>(y));
}

template<typename U>
U qmulr_sat(U x, U y) {
    static constexpr auto W = std::numeric_limits<U>::digits;

    return sat<U>(shr((int2<U>(x) * int2<U>(y)) + (int2<U>(1) << (W - 1)), W));
}

template<typename F>
inline F fabs(F f) {
    return float_from_bits<F>(
        float_bits(f) & ~nan_limits<F>::sign_bit
    );
}

template<typename F>
inline F fneg(F f) {
    return float_from_bits<F>(
        float_bits(f) ^ nan_limits<F>::sign_bit
    );
}

template<typename F>
inline F fcopysign(F f1, F f2) {
    return float_from_bits<F>(
        (float_bits(f1) & ~nan_limits<F>::sign_bit)
            |
        (float_bits(f2) &  nan_limits<F>::sign_bit)
    );
}

template<typename F>
inline F fsqrt(F v) {
    return std::sqrt(v);
}

template<typename F>
inline F fceil(F v) {
    return std::ceil(v);
}

template<typename F>
inline F ffloor(F v) {
    return std::floor(v);
}

template<typename F>
inline F fmin(F v, F w) {
    return
        v == w ? fbits_or(v, w) //min -0 +0 -> -0
      : v <  w ? v
      : w <  v ? w
      : choose_nan(v, w);
}

template<typename F>
inline F fmax(F v, F w) {
    return
        v == w ? fbits_and(v, w) //min -0 +0 -> +0
      : v >  w ? v
      : w >  v ? w
      : choose_nan(v, w);
}

template<typename F, typename U>
inline F fpromote(U x) {
    return x;
}

template<typename F, typename U>
inline F fdemote(U x) {
    return x;
}

template<typename F, typename U>
inline F fconvert(U x) {
    return x;
}

template<typename F>
F ftrunc(F f);

template<>
inline float ftrunc(float f) {
    return std::truncf(f);
}

template<>
inline double ftrunc(double f) {
    return std::trunc(f);
}

template<typename F>
F fnearest(F f);

template<>
inline float fnearest(float f) {
    return roundevenf(f);
}

template<>
inline double fnearest(double f) {
    return roundeven(f);
}

template<typename T>
inline bool ftrunc(double d, T& r) {
    static constexpr double min = std::numeric_limits<T>::min();

    static_assert (min == (std::numeric_limits<T>::is_signed ? -ldexp(1, std::numeric_limits<T>::digits) : 0));
    static constexpr double max_1 = ldexp(1, std::numeric_limits<T>::digits);

    r = d = std::trunc(d);

    return min <= d and d < max_1;
}

template<typename T>
inline T ftrunc_sat(double d) {
    static constexpr double min = std::numeric_limits<T>::min();

    static_assert (min == (std::numeric_limits<T>::is_signed ? -ldexp(1, std::numeric_limits<T>::digits) : 0));
    static constexpr double max_1 = ldexp(1, std::numeric_limits<T>::digits);

    if (std::isnan(d))
        return 0;

    d = std::trunc(d);

    if (d < min)
        return std::numeric_limits<T>::min();
    else if (d >= max_1)
        return std::numeric_limits<T>::max();
    else
        return d;
}

#endif