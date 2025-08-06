#include "common.hpp"
#include <cmath>


std::ostream& std::operator<<(ostream& os, uint128_t v) {
    return os << "0x" << std::hex << std::setw(16) << std::setfill('0') << (uint64_t) (v >> 64) << std::setw(16) << std::setfill('0') << uint64_t(v) << std::dec;
}


std::ostream& std::operator<<(ostream& os, int128_t v) {
    if (v == std::numeric_limits<int128_t>::min()) {
        return os << "-0x100000000000000000000000000000000";
    } else if (v < 0) {
        os << "-";
        v = -v;
    }

    return os << (uint128_t) v;
}


template<>
bool parse_decimal<float>(const char* s, float& f) {
    static_assert (sizeof(uint32_t) == sizeof(float));

    char* endptr;

    f = strtof(s, &endptr);

    if (std::isnan(f) and *endptr == ':') {
        const uint32_t bits = strtoul(endptr + 1, &endptr, 16);
        f = float_from_bits<float>(bits);
    }

    if (errno != ERANGE and *endptr == '\0')
        return true;

    return false;
}

template<>
bool parse_decimal<double>(const char* s, double& d) {
    static_assert (sizeof(uint32_t) == sizeof(float));

    char* endptr;

    d = strtod(s, &endptr);

    if (std::isnan(d) and *endptr == ':') {
        const uint64_t bits = strtoull(endptr + 1, &endptr, 16);
        d = float_from_bits<double>(bits);
    }

    if (errno != ERANGE and *endptr == '\0')
        return true;

    return false;
}
