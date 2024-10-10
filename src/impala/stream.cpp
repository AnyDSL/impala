#include "impala/stream.h"

namespace impala {

Stream& Stream::endl() {
    ostream() << '\n';
    for (size_t i = 0; i != level_; ++i) ostream() << tab_;
    return *this;
}

Stream& Stream::fmt(const char* s) {
    while (*s) {
        auto next = s + 1;

        switch (*s) {
                // clang-format off
            case '\n': s++; endl();   break;
            case '\t': s++; indent(); break;
            case '\b': s++; dedent(); break;
            // clang-format on
            case '{':
                if (match2nd(next, s, '{')) continue;

                while (*s && *s != '}') s++;

                assert(*s != '}' && "invalid format string for 'streamf': missing argument(s)");
                fe::unreachable();
                break;
            case '}':
                if (match2nd(next, s, '}')) continue;
                assert(false && "unmatched/unescaped closing brace '}' in format string");
                fe::unreachable();
            default: (*this) << *s++;
        }
    }
    return *this;
}

bool Stream::match2nd(const char* next, const char*& s, const char c) {
    if (*next == c) {
        (*this) << c;
        s += 2;
        return true;
    }
    return false;
}

} // namespace impala
