#ifndef IMPALA_STREAM_H
#define IMPALA_STREAM_H

#include <cassert>
#include <cstring>

#include <fstream>
#include <iostream>
#include <memory>
#include <ranges>
#include <sstream>
#include <string>

#include "thorin/util/assert.h"

namespace impala {

class Stream {
public:
    Stream(std::ostream& ostream = std::cout, std::string_view tab = {"    "}, size_t level = 0)
        : ostream_(&ostream)
        , tab_(tab)
        , level_(level) {}

    /// @name getters
    ///@{
    std::ostream& ostream() { return *ostream_; }
    std::string_view tab() const { return tab_; }
    size_t level() const { return level_; }
    ///@}

    /// @name modify Stream
    ///@{
    Stream& indent(size_t i = 1) {
        level_ += i;
        return *this;
    }
    Stream& dedent(size_t i = 1) {
        assert(level_ >= i);
        level_ -= i;
        return *this;
    }
    Stream& endl();
    Stream& flush() {
        ostream().flush();
        return *this;
    }
    ///@}

    /// @name stream
    ///@{
    Stream& fmt(const char* s); ///< Base case.
    /// `fprintf`-like function.
    /// Each @c "{}" in @p s corresponds to one of the variadic arguments in @p args.
    /// The type of the corresponding argument must support @c operator<< on @c std::ostream& or preferably @p Stream.
    /// Furthermore, an argument may also be a range-based container where its elements fulfill the condition above.
    /// You can specify the separator within the braces:
    /// ```cpp
    /// s.fmt("({, })", list) // yields "(a, b, c)"
    /// ```
    /// If you use @c "\n" as separator, it will invoke Stream::endl - keeping indentation:
    /// ```cpp
    /// s.fmt("({\n})", list)
    /// ```
    /// Finally, you can use @c "\n", "\t", and "\b" to @p endl, @p indent, or @p dedent, respectively.
    template<class T, class... Args>
    Stream& fmt(const char* s, T&& t, Args&&... args);
    template<class R, class F, bool rangei = false>
    Stream& range(const R& r, const char* sep, F f);
    template<class R, class F, bool rangei = false>
    Stream& range(const R& r, F f) {
        return range(r, ", ", f);
    }
    template<class R, class F>
    Stream& rangei(const R& r, const char* sep, F f) {
        return range<R, F, true>(r, sep, f);
    }
    template<class R, class F>
    Stream& rangei(const R& r, F f) {
        return range<R, F, true>(r, ", ", f);
    }
    template<class R>
    Stream& range(const R& r, const char* sep = ", ") {
        return range(r, sep, [&](const auto& x) { (*this) << x; });
    }
    ///@}

    void friend swap(Stream& a, Stream& b) {
        using std::swap;
        swap(a.ostream_, b.ostream_);
        swap(a.tab_, b.tab_);
        swap(a.level_, b.level_);
    }

protected:
    bool match2nd(const char* next, const char*& s, const char c);

    std::ostream* ostream_;
    std::string tab_;
    size_t level_;
};

class StringStream : public Stream {
public:
    StringStream()
        : Stream(oss_) {}

    std::string str() const { return oss_.str(); }

    friend void swap(StringStream& a, StringStream& b) {
        using std::swap;
        swap((Stream&)a, (Stream&)b);
        swap(a.oss_, b.oss_);
        // Pointers have to be restored so that this stream still holds the ownership over its ostringstream object.
        a.ostream_ = &a.oss_;
        b.ostream_ = &b.oss_;
    }

private:
    std::ostringstream oss_;
};

template<class... Args>
auto outf(const char* fmt, Args&&... args) {
    return Stream(std::cout).fmt(fmt, std::forward<Args&&>(args)...);
}
template<class... Args>
auto errf(const char* fmt, Args&&... args) {
    return Stream(std::cerr).fmt(fmt, std::forward<Args&&>(args)...);
}
template<class... Args>
auto outln(const char* fmt, Args&&... args) {
    return outf(fmt, std::forward<Args&&>(args)...).endl();
}
template<class... Args>
auto errln(const char* fmt, Args&&... args) {
    return errf(fmt, std::forward<Args&&>(args)...).endl();
}

template<class C>
class Streamable {
private:
    constexpr const C& child() const { return *static_cast<const C*>(this); };

public:
    /// Writes to a file with name @p filename.
    void write(std::string_view filename) const {
        std::ofstream ofs{std::string(filename)};
        Stream s(ofs);
        child().stream(s).endl();
    }
    /// Writes to stdout.
    void dump() const {
        Stream s(std::cout);
        child().stream(s).endl();
    }
    /// Streams to string.
    std::string to_string() const {
        std::ostringstream oss;
        Stream s(oss);
        child().stream(s);
        return oss.str();
    }
};

template<typename T>
concept PtrStream = requires(T x) {
    x->stream(std::declval<Stream&>());
};
template<typename T>
concept RefStream = requires(T x) {
    x.stream(std::declval<Stream&>());
};

template<class T>
requires PtrStream<T> Stream& operator<<(Stream& s, const T& x) { return x->stream(s); }
template<class T>
requires RefStream<T> Stream& operator<<(Stream& s, const T& x) { return x.stream(s); }
/// Fallback uses `std::ostream operator<<`.
template<class T>
Stream& operator<<(Stream& s, const T& x) {
    s.ostream() << x;
    return s;
}

template<class T, class... Args>
Stream& Stream::fmt(const char* s, T&& t, Args&&... args) {
    while (*s != '\0') {
        auto next = s + 1;

        switch (*s) {
            // clang-format off
            case '\n': s++; endl();   break;
            case '\t': s++; indent(); break;
            case '\b': s++; dedent(); break;
            // clang-format on
            case '{': {
                if (match2nd(next, s, '{')) continue;
                s++; // skip opening brace '{'

                std::string spec;
                while (*s != '\0' && *s != '}') spec.push_back(*s++);
                assert(*s == '}' && "unmatched closing brace '}' in format string");

                if constexpr (std::ranges::range<decltype(t)>) {
                    range(t, spec.c_str());
                } else {
                    (*this) << t;
                }

                ++s;                                          // skip closing brace '}'
                return fmt(s, std::forward<Args&&>(args)...); // call even when *s == '\0' to detect extra arguments
            }
            case '}':
                if (match2nd(next, s, '}')) continue;
                assert(false && "unmatched/unescaped closing brace '}' in format string");
                thorin::unreachable();
            default: (*this) << *s++;
        }
    }

    assert(false && "invalid format string for 's'");
    thorin::unreachable();
}

template<class R, class F, bool use_rangei>
Stream& Stream::range(const R& r, const char* sep, F f) {
    const char* cur_sep = "";
    size_t j            = 0;
    for (const auto& elem : r) {
        for (auto i = cur_sep; *i != '\0'; ++i) {
            if (*i == '\n')
                this->endl();
            else
                (*this) << *i;
        }
        if constexpr (use_rangei) {
            f(j++);
        } else {
            f(elem);
        }
        cur_sep = sep;
    }
    return *this;
}

} // namespace impala

#endif
