#ifndef IMPALA_SYMBOL_H
#define IMPALA_SYMBOL_H

#include <cstring>
#include <string>
#include <algorithm>

#include "thorin/util/hash.h"

namespace impala {

class Symbol {
public:
    struct Hash {
        static uint64_t hash(impala::Symbol s) { return thorin::hash(s.str()); }
        static bool eq(impala::Symbol s1, impala::Symbol s2) { return s1 == s2; }
        static impala::Symbol sentinel() { return impala::Symbol(/*dummy*/23); }
    };

    Symbol() { insert(""); }
    Symbol(const char* str) { insert(str); }
    Symbol(const std::string& str) { insert(str.c_str()); }

    const char* str() const { return str_; }
    operator bool() const { return *this != Symbol(""); }
    bool operator == (Symbol symbol) const { return str() == symbol.str(); }
    bool operator != (Symbol symbol) const { return str() != symbol.str(); }
    bool operator == (const char* s) const { return str() == Symbol(s).str(); }
    bool operator != (const char* s) const { return str() != Symbol(s).str(); }
    bool empty() const { return *str_ == '\0'; }
    bool is_anonymous() { return (*this) == "_"; }
    std::string remove_quotation() const;

    static void destroy();

private:
    Symbol(int /* just a dummy */)
        : str_((const char*)(1))
    {}

    void insert(const char* str);

    const char* str_;
    typedef thorin::HashSet<const char*, thorin::StrHash> Table;
    static Table table_;
};

inline std::ostream& operator << (std::ostream& os, Symbol s) { return os << s.str(); }

}

#endif
