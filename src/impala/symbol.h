#ifndef IMPALA_SYMBOL_H
#define IMPALA_SYMBOL_H

#include <cstring>
#include <string>
#include <algorithm>

#include "thorin/util/hash.h"

namespace impala {

struct StrHash { uint64_t operator () (const char* s) const; };
struct StrEqual { bool operator () (const char* s1, const char* s2) const { return std::strcmp(s1, s2) == 0; } };

class Symbol {
public:
    Symbol() { insert(""); }
    Symbol(const char* str) { insert(str); }
    Symbol(const std::string& str) { insert(str.c_str()); }

    const char* str() const { return str_; }
    bool operator == (Symbol symbol) const { return str() == symbol.str(); }
    bool operator == (const char* s) const { return str() == Symbol(s).str(); }
    bool operator != (Symbol symbol) const { return str() != symbol.str(); }
    bool operator != (const char* s) const { return str() != Symbol(s).str(); }
    bool empty() const { return *str_ == '\0'; }
    bool is_anonymous() { return (*this) == "_"; }
    std::string remove_quotation() const;

    static void destroy();

private:
    void insert(const char* str);

    const char* str_;
    typedef thorin::HashSet<const char*, StrHash, StrEqual> Table;
    static Table table_;
};

inline std::ostream& operator << (std::ostream& o, Symbol s) { return o << s.str(); }

}

namespace thorin {

template<>
struct Hash<impala::Symbol> {
    uint64_t operator () (impala::Symbol symbol) const { return thorin::hash_value(symbol.str()); }
};

}

#endif
