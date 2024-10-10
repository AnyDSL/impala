#ifndef THORIN_UTIL_SYMBOL_H
#define THORIN_UTIL_SYMBOL_H

#include <string>

#include <absl/container/flat_hash_map.h>
#include <absl/container/flat_hash_set.h>

#include "mim/util/hash.h"

namespace impala {

struct StrHash {
  size_t operator()(const char *s) const { return mim::hash(s); }
};

struct StrEq {
  bool operator()(const char *s1, const char *s2) const {
    return std::strcmp(s1, s2) == 0;
  }
};

class Symbol {
public:
  Symbol() { insert(""); }
  Symbol(const char *str) { insert(str); }
  Symbol(const std::string &str) { insert(str.c_str()); }

  const char *c_str() const { return str_; }
  std::string str() const { return str_; }
  operator bool() const { return *this != Symbol(""); }
  bool operator==(Symbol symbol) const { return c_str() == symbol.c_str(); }
  bool operator!=(Symbol symbol) const { return c_str() != symbol.c_str(); }
  bool operator==(const char *s) const { return c_str() == Symbol(s).c_str(); }
  bool operator!=(const char *s) const { return c_str() != Symbol(s).c_str(); }
  bool empty() const { return *str_ == '\0'; }
  bool is_anonymous() { return (*this) == "_"; }
  std::string remove_quotation() const;

private:
  Symbol(int /* just a dummy */) : str_((const char *)(1)) {}

  struct Table {
    ~Table() {
      for (auto s : set)
        free((void *)const_cast<char *>(s));
    }

    absl::flat_hash_set<const char *, StrHash, StrEq> set;
  };

  void insert(const char *str);

  const char *str_;
  static Table table_;

  template <class H> friend H AbslHashValue(H h, Symbol symbol) {
    return H::combine(std::move(h), symbol.str_);
  }
};

inline Symbol operator+(Symbol s1, Symbol s2) {
  return std::string(s1.c_str()) + s2.str();
}
inline Symbol operator+(Symbol s1, const char *s2) {
  return std::string(s1.c_str()) + s2;
}
inline Symbol operator+(Symbol s1, std::string s2) {
  return std::string(s1.c_str()) + s2;
}
inline std::ostream &operator<<(std::ostream &os, Symbol s) {
  return os << s.c_str();
}

template <class T> using SymbolMap = absl::flat_hash_map<Symbol, T>;

} // namespace impala

#endif
