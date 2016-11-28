#include "impala/symbol.h"

#include <iomanip>
#include <sstream>

namespace impala {

Symbol::Table Symbol::table_;

uint64_t StrHash::hash(const char* s) {
    uint64_t seed = thorin::hash_begin();
    const char* i = s;

    while (*i != '\0')
        seed = thorin::hash_combine(seed, *i++);
    return thorin::hash_combine(seed, i-s);
}

#ifdef _MSC_VER
static const char* duplicate(const char* s) { return _strdup(s); }
#else // _MSC_VER
static const char* duplicate(const char* s) { return strdup(s); }
#endif // _MSC_VER

void Symbol::insert(const char* s) {
    auto i = table_.find(s);
    if (i == table_.end())
        i = table_.insert(duplicate(s)).first;
    str_ = *i;
}

void Symbol::destroy() {
    for (auto s : table_)
        free((void*) const_cast<char*>(s));
}

std::string Symbol::remove_quotation() const {
    std::string str = str_;
    if (!str.empty() && str.front() == '"') {
        assert(str.size() >= 2 && str.back() == '"');
        str = str.substr(1, str.size()-2);
    }
    return str;
}

}
