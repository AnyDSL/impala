#include "impala/symbol.h"

#include <iomanip>
#include <sstream>

namespace impala {

Symbol::Table Symbol::table_;

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
