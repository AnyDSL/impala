#include "type.h"

#include <iostream>

using namespace anydsl2;

//------------------------------------------------------------------------------

size_t Type::hash() const {
    size_t seed = hash_combine(hash_value((int) kind()), size());
    for (auto elem : elems())
        seed = hash_combine(seed, elem);
    return seed;
}

bool Type::equal(const Type* other) const {
    bool result = this->kind() == other->kind() && this->size() == other->size();
    for (size_t i = 0, e = size(); i != e && result; ++i)
        result &= this->elem(i) == other->elem(i);
    return result;
}

void Type::dump() const { std::cout << to_string() << std::endl; }

//------------------------------------------------------------------------------

std::string PrimType::to_string() const {
    switch (primtype_kind()) {
#define PRIMTYPE(T) case PrimType_##T: return #T;
#include "primtypes.h"
        default: ANYDSL2_UNREACHABLE;
    }
}

std::string CompoundType::elems_to_string() const {
    std::string result;
    const char* separator = "(";
    for (auto elem : elems()) {
        result = separator + elem->to_string();
        separator = ", ";
    }
    return result + ')';
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : types_()
#define PRIMTYPE(T) , T##_(unify(new PrimType(*this, PrimType_##T)))
#include "primtypes.h"
    , type_error_(unify(new TypeError(*this)))
{}

const Type* TypeTable::unify_base(const Type* type) {
    auto i = types_.find(type);
    if (i != types_.end()) {
        delete type;
        return *i;
    }

    auto p = types_.insert(type);
    assert(p.second && "hash/equal broken");
    return type;
}

const PrimType* TypeTable::primtype(PrimTypeKind kind) {
    switch (kind) {
#define PRIMTYPE(T) case PrimType_##T: return T##_;
#include "primtypes.h"
        default: ANYDSL2_UNREACHABLE;
    }
}

//------------------------------------------------------------------------------
