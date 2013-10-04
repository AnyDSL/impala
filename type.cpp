#include "type.h"

#include <typeinfo>

//#include <sstream>

using namespace anydsl2;

//------------------------------------------------------------------------------

size_t Type::hash() const {
    size_t seed = hash_combine(hash_value(kind()), size());
    for (auto op : ops_)
        seed = hash_combine(seed, op);
    return seed;
}

bool Type::equal(const Node* other) const {
    if (typeid(*this) == typeid(*other) && this->size() == other->size()) {
        for (size_t i = 0, e = size(); i != e; ++i) {
            if (this->ops_[i] != other->ops_[i])
                return false;
        }
        return true;
    }
    return false;
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : types_()
#define IMPALA_TYPE(itype, atype) ,itype##_(unify(new PrimType(*this, Token::TYPE_##itype)))
#include "impala/tokenlist.h"
    , type_error_(unify(new TypeError(*this)))
{}

TypeTable::~TypeTable() {
    for (auto type : types_)
        delete type;
}

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

const PrimType* TypeTable::primtype(TokenKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: ANYDSL2_UNREACHABLE;
    }
}

const FnType* TypeTable::fntype1(const Type* elem) { const Type* elems[1] = { elem }; return fntype(elems); }
const FnType* TypeTable::fntype(anydsl2::ArrayRef<const Type*> elems) { return unify(new FnType(*this, elems)); }
const TupleType* TypeTable::tupletype(anydsl2::ArrayRef<const Type*> elems) { return unify(new TupleType(*this, elems)); }

//------------------------------------------------------------------------------
