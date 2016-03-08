#include "impala/sema/typetable.h"

namespace impala {

TypeTable::TypeTable()
    : type_error_(unify(new TypeError(*this)))
    , type_noret_(unify(new NoRetType(*this)))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(new PrimType(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
{}

TypeTable::~TypeTable() {
}

const PrimType* TypeTable::prim_type(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const Type* TypeTable::unify_base(const Type* type) {
    return type; // TODO
}

}
