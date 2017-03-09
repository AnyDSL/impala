#include "impala/sema/typetable.h"

namespace impala {

TypeTable::TypeTable()
    : type_noret_(unify(new NoRetType(*this)))
    , type_error_(unify(new TypeError(*this)))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(new PrimType(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
{}

const PrimType* TypeTable::prim_type(const PrimTypeTag tag) {
    switch (tag) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const InferError* TypeTable::infer_error(const Type* dst, const Type* src) {
    if (auto di = dst->isa<InferError>()) {
        if (di->src() == src)
            return di;
    }

    if (auto si = src->isa<InferError>()) {
        if (si->dst() == dst)
            return si;
    }

    return unify(new InferError(*this, dst, src));
}

}
