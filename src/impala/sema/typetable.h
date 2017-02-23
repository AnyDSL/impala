#ifndef IMPALA_SEMA_TYPETABLE_H
#define IMPALA_SEMA_TYPETABLE_H

#include "thorin/util/hash.h"

#include "impala/sema/type.h"

namespace impala {

class TypeTable : public TypeTableBase<TypeTable> {
public:
    TypeTable();

#define IMPALA_TYPE(itype, atype) const PrimType* type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    const DefiniteArrayType* definite_array_type(const Type* elem_type, uint64_t dim) {
        return unify(new DefiniteArrayType(*this, elem_type, dim));
    }
    const FnType* fn_type(Types params) { return unify(new FnType(*this, params)); }
    const IndefiniteArrayType* indefinite_array_type(const Type* elem_type) {
        return unify(new IndefiniteArrayType(*this, elem_type));
    }
    const SimdType* simd_type(const Type* elem_type, uint64_t size) { return unify(new SimdType(*this, elem_type, size)); }
    const BorrowedPtrType* borrowed_ptr_type(const Type* pointee, bool mut, int addr_space) {
        return unify(new BorrowedPtrType(*this, pointee, mut, addr_space));
    }
    const OwnedPtrType* owned_ptr_type(const Type* pointee, int addr_space) {
        return unify(new OwnedPtrType(*this, pointee, addr_space));
    }
    const RefType* ref_type(const Type* pointee, bool mut, int addr_space) {
        return unify(new RefType(*this, pointee, mut, addr_space));
    }
    const NoRetType* type_noret() { return type_noret_; }
    const PrimType* prim_type(PrimTypeTag tag);
    const UnknownType* unknown_type() { return unify(new UnknownType(*this)); }
    const TypeError* type_error() { return type_error_; }

private:
    const NoRetType* type_noret_;
    const TypeError* type_error_;
#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
};

}

#endif
