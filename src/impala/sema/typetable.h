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
    const BorrowedPtrType* borrowed_ptr_type(const Type* referenced_type, int addr_space = 0) {
        return unify(new BorrowedPtrType(*this, referenced_type, addr_space));
    }
    const DefiniteArrayType* definite_array_type(const Type* elem_type, uint64_t dim) {
        return unify(new DefiniteArrayType(*this, elem_type, dim));
    }
    const FnType* fn_type(Types params) { return unify(new FnType(*this, params)); }
    const IndefiniteArrayType* indefinite_array_type(const Type* elem_type) {
        return unify(new IndefiniteArrayType(*this, elem_type));
    }
    const SimdType* simd_type(const Type* elem_type, uint64_t size) { return unify(new SimdType(*this, elem_type, size)); }
    const MutPtrType* mut_ptr_type(const Type* referenced_type, int addr_space = 0) {
        return unify(new MutPtrType(*this, referenced_type, addr_space));
    }
    const NoRetType* type_noret() { return type_noret_; }
    const OwnedPtrType* owned_ptr_type(const Type* referenced_type, int addr_space = 0) {
        return unify(new OwnedPtrType(*this, referenced_type, addr_space));
    }
    const PrimType* prim_type(PrimTypeKind kind);
    const TypeError* type_error() { return type_error_; }
    const UnknownType* unknown_type() { return unify(new UnknownType(*this)); }

private:
    const TypeError* type_error_;
    const NoRetType* type_noret_;
#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
};

}

#endif
