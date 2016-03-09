#ifndef IMPALA_SEMA_TYPETABLE_H
#define IMPALA_SEMA_TYPETABLE_H

#include "thorin/util/hash.h"

#include "impala/sema/type.h"

namespace impala {

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

#define IMPALA_TYPE(itype, atype) const PrimType* type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    const BorrowedPtrType* borrowed_ptr_type(const Type* referenced_type, int addr_space = 0) {
        return unify(new BorrowedPtrType(*this, referenced_type, addr_space));
    }
    const DefiniteArrayType* definite_array_type(const Type* elem_type, uint64_t dim) {
        return unify(new DefiniteArrayType(*this, elem_type, dim));
    }
    const FnType* fn_type(Types params, size_t num_type_params = 0) {
        return unify(new FnType(*this, params, num_type_params));
    }
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
    const StructAbsType* struct_abs_type(const StructDecl* struct_decl) { return unify(new StructAbsType(*this, struct_decl)); }
    const StructAppType* struct_app_type(const StructAbsType* struct_abs, Types args) {
        return unify(new StructAppType(struct_abs, args));
    }
    //TypedefAbs          typedef_abs(const Type* t) { return unify(new TypedefAbsNode(*this, t)); }
    const TupleType* tuple_type(Types args) { return unify(new TupleType(*this, args)); }
    const TupleType* unit() { return tuple_type({}); }
    const TypeError* type_error() { return type_error_; }
    const UnknownType* unknown_type() { return unify(new UnknownType(*this)); }
    const TypeParam* type_param(Symbol symbol) { return unify(new TypeParam(*this, symbol)); }

    /// Unify a type and return its representative.
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    const Type* unify_base(const Type*);

private:
    struct TypeHash {
        uint64_t operator () (const Type* type) const { return type->hash(); }
    };
    struct TypeEqual {
        bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); }
    };

    thorin::HashSet<const Type*, TypeHash, TypeEqual> types_;
    const TypeError* type_error_;
    const NoRetType* type_noret_;
#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
};

}

#endif
