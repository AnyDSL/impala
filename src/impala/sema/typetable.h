#ifndef IMPALA_SEMA_TYPETABLE_H
#define IMPALA_SEMA_TYPETABLE_H

#include "thorin/util/hash.h"

#include "impala/sema/errorhandler.h"
#include "impala/sema/unifiable.h"

namespace impala {

class TypeTable : public ErrorHandler {
public:
    TypeTable();
    ~TypeTable();

#define IMPALA_TYPE(itype, atype) PrimType type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    BorrowedPtrType     borrowd_ptr_type(Type referenced_type) { return join(new BorrowedPtrTypeNode(*this, referenced_type)); }
    TraitApp            trait_app(TraitAbs trait, ArrayRef<Type> args) { return join(new TraitAppNode(trait, args)); }
    TraitApp            trait_app_error() { return trait_app_error_; }
    DefiniteArrayType   definite_array_type(Type elem_type, uint64_t dim) { 
        return join(new DefiniteArrayTypeNode(*this, elem_type, dim)); 
    }
    FnType              fn_type(ArrayRef<Type> params) { return join(new FnTypeNode(*this, params)); }
    Impl                impl(const ImplItem* impl, TraitApp trait_app, Type type) { 
        return join(new ImplNode(*this, impl, trait_app, type)); 
    }
    IndefiniteArrayType indefinite_array_type(Type elem_type) { 
        return join(new IndefiniteArrayTypeNode(*this, elem_type)); 
    }
    SimdType simd_type(Type elem_type, uint64_t size) {
        return join(new SimdTypeNode(*this, elem_type, size));
    }
    NoRetType           type_noret() { return type_noret_; }
    OwnedPtrType        owned_ptr_type(Type referenced_type) { return join(new OwnedPtrTypeNode(*this, referenced_type)); }
    PrimType            type(PrimTypeKind kind);
    StructAbsType       struct_abs_type(const StructDecl* struct_decl) { return join(new StructAbsTypeNode(*this, struct_decl)); }
    StructAppType       struct_app_type(StructAbsType struct_abs, ArrayRef<Type> args) { 
        return join(new StructAppTypeNode(*this, struct_abs, args)); 
    }
    TypedefAbs          typedef_abs(Type t) { return join(new TypedefAbsNode(*this, t)); }
    TraitAbs            trait_abs(const TraitDecl* trait_decl) { return join(new TraitAbsNode(*this, trait_decl)); }
    TraitAbs            trait_abs_error() { return trait_abs_error_; }
    TupleType           tuple_type(ArrayRef<Type> args) { return join(new TupleTypeNode(*this, args)); }
    TupleType           unit() { return tuple_type({}); }
    TypeError           type_error() { return type_error_; }
    TypeVar             type_var(Symbol name = Symbol()) { return join(new TypeVarNode(*this, name)); }
    UnknownType         unknown_type() { return join(new UnknownTypeNode(*this)); }

    /// Unify a type and return its representative.
    template<class T> Proxy<T> unify(Proxy<T> proxy) { return unify(*proxy)->template as<T>(); }
    const Unifiable* unify(const Unifiable*);
    void verify() const; ///< Checks if all types in the type tables are sane and correctly unified.

private:
    template<class T> 
    Proxy<T> join(T* tn) { garbage_.push_back(tn); return Proxy<T>(tn); }

    struct UniHash {
        size_t operator () (const Unifiable* u) const { return u->hash(); }
    };
    struct UniEqual {
        bool operator () (const Unifiable* u1, const Unifiable* u2) const { return u1->equal(u2); }
    };

    thorin::HashSet<const Unifiable*, UniHash, UniEqual> unifiables_;
    std::vector<const Unifiable*> garbage_;
    NoRetType type_noret_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"
    TypeError type_error_;
    TraitAbs trait_abs_error_;
    TraitApp trait_app_error_;
};

}

#endif
