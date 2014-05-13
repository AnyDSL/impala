#ifndef IMPALA_SEMA_TYPETABLE_H
#define IMPALA_SEMA_TYPETABLE_H

#include "thorin/util/hash.h"

#include "impala/sema/errorhandler.h"
#include "impala/sema/type.h"
#include "impala/sema/trait.h"

namespace impala {

class TraitDecl;

template<class T> struct TypetableHash {
    size_t operator () (const T* t) const { return t->hash(); }
};
template<class T> struct TypetableEqual {
    bool operator () (const T* t1, const T* t2) const { return t1->equal(t2); }
};
template<class T> using TypetableSet = thorin::HashSet<T*, TypetableHash<T>, TypetableEqual<T>>;

//------------------------------------------------------------------------------

class TypeTable : public ErrorHandler {
public:
    TypeTable();
    ~TypeTable();

    TypeError type_error() { return type_error_; }
    Trait trait_error() { return trait_error_; }
    Bound bound_error() { return bound_error_; }
    NoReturnType type_noreturn() { return type_noreturn_; }
    PrimType type(PrimTypeKind kind);
#define IMPALA_TYPE(itype, atype) PrimType type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    FnType fn_type(thorin::ArrayRef<Type> params) { return new_unifiable(new FnTypeNode(*this, params)); }
    TupleType tuple_type(thorin::ArrayRef<Type> elems) { return new_unifiable(new TupleTypeNode(*this, elems)); }
    TupleType unit() { return tuple_type({}); }
    StructType struct_type(const StructDecl* struct_decl) { return new_unifiable(new StructTypeNode(*this, struct_decl)); }
    TypeVar type_var(Symbol name = Symbol()) { return new_unifiable(new TypeVarNode(*this, name)); }
    UnknownType unknown_type() { return new_unifiable(new UnknownTypeNode(*this)); }
    Bound bound(Trait trait, thorin::ArrayRef<Type> args) { return new_unifiable(new BoundNode(trait, args)); }
    Trait trait(const TraitDecl* trait_decl) { return new_unifiable(new TraitNode(*this, trait_decl)); }
    Impl impl(const ImplItem* impl, Bound bound, Type type) { return new_unifiable(new ImplNode(*this, impl, bound, type)); }

    /// Unify a type and return \p true if the representative changed.
    template<class T> bool unify(Proxy<T> proxy) { return unify(*proxy); }
    bool unify(const Unifiable*);

    /**
     * note: bound checking cannot be done during instantiation of the unknowns because of types like fn[A:T[B], B: T[A]](a: A, b: B)
     * therefore it is important to call \p check_bounds after all unknowns have been resolved!
     */
    Type instantiate_unknown(Type, std::vector<Type>&);

    void verify() const; ///< Checks if all types in the type tables are sane and correctly unified.
    static SpecializeMap specialize_map(const Unifiable*, thorin::ArrayRef<Type>);

protected:
    template<class T>
    static SpecializeMap specialize_map(Proxy<T> proxy, thorin::ArrayRef<Type> type_args) { return specialize_map(*proxy, type_args); }

private:
    template<class T> 
    Proxy<T> new_unifiable(T* tn) {
        garbage_.push_back(tn);
        return Proxy<T>(tn);
    }

    TypetableSet<const Unifiable> unifiables_;
    std::vector<const Unifiable*> garbage_;
    TypeError type_error_;
    Trait trait_error_;
    Bound bound_error_;
    NoReturnType type_noreturn_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"

    friend class TraitNode;
    friend class BoundNode;
};

}

#endif
