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
    NoReturnType type_noreturn() { return type_noreturn_; }
    PrimType type(PrimTypeKind kind);
#define IMPALA_TYPE(itype, atype) PrimType type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    Trait trait(const TraitDecl* trait_decl) { return new_unifiable(new TraitNode(*this, trait_decl)); }
    TraitImpl implement_trait(const Impl* impl_decl, Trait trait) { return new_unifiable(new TraitImplNode(*this, impl_decl, trait)); }
    UnknownType unknown_type() { return new_unifiable(new UnknownTypeNode(*this)); }
    TypeVar type_var(Symbol name = Symbol()) { return new_unifiable(new TypeVarNode(*this, name)); }
    FnType fn_type(thorin::ArrayRef<Type> params) { return new_unifiable(new FnTypeNode(*this, params)); }
    TupleType tuple_type(thorin::ArrayRef<Type> elems) { return new_unifiable(new TupleTypeNode(*this, elems)); }
    TupleType unit() { return tuple_type({}); }
    StructType struct_type(const StructDecl* struct_decl) { return new_unifiable(new StructTypeNode(*this, struct_decl)); }

    /// Unify a type and return \p true if the representative changed.
    template<class T> bool unify(Proxy<T> proxy) { return unify(*proxy); }
    bool unify(Unifiable*);

    /**
     * note: bound checking cannot be done during instantiation of the unknowns because of types like fn[A:T[B], B: T[A]](a: A, b: B)
     * therefore it is important to call \p check_bounds after all unknowns have been resolved!
     */
    template<class T>
    Proxy<T> instantiate_unknown(Proxy<T> proxy, std::vector<Type>& types) { 
        return Proxy<T>(instantiate_unknown(*proxy, types)->template as<T>());
    }
    template<class T>
    bool check_bounds(const ASTNode* loc, Proxy<T> proxy, thorin::ArrayRef<Type> types) { 
        auto map = infer(*proxy, types);
        return check_bounds(loc, *proxy, types, map);
    }
    template<class T>
    bool check_bounds(const ASTNode* loc, Proxy<T> proxy, thorin::ArrayRef<Type> types, SpecializeMap& map) {
        return check_bounds(loc, *proxy, types, map);
    }

    virtual void check_impls() = 0;
    void verify() const; ///< Checks if all types in the type tables are sane and correctly unified.

protected:
    bool check_bounds(const ASTNode* loc, Unifiable* unifiable, thorin::ArrayRef<Type> types, SpecializeMap& map);
    SpecializeMap infer(Unifiable*, thorin::ArrayRef<Type>) const;
    template<class T>
    SpecializeMap infer(Proxy<T> proxy, thorin::ArrayRef<Type> var_instances) const { return infer(*proxy, var_instances); }

private:
    Unifiable* instantiate_unknown(Unifiable*, std::vector<Type>&);
    template<class T> 
    Proxy<T> new_unifiable(T* tn) {
        garbage_.push_back(tn);
        return Proxy<T>(tn);
    }

    TraitInstance instantiate(Trait trait, thorin::ArrayRef<Type> args) {
        return new_unifiable(new TraitInstanceNode(trait, args));
    }

    TypetableSet<Unifiable> unifiables_;
    std::vector<Unifiable*> garbage_;
    Trait trait_error_;
    TypeError type_error_;
    NoReturnType type_noreturn_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"

    friend class TraitNode;
    friend class TraitInstanceNode;
};

}

#endif
