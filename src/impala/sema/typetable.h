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
    PrimType primtype(PrimTypeKind kind);
#define IMPALA_TYPE(itype, atype) PrimType type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    Trait trait(const TraitDecl* trait_decl) { return new_unifiable(new TraitNode(*this, trait_decl)); }
    TraitImpl implement_trait(const Impl* impl_decl, Trait trait) { return new_unifiable(new TraitImplNode(*this, impl_decl, trait)); }
    UnknownType unknown_type() { return new_unifiable(new UnknownTypeNode(*this)); }
    TypeVar typevar() { return new_unifiable(new TypeVarNode(*this, Symbol())); }
    TypeVar typevar(Symbol name) { return new_unifiable(new TypeVarNode(*this, name)); }
    FnType fntype(thorin::ArrayRef<Type> params) { return new_unifiable(new FnTypeNode(*this, params)); }
    TupleType tupletype(thorin::ArrayRef<Type> elems) { return new_unifiable(new TupleTypeNode(*this, elems)); }
    TupleType unit() { return tupletype({}); }

    /// Unify a type and return \p true if the representative changed.
    bool unify(Unifiable*);
    template<class T> bool unify(Proxy<T> proxy) { return unify(proxy.node()); }
    /// Checks if all types in the type tables are sane and correctly unified.
    void verify() const;

    /**
     * note: bound checking cannot be done during instantiation of the unknowns because of types like fn[A:T[B], B: T[A]](a: A, b: B)
     * therefore it is important to call \p check_bounds after all unknowns have been resolved!
     */
    Unifiable* instantiate_unknown(Unifiable* unifiable, std::vector<Type>& inst_types) {
        for (size_t i = 0; i < unifiable->num_bound_vars(); ++i) 
            inst_types.push_back(unknown_type());
        auto map = infer(unifiable, inst_types);
        return unifiable->instantiate(map);
    }
    template<class T>
    Proxy<T> instantiate_unknown(Proxy<T> proxy, std::vector<Type>& types) { 
        return Proxy<T>(instantiate_unknown(*proxy, types)->template as<T>());
    }

    bool check_bounds(const ASTNode* loc, Unifiable* unifiable, thorin::ArrayRef<Type> types) {
        assert(types.size() == unifiable->num_bound_vars());
        auto map = infer(unifiable, types);
        return check_bounds(loc, unifiable, types, map);
    }
    template<class T>
    bool check_bounds(const ASTNode* loc, Proxy<T> proxy, thorin::ArrayRef<Type> types) { 
        return check_bounds(loc, *proxy, types);
    }

    virtual void check_impls() {}

protected:
    template<class T>
    bool check_bounds(const ASTNode* loc, Proxy<T> proxy, thorin::ArrayRef<Type> types, SpecializeMap& map) {
        return check_bounds(loc, *proxy, types, map);
    }
    bool check_bounds(const ASTNode* loc, Unifiable* unifiable, thorin::ArrayRef<Type> types, SpecializeMap& map);
    SpecializeMap infer(Unifiable* unifiable, thorin::ArrayRef<Type> var_instances) const {
        assert(unifiable->num_bound_vars() == var_instances.size());
        SpecializeMap map;
        size_t i = 0;
        for (TypeVar v : unifiable->bound_vars())
            map[*v] = *var_instances[i++]; // CHECK ist deref correct here and below?
        assert(map.size() == var_instances.size());
        return map;
    }
    template<class T>
    SpecializeMap infer(Proxy<T> proxy, thorin::ArrayRef<Type> var_instances) const { return infer(*proxy, var_instances); }

private:
    template<class T> 
    Proxy<T> new_unifiable(T* tn) {
        garbage_.push_back(tn);
        return Proxy<T>(tn);
    }

    /// insert all contained unifiables that are not yet unified
    void insert_new(Unifiable*);

    /**
     * Recursively change the representatives of the not-unified elements in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    void change_repr(Unifiable* unifiable, Unifiable* representative) const;
    void change_repr_unifiable(Unifiable* unifiable, Unifiable* representative) const;
    /// change the representative of the contained types
    void change_repr_rec(Unifiable* u, Unifiable* repr) const;

    TraitInstanceNode* instantiate_trait(TraitNode* trait, SpecializeMap& map) { 
        return instantiate_trait(Trait(trait), map); 
    }
    TraitInstanceNode* instantiate_trait(Trait trait, SpecializeMap& map) {
        return new_unifiable(new TraitInstanceNode(trait, map)).node();
    }

    TypetableSet<Unifiable> unifiables_;
    std::vector<Unifiable*> garbage_;
    Trait trait_error_;
    TypeError type_error_;
    NoReturnType type_noreturn_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"

    friend class TypeVarNode;
    friend class TraitNode;
    friend class TraitInstanceNode;
};

}

#endif
