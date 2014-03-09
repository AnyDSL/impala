/*
 * TypeTable.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

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

    TypeVar typevar() { return new_unifiable(new TypeVarNode(*this)); }
    FnType fntype(thorin::ArrayRef<Type> params) { return new_unifiable(new FnTypeNode(*this, params)); }
    TupleType tupletype(thorin::ArrayRef<Type> elems) { return new_unifiable(new TupleTypeNode(*this, elems)); }
    TupleType unit() { return tupletype({}); }

    /// unify a type and return \p true if the representative changed
    template<class T> bool unify(Proxy<T> t);

    /// Checks if all types in the type tables are sane and correctly unified.
    void verify() const;

protected:
    template<class T> SpecializeMapping create_spec_mapping(T generic, thorin::ArrayRef<Type> var_instances) const {
        assert(generic->num_bound_vars() == var_instances.size());
        SpecializeMapping mapping;
        size_t i = 0;
        for (TypeVar v : generic->bound_vars()) {
            mapping[*v] = *var_instances[i++]; // CHECK ist deref correct here and below?
        }
        assert(mapping.size() == var_instances.size());
        return mapping;
    }

private:
    template<class T> 
    Proxy<T> new_unifiable(T* tn) {
        garbage_.push_back(tn);
        return Proxy<T>(tn);
    }

    /// insert all contained unifiables that are not yet unified
    template<class T> void insert_new(T*);
    /// insert all contained types
    void insert_new_rec(TypeNode*);
    void insert_new_rec(TraitNode*);
    void insert_new_rec(TraitImplNode*) {}

    /**
     * Recursively change the representatives of the not-unified elements in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    template<class T> void change_repr(T* t, T* repr) const;
    /// change the representative of the bound type variables
    template<class T> void change_repr_generic(T* t, T* repr) const;
    /// change the representative of the contained types
    void change_repr_rec(TypeNode* t, TypeNode* repr) const;
    void change_repr_rec(TraitNode* t, TraitNode* repr) const {}
    void change_repr_rec(TraitImplNode* t, TraitImplNode* repr) const {}

    TraitInstanceNode* instantiate_trait(TraitNode* trait, SpecializeMapping& mapping) { return instantiate_trait(Trait(trait), mapping); }
    TraitInstanceNode* instantiate_trait(Trait trait, SpecializeMapping& mapping) {
        return new_unifiable(new TraitInstanceNode(trait, mapping)).node();
    }

    TypetableSet<Generic> unifiables_;
    std::vector<Generic*> garbage_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"
    Trait trait_error_;
    TypeError type_error_;
    NoReturnType type_noreturn_;

    friend class TypeVarNode;
    friend class TraitNode;
    friend class TraitInstanceNode;
};

}

#endif
