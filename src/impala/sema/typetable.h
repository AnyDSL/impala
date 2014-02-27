/*
 * TypeTable.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef IMPALA_SEMA_TYPETABLE_H
#define IMPALA_SEMA_TYPETABLE_H

#include "impala/sema/type.h"
#include "impala/sema/trait.h"

namespace impala {

class TraitDecl;

//------------------------------------------------------------------------------

class UnifiableSet {
public:
    UnifiableSet() {}
    ~UnifiableSet();

    void add(const TraitInstanceNode* t) { trait_instances_.push_back(t); }
    void add(const TypeNode* t) { types_.push_back(t); }
    void add(const Trait* t) { traits_.push_back(t); }
    void add(const TraitImpl* impl) { trait_impls_.push_back(impl); }

private:
    std::vector<const TraitInstanceNode*> trait_instances_;
    std::vector<const TypeNode*> types_;
    std::vector<const Trait*> traits_;
    std::vector<const TraitImpl*> trait_impls_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

    TypeError type_error() { return type_error_; }
    PrimType primtype(PrimTypeKind kind);
#define IMPALA_TYPE(itype, atype) PrimType type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    Trait* trait(const TraitDecl* trait_decl, TraitSet super_traits) {
        auto t = new Trait(*this, trait_decl, super_traits);
        unifiables_.add(t);
        return t;
    }
    TraitInstance instantiate_trait(const Trait* trait, thorin::ArrayRef<Type> var_instances) {
        auto tti = new TraitInstanceNode(*this, trait, var_instances);
        unifiables_.add(tti);
        return tti;
    }
    TraitImpl* implement_trait(const Impl* impl_decl, TraitInstance trait) {
        auto impl = new TraitImpl(*this, impl_decl, trait);
        unifiables_.add(impl);
        return impl;
    }
    TypeVar typevar() { return new_type(new TypeVarNode(*this)); }
    FnType fntype(thorin::ArrayRef<Type> params) { return new_type(new FnTypeNode(*this, params)); }
    TupleType tupletype(thorin::ArrayRef<Type> elems) { return new_type(new TupleTypeNode(*this, elems)); }
    TupleType unit() { return tupletype({}); }

    /// unify a trait instance and return \p true if the representative changed
    template<class T> bool unify(Proxy<T> type) { return unify_base(type.node_); }
    //const TraitInstance* unify_trait_inst(TraitInstance* type);
    /// Checks if all types in the type tables are sane and correctly unified.
    void verify() const;

private:
    template<class T> 
    Proxy<T> new_type(T* tn) {
        unifiables_.add(tn);
        return Proxy<T>(tn);
    }

    /// insert all not-unified types contained in type
    void insert_new(Type type);
    void insert_new(TraitInstance tti);

    /**
     * Recursivly change the representatives of the not-unified types in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    template<class T> void change_repr(T* t, T* repr) const;
    template<class T> void change_repr_rec(T* t, T* repr) const;
    void change_repr_rec(TraitInstanceNode* t, TraitInstanceNode* repr) const;
    bool unify_base(TraitInstanceNode* trait);
    bool unify_base(TypeNode* type);

    TypeNodeSet types_;
    TraitInstanceNodeTableSet trait_instances_;
    UnifiableSet unifiables_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"
    TypeError type_error_;

    friend class TypeVarNode;
};

}

#endif
