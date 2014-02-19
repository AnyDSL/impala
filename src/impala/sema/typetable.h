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

    void add(Unifiable<TraitInstanceNode>* t) { trait_instances_.push_back(t); }
    void add(Unifiable<TypeNode>* t) { types_.push_back(t); }
    void add(const Trait* t) { traits_.push_back(t); }

private:
    std::vector<Unifiable<TraitInstanceNode>*> trait_instances_;
    std::vector<Unifiable<TypeNode>*> types_;
    std::vector<const Trait*> traits_;

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
        auto tti = TraitInstance(new TraitInstanceNode(trait, var_instances));
        unifiables_.add(tti.node_);
        return tti;
    }
    TypeVar typevar() { return new_type(new TypeVarNode(*this)); }
    FnType fntype(thorin::ArrayRef<Type> params) { return new_type(new FnTypeNode(*this, params)); }
    FnType fntype(thorin::ArrayRef<Type> params, Type return_type);
    void unify(TraitInstance tti);
    template<class T> void unify(UnifiableProxy<T> type) { unify_base(type); }
    //const TraitInstance* unify_trait_inst(TraitInstance* type);
    /// Checks if all types in the type tables are sane and correctly unified.
    void verify() const;

private:
    template<class T> UnifiableProxy<T> new_type(T* tn) {
        auto t = UnifiableProxy<T>(tn);
        UnifiableProxy<TypeNode> x = t;  // CHECK is this really memory safe?
        unifiables_.add(x.node_);
        return t;
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
    template<class T> void change_repr(UnifiableProxy<T> t, T* repr) const;
    template<class T> void change_repr_rec(UnifiableProxy<T> t, T* repr) const;
    void change_repr_rec(TraitInstance t, TraitInstanceNode* repr) const;
    void unify_base(Type type);

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
