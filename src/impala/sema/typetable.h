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

//------------------------------------------------------------------------------

class UnifiableSet {
public:
    UnifiableSet() {}
    ~UnifiableSet();

    void add(Unifiable<TypeTraitInstanceNode>* t) { trait_instances_.push_back(t); }
    void add(Unifiable<TypeNode>* t) { types_.push_back(t); }
    void add(const TypeTrait* t) { traits_.push_back(t); }

private:
    std::vector<Unifiable<TypeTraitInstanceNode>*> trait_instances_;
    std::vector<Unifiable<TypeNode>*> types_;
    std::vector<const TypeTrait*> traits_;

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
    const TypeTrait* top_trait() const { return top_trait_; }
    TypeTraitInstance top_trait_inst() const { return top_trait_inst_; }
    TypeTrait* typetrait(std::string name, TypeTraitSet super_traits) {
        auto t = new TypeTrait(*this, name, super_traits);
        unifiables_.add(t);
        return t;
    }
    TypeTrait* typetrait(std::string name) { return typetrait(name, {top_trait_}); }
    TypeTraitInstance instantiate_trait(const TypeTrait* trait, thorin::ArrayRef<Type> var_instances) {
        auto tti = TypeTraitInstance(new TypeTraitInstanceNode(trait, var_instances));
        unifiables_.add(tti.node_);
        return tti;
    }
    TypeVar typevar() { return new_type(new TypeVarNode(*this)); }
    FnType fntype(thorin::ArrayRef<Type> params) { return new_type(new FnTypeNode(*this, params)); }
    FnType fntype(thorin::ArrayRef<Type> params, Type return_type);
    void unify(TypeTraitInstance tti);
    template<class T> void unify(UnifiableProxy<T> type) { unify_base(type); }
    //const TypeTraitInstance* unify_trait_inst(TypeTraitInstance* type);
    /// Checks if all types in the type tables are sane and correctly unified.
    void verify() const;

private:
    template<class T> UnifiableProxy<T> new_type(T* tn) {
        auto t = UnifiableProxy<T>(tn);
        UnifiableProxy<TypeNode> x = t;  // TODO is this really memory safe?
        unifiables_.add(x.node_);
        return t;
    }

    /// insert all not-unified types contained in type
    void insert_new(Type type);
    void insert_new(TypeTraitInstance tti);

    /**
     * Recursivly change the representatives of the not-unified types in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    template<class T> void change_repr(UnifiableProxy<T> t, T* repr) const;
    template<class T> void change_repr_rec(UnifiableProxy<T> t, T* repr) const;
    void change_repr_rec(TypeTraitInstance t, TypeTraitInstanceNode* repr) const;
    void unify_base(Type type);

    // TODO still needed?
    /// like unify but deletes the given type if unification returned a different one
    /*template<class T>  unify_new(T* type) {
        T* unified_type = unify(type);
        if (unified_type != type)
            delete type;
        return unified_type;
    }*/

    TypeNodeSet types_;
    TraitInstanceNodeTableSet trait_instances_;
    UnifiableSet unifiables_;
#define IMPALA_TYPE(itype, atype) PrimType itype##_;
#include "impala/tokenlist.h"
    TypeError type_error_;
    const TypeTrait* top_trait_;
    TypeTraitInstance top_trait_inst_;
};

}

#endif
