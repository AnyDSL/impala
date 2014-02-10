/*
 * TypeTable.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef TYPETABLE_H_
#define TYPETABLE_H_

#include "type.h"
#include "trait.h"

//struct TypeTraitHash { size_t operator () (const TypeTrait* t) const { return t->hash(); } };
//struct TypeTraitEqual { bool operator () (const TypeTrait* t1, const TypeTrait* t2) const { return t1->equal(t2); } };
//typedef std::unordered_set<TypeTrait*, TypeTraitHash, TypeTraitEqual> TraitTableSet;

struct TypeTraitInstanceNodeHash { size_t operator () (const TypeTraitInstanceNode* t) const { return t->hash(); } };
struct TypeTraitInstanceNodeEqual { bool operator () (const TypeTraitInstanceNode* t1, const TypeTraitInstanceNode* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<TypeTraitInstanceNode*, TypeTraitInstanceNodeHash, TypeTraitInstanceNodeEqual> TraitInstanceNodeTableSet;

struct TypeNodeHash { size_t operator () (const TypeNode* t) const { return t->hash(); } };
struct TypeNodeEqual { bool operator () (const TypeNode* t1, const TypeNode* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<TypeNode*, TypeNodeHash, TypeNodeEqual> TypeNodeSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

    TypeError type_error() { return type_error_; }
    PrimType primtype(PrimTypeKind kind);

#define PRIMTYPE(T) PrimType type_##T() { return T##_; }
#include "primtypes.h"

    const TypeTrait* top_trait() const { return top_trait_; }
    // TODO store the proxy
    TypeTraitInstance top_trait_inst() const { return top_trait_inst_; }

    // TODO maybe seperate traits completely from the TypeTable
    TypeTrait* typetrait(std::string name, TypeTraitSet super_traits) {
        return new TypeTrait(*this, name, super_traits);
    }
    TypeTrait* typetrait(std::string name) { return typetrait(name, {top_trait_}); }

    TypeTraitInstance instantiate_trait(const TypeTrait* trait, thorin::ArrayRef<Type> var_instances) {
        return TypeTraitInstance(new TypeTraitInstanceNode(trait, var_instances));
    }

    TypeVarNode* typevar() { return new TypeVarNode(*this); }

    FnType fntype(thorin::ArrayRef<Type> params) { return FnType(new FnTypeNode(*this, params)); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    FnType fntype_simple(thorin::ArrayRef<Type> params, Type return_type);

    // TODO review this
    //const TupleType* tupletype(thorin::ArrayRef<Type> elems) { return unify_new(new TupleType(*this, elems)); }

    /**
     * Checks if all types in the type tables are sane and correctly unified.
     */
    void check_sanity() const;

    void unify(TypeTraitInstance tti);
    template<class T> void unify(UnifiableProxy<T> type) { unify_base(type); }

    //const TypeTraitInstance* unify_trait_inst(TypeTraitInstance* type);

private:
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

    /// like unify but deletes the given type if unification returned a different one
    /*template<class T>  unify_new(T* type) {
        T* unified_type = unify(type);
        if (unified_type != type)
            delete type;
        return unified_type;
    }*/

    TypeNodeSet types_;
    //TraitTableSet traits_;
    TraitInstanceNodeTableSet trait_instances_;

#define PRIMTYPE(T) PrimType T##_;
#include "primtypes.h"
    TypeError type_error_;
    const TypeTrait* top_trait_;
    TypeTraitInstance top_trait_inst_;
};


#endif /* TYPETABLE_H_ */
