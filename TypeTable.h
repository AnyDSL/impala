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

struct TypeTraitInstanceHash { size_t operator () (const TypeTraitInstance* t) const { return t->hash(); } };
struct TypeTraitInstanceEqual { bool operator () (const TypeTraitInstance* t1, const TypeTraitInstance* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<TypeTraitInstance*, TypeTraitInstanceHash, TypeTraitInstanceEqual> TraitInstanceTableSet;

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<Type*, TypeHash, TypeEqual> TypeSet;

class TypeTable {
public:
    TypeTable();
    // TODO also delete traits & trait instances
    ~TypeTable() { for (auto type : types_) delete type; }

    const TypeError* type_error() { return type_error_; }
    const PrimType* primtype(PrimTypeKind kind);

#define PRIMTYPE(T) PrimType* type_##T() { return T##_; }
#include "primtypes.h"

    const TypeTrait* top_trait() const { return top_trait_; }
    TypeTraitInstance* top_trait_inst() const { return top_trait_inst_; }

    // TODO maybe seperate traits completely from the TypeTable
    TypeTrait* typetrait(std::string name, TypeTraitSet super_traits) {
        return new TypeTrait(*this, name, super_traits);
    }
    TypeTrait* typetrait(std::string name) { return typetrait(name, {top_trait_}); }

    TypeTraitInstance* instantiate_trait(const TypeTrait* trait, TypeArray var_instances) {
        return new TypeTraitInstance(trait, var_instances);
    }

    TypeVar* typevar() { return new TypeVar(*this); }

    FnType* fntype(TypeArray params) { return new FnType(*this, params); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    FnType* fntype_simple(TypeArray params, Type* return_type);

    // TODO review this
    //const TupleType* tupletype(TypeArray elems) { return unify_new(new TupleType(*this, elems)); }

    /**
     * Checks if all types in the type tables are sane and correctly unified.
     */
    void check_sanity() const;

    template<class T> T* unify(T* type) { return unify_base(type)->template as<T>(); }
    //const TypeTraitInstance* unify_trait_inst(TypeTraitInstance* type);

private:
    /// insert all not-unified types contained in type
    void insert_new(Type* type);
    void insert_new(TypeTraitInstance* tti);

    /**
     * Recursivly change the representatives of the not-unified types in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    template<class T> void change_repr(T* t, const T* repr) const;
    void change_repr_rec(Type* t, const Type* repr) const;
    void change_repr_rec(TypeTraitInstance* t, const TypeTraitInstance* repr) const;

    Type* unify_base(Type* type);
    TypeTraitInstance* unify_base(TypeTraitInstance* trait_inst);

    /// like unify but deletes the given type if unification returned a different one
    template<class T> T* unify_new(T* type) {
        T* unified_type = unify(type);
        if (unified_type != type)
            delete type;
        return unified_type;
    }

    TypeSet types_;
    //TraitTableSet traits_;
    TraitInstanceTableSet trait_instances_;

#define PRIMTYPE(T) PrimType* T##_;
#include "primtypes.h"
    const TypeError* type_error_;
    const TypeTrait* top_trait_;
    TypeTraitInstance* top_trait_inst_;
};


#endif /* TYPETABLE_H_ */
