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

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

struct TypeTraitHash { size_t operator () (const TypeTrait* t) const { return t->hash(); } };
struct TypeTraitEqual { bool operator () (const TypeTrait* t1, const TypeTrait* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const TypeTrait*, TypeTraitHash, TypeTraitEqual> TraitTableSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable() { for (auto type : types_) delete type; }

    const TypeError* type_error() { return type_error_; }
    const PrimType* primtype(PrimTypeKind kind);

#define PRIMTYPE(T) const PrimType* type_##T() { return T##_; }
#include "primtypes.h"

    const TypeTrait* typetrait(std::string name, TypeTraitSet super_traits) {
        return unify_trait(new TypeTrait(*this, name, super_traits));
    }
    const TypeTrait* typetrait(std::string name) { return typetrait(name, {top_trait_}); }

    const TypeVar* typevar(TypeTraitSet restriction) { return new TypeVar(*this, restriction); }
    const TypeVar* typevar() { return typevar({top_trait_}); }

    const FnType* fntype(TypeArray params) { return unify_new(new FnType(*this, params)); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    const FnType* fntype_simple(TypeArray params, const Type* return_type);

    /**
     * Create a generic type given the quantified type variables and the type
     * using them.
     *
     * Example: create 'fn<A>(A)'
     * @code{.cpp}
     * TypeVarRef* A = typevar();
     * gentype({A}, fntype({A}));
     * @endcode
     */
    template<class T> const T* gentype(TypeVarArray tvars, const T* type) { return gentype_base(tvars, type)->template as<const T>(); }

    const TupleType* tupletype(TypeArray elems) { return unify_new(new TupleType(*this, elems)); }

    /**
     * Checks if all types in the type tables are sane and correctly unified.
     */
    void check_sanity() const;

private:
    const Type* gentype_base(TypeVarArray tvars, const Type* type);

    /// insert all not-unified types contained in type
    void insert_new(const Type* type);

    /**
     * Recursivly change the representatives of the not-unified types in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    void change_repr(const Type* t, const Type* repr) const;

    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<const T>(); }

    /// like unify but deletes the given type if unification returned a different one
    template<class T> const T* unify_new(const T* type) {
        const T* unified_type = unify(type);
        if (unified_type != type)
            delete type;
        return unified_type;
    }

    const TypeTrait* unify_trait(const TypeTrait* type);

    TypeSet types_;
    TraitTableSet traits_;

#define PRIMTYPE(T) const PrimType* T##_;
#include "primtypes.h"
    const TypeError* type_error_;
    const TypeTrait* top_trait_;
};


#endif /* TYPETABLE_H_ */
