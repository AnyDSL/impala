/*
 * trait.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef TRAIT_H_
#define TRAIT_H_

#include "type.h"

class TypeTrait;
class TypeTable;

typedef std::unordered_set<const TypeTrait*> TypeTraitSet;

struct TypeTraitMethod {
    std::string name;
    const FnType* type;
};

/**
 * Represents a declared trait.
 * A trait consists of a name, a number of declared methods and a number of
 * super traits. Also, it may be generic in a number of type variables that
 * can be restricted by any number of instantiated traits.
 *
 * The restrictions for the traits must not include the newly declared trait
 * itself. Otherwise things get complicated, e.g. the following would be
 * allowed (I guess):
 * @code trait TT<X:TT<Self>> {}; impl TT<int> for int {} @endcode
 *
 *
 * @see TypeTraitInstance
 */
class TypeTrait {
private:
    /// create the global top type trait (like Object in java)
    TypeTrait(TypeTable& tt)
        : typetable_(tt)
        , name_(top_trait_name)
        , super_traits_()
        , bound_vars_()
    {}

    TypeTrait(TypeTable& tt, std::string name, const TypeTraitSet super_traits,
            const TypeVarArray bound_vars)
        : typetable_(tt)
        , name_(name)
        , super_traits_(super_traits)
        , bound_vars_(bound_vars)
    {}

    TypeTrait& operator = (const TypeTrait&); ///< Do not copy-assign a \p TypeTrait.
    TypeTrait(const TypeTrait& node);         ///< Do not copy-construct a \p TypeTrait.

    TypeTable& typetable_;
    std::string name_;
    const TypeTraitSet super_traits_;
    const TypeVarArray bound_vars_;
    mutable std::vector<const TypeTraitMethod*> methods_;

    static const std::string top_trait_name;

    void addMethod(const std::string name, const FnType* type) const {
        TypeTraitMethod* m = new TypeTraitMethod();
        m->name = name;
        m->type = type;
        methods_.push_back(m);
    }

public:
    bool equal(const TypeTrait* t) const;
    size_t hash() const;

    std::string to_string() const;

    /// true if this is the top type trait (like Object in java)
    bool is_top_trait() const {
        assert(super_traits_.size() != 0 || name_.compare(top_trait_name) == 0);
        return super_traits_.size() == 0;
    }

    friend class TypeTable;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TypeTraitInstance {
private:
    /// create the global top type trait (like Object in java)
    TypeTraitInstance(const TypeTrait* trait, TypeArray var_instances)
        : trait_(trait)
        , var_instances_(var_instances)
    {}

    TypeTraitInstance& operator = (const TypeTraitInstance&); ///< Do not copy-assign a \p TypeTraitInstance.
    TypeTraitInstance(const TypeTraitInstance& node);         ///< Do not copy-construct a \p TypeTraitInstance.

    const TypeTrait* trait_;
    TypeArray var_instances_;

public:
    bool equal(const TypeTraitInstance* t) const;
    size_t hash() const;

    std::string to_string() const;

    /// true if this is an instance of the top type trait (like Object in Java)
    bool is_top_trait() const { return trait_->is_top_trait(); }

    friend class TypeTable;
};

#endif /* TRAIT_H_ */
