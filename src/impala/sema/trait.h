/*
 * trait.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef IMPALA_SEMA_TRAIT_H
#define IMPALA_SEMA_TRAIT_H

#include "impala/sema/type.h"

namespace impala {

class TraitDecl;

struct TypeTraitMethod {
    std::string name;
    FnType type;
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
 * @see TypeTraitInstance
 */
class TypeTrait : public GenericElement {
private:
    TypeTrait(TypeTable& tt, const TraitDecl* trait_decl, const TypeTraitSet super_traits)
        : typetable_(tt)
        , trait_decl_(trait_decl)
        , super_traits_(super_traits)
    {}
    TypeTrait& operator = (const TypeTrait&); ///< Do not copy-assign a \p TypeTrait.
    TypeTrait(const TypeTrait& node);         ///< Do not copy-construct a \p TypeTrait.

public:
    TypeTable& typetable() const { return typetable_; }
    virtual bool equal(const GenericElement* t) const;
    bool equal(const TypeTrait* other) const { return this->trait_decl() == other->trait_decl(); }
    size_t hash() const { return thorin::hash_value(trait_decl()); }
    const TraitDecl* trait_decl() const { return trait_decl_; }
    std::string to_string() const;
    // TODO retrieve methods via trait_decl()->methods and remove this
    void add_method(const std::string name, FnType type);

private:
    TypeTable& typetable_;
    const TraitDecl* const trait_decl_;
    const TypeTraitSet super_traits_;
    std::vector<const TypeTraitMethod*> methods_;
    static const std::string top_trait_name;

    friend class TypeTable;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TypeTraitInstanceNode : public thorin::MagicCast<TypeTraitInstanceNode> {
private:
    TypeTraitInstanceNode(const TypeTrait* trait, thorin::ArrayRef<Type> var_instances);
    TypeTraitInstanceNode& operator = (const TypeTraitInstanceNode&); ///< Do not copy-assign a \p TypeTraitInstance.
    TypeTraitInstanceNode(const TypeTraitInstanceNode& node);         ///< Do not copy-construct a \p TypeTraitInstance.

    Type var_inst_(size_t i) const { return var_instances_[i]; }

public:
    const TypeTrait* trait() const { return trait_; }
    TypeTable& typetable() const { return trait()->typetable(); }
    bool equal(TypeTraitInstance t) const { return equal(t.representative()); }
    bool equal(const TypeTraitInstanceNode* t) const;
    size_t hash() const;
    const Type var_inst(size_t i) const { return var_instances_[i]; }
    /// Returns number of variables instances.
    size_t var_inst_size() const { return var_instances_.size(); }
    bool is_closed() const;
    std::string to_string() const;

private:
    const TypeTrait* trait_;
    std::vector<Type> var_instances_;

    friend class TypeTable;
};

struct TypeTraitInstanceNodeHash { 
    size_t operator () (const TypeTraitInstanceNode* t) const { return t->hash(); } 
};

struct TypeTraitInstanceNodeEqual { 
    bool operator () (const TypeTraitInstanceNode* t1, const TypeTraitInstanceNode* t2) const { return t1->equal(t2); } 
};

typedef std::unordered_set<TypeTraitInstanceNode*, TypeTraitInstanceNodeHash, TypeTraitInstanceNodeEqual> TraitInstanceNodeTableSet;

}

#endif
