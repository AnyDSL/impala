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
class Impl;

struct TraitMethod {
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
 * @see TraitInstance
 */
class Trait : public GenericElement {
private:
    Trait(TypeTable& tt, const TraitDecl* trait_decl, const TraitSet super_traits)
        : GenericElement(tt)
        , trait_decl_(trait_decl)
        , super_traits_(super_traits)
    {}
    Trait& operator = (const Trait&); ///< Do not copy-assign a \p Trait.
    Trait(const Trait& node);         ///< Do not copy-construct a \p Trait.

public:
    virtual bool equal(const GenericElement* t) const;
    bool equal(const Trait* other) const { return this->trait_decl() == other->trait_decl(); }
    size_t hash() const { return thorin::hash_value(trait_decl()); }
    const TraitDecl* trait_decl() const { return trait_decl_; }
    std::string to_string() const;
    // TODO retrieve methods via trait_decl()->methods and remove this
    void add_method(const std::string name, FnType type);

private:
    const TraitDecl* const trait_decl_;
    const TraitSet super_traits_;
    std::vector<const TraitMethod*> methods_;

    friend class TypeTable;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TraitInstanceNode : public thorin::MagicCast<TraitInstanceNode> {
private:
    TraitInstanceNode(const Trait* trait, thorin::ArrayRef<Type> var_instances);
    TraitInstanceNode& operator = (const TraitInstanceNode&); ///< Do not copy-assign a \p TraitInstance.
    TraitInstanceNode(const TraitInstanceNode& node);         ///< Do not copy-construct a \p TraitInstance.

    Type var_inst_(size_t i) const { return var_instances_[i]; }

public:
    const Trait* trait() const { return trait_; }
    TypeTable& typetable() const { return trait()->typetable(); }
    bool equal(TraitInstance t) const { return equal(t.representative()); }
    bool equal(const TraitInstanceNode* t) const;
    size_t hash() const;

    thorin::ArrayRef<Type> var_instances() const { return thorin::ArrayRef<Type>(var_instances_); }
    const Type var_inst(size_t i) const { return var_instances_[i]; }
    /// Returns number of variables instances.
    size_t var_inst_size() const { return var_instances_.size(); }

    bool is_closed() const;
    std::string to_string() const;

private:
    const Trait* trait_;
    std::vector<Type> var_instances_;

    TraitInstance specialize(SpecializeMapping&) const;

    friend class TypeVarNode;
    friend class TypeTable;
};

class TraitImpl : public GenericElement {
    TraitImpl(TypeTable& tt, const Impl* impl_decl, TraitInstance trait)
        : GenericElement(tt)
        , impl_decl_(impl_decl)
        , trait_(trait)
    {}
    TraitImpl& operator = (const TraitImpl&); ///< Do not copy-assign a \p TraitImpl.
    TraitImpl(const TraitImpl&);              ///< Do not copy-construct a \p TraitImpl.

public:
    virtual bool equal(const GenericElement* t) const;
    bool equal(const TraitImpl* other) const { return this->impl_decl() == other->impl_decl(); }
    size_t hash() const { return thorin::hash_value(impl_decl()); }
    const Impl* impl_decl() const { return impl_decl_; }
    TraitInstance trait_inst() const { return trait_; }

private:
    const Impl* const impl_decl_;
    TraitInstance trait_;

    friend class TypeTable;
};

struct TraitInstanceNodeHash { 
    size_t operator () (const TraitInstanceNode* t) const { return t->hash(); } 
};

struct TraitInstanceNodeEqual { 
    bool operator () (const TraitInstanceNode* t1, const TraitInstanceNode* t2) const { return t1->equal(t2); } 
};

typedef std::unordered_set<TraitInstanceNode*, TraitInstanceNodeHash, TraitInstanceNodeEqual> TraitInstanceNodeTableSet;

}

#endif
