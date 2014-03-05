/*
 * trait.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef IMPALA_SEMA_TRAIT_H
#define IMPALA_SEMA_TRAIT_H

#include <unordered_map>
#include <unordered_set>

#include "impala/sema/generic.h"

namespace impala {

class FnTypeNode;
class Impl;
class TraitDecl;
typedef Proxy<FnTypeNode> FnType;
typedef std::unordered_map<const TypeNode*, Type> SpecializeMapping; // FIXME

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
class TraitNode : public Unifiable<TraitNode> {
private:
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
        : Unifiable(tt)
        , trait_decl_(trait_decl)
    {}
    TraitNode& operator = (const TraitNode&); ///< Do not copy-assign a \p Trait.
    TraitNode(const TraitNode& node);         ///< Do not copy-construct a \p Trait.

public:
    virtual bool equal(const Generic* t) const;
    bool equal(const TraitNode* other) const { return this->trait_decl() == other->trait_decl(); }
    size_t hash() const { return thorin::hash_value(trait_decl()); }
    const TraitDecl* trait_decl() const { return trait_decl_; }
    std::string to_string() const;

    virtual bool is_closed() const { return true; } // TODO

    /// copy this trait but replace the sub-elements given in the mapping
    Trait specialize(SpecializeMapping&) const { return Trait(); } // FIXME

private:
    const TraitDecl* const trait_decl_;

    friend class TypeTable;
};

#if 0
/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TraitInstanceNode : public Unifiable<TraitInstanceNode> {
private:
    TraitInstanceNode(TypeTable&, const Trait* trait, thorin::ArrayRef<Type> var_instances);
    TraitInstanceNode& operator = (const TraitInstanceNode&); ///< Do not copy-assign a \p TraitInstance.
    TraitInstanceNode(const TraitInstanceNode& node);         ///< Do not copy-construct a \p TraitInstance.

    Type var_inst_(size_t i) const { return var_instances_[i]; }

public:
    const Trait* trait() const { return trait_; }
    TypeTable& typetable() const { return trait()->typetable(); }
    bool equal(TraitInstance t) const { return equal(t); }
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
#endif

class TraitImplNode : public Unifiable<TraitImplNode> {
    TraitImplNode(TypeTable& tt, const Impl* impl_decl, Trait trait)
        : Unifiable(tt)
        , impl_decl_(impl_decl)
        , trait_(trait)
    {}
    TraitImplNode& operator = (const TraitImplNode&); ///< Do not copy-assign a \p TraitImpl.
    TraitImplNode(const TraitImplNode&);              ///< Do not copy-construct a \p TraitImpl.

public:
    virtual bool equal(const Generic* t) const;
    bool equal(const TraitImplNode* other) const { return this->impl_decl() == other->impl_decl(); }
    size_t hash() const { return thorin::hash_value(impl_decl()); }
    const Impl* impl_decl() const { return impl_decl_; }
    Trait trait() const { return trait_; }

    virtual bool is_closed() const { return true; } // TODO

private:
    const Impl* const impl_decl_;
    Trait trait_;

    friend class TypeTable;
};

}

#endif
