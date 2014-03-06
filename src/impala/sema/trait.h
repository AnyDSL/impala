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
protected:
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
        : Unifiable(tt)
        , trait_decl_(trait_decl)
    {}

private:
    TraitNode& operator = (const TraitNode&); ///< Do not copy-assign a \p Trait.
    TraitNode(const TraitNode& node);         ///< Do not copy-construct a \p Trait.

public:
    virtual bool equal(const TraitNode* other) const { return this->trait_decl() == other->trait_decl(); }
    virtual size_t hash() const;
    const TraitDecl* trait_decl() const { return trait_decl_; }
    virtual std::string to_string() const;

    virtual bool is_closed() const { return true; } // TODO

protected:
    /// copy this trait but replace the sub-elements given in the mapping
    TraitNode* vspecialize(SpecializeMapping&);

private:
    const TraitDecl* const trait_decl_;

    friend class TypeTable;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TraitInstanceNode : public TraitNode {
private:
    TraitInstanceNode(const Trait trait, SpecializeMapping var_instances)
        : TraitNode(trait->typetable(), trait->trait_decl())
        , trait_(trait)
        , var_instances_(var_instances)
    {}
    TraitInstanceNode& operator = (const TraitInstanceNode&); ///< Do not copy-assign a \p TraitInstance.
    TraitInstanceNode(const TraitInstanceNode& node);         ///< Do not copy-construct a \p TraitInstance.

public:
    virtual bool equal(const TraitNode* other) const;
    virtual size_t hash() const;
    virtual std::string to_string() const;

    virtual bool is_closed() const;

protected:
    /// copy this trait but replace the sub-elements given in the mapping
    TraitNode* vspecialize(SpecializeMapping&);

private:
    const Trait trait() const { return trait_; }
    SpecializeMapping var_instances() const { return var_instances_; }

    const Trait trait_;
    SpecializeMapping var_instances_;

    friend class TypeVarNode;
    friend class TypeTable;
};

class TraitImplNode : public Unifiable<TraitImplNode> {
    TraitImplNode(TypeTable& tt, const Impl* impl_decl, Trait trait)
        : Unifiable(tt)
        , impl_decl_(impl_decl)
        , trait_(trait)
    {}
    TraitImplNode& operator = (const TraitImplNode&); ///< Do not copy-assign a \p TraitImpl.
    TraitImplNode(const TraitImplNode&);              ///< Do not copy-construct a \p TraitImpl.

public:
    virtual bool equal(const TraitImplNode* other) const { return this->impl_decl() == other->impl_decl(); }
    virtual size_t hash() const;
    const Impl* impl_decl() const { return impl_decl_; }
    Trait trait() const { return trait_; }

    virtual bool is_closed() const { return true; } // TODO

protected:
    /// copy this \p TraitImplNode but replace the sub-elements given in the mapping
    TraitImplNode* vspecialize(SpecializeMapping&) { return new TraitImplNode(typetable(), impl_decl(), trait()); } // TODO specialization

    virtual std::string to_string() const { return ""; } // TODO

private:
    const Impl* const impl_decl_;
    Trait trait_;

    friend class TypeTable;
};

}

#endif
