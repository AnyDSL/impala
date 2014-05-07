#ifndef IMPALA_SEMA_TRAIT_H
#define IMPALA_SEMA_TRAIT_H

#include "thorin/util/hash.h"

#include "impala/symbol.h"
#include "impala/sema/unifiable.h"

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
 * @see TraitInstanceNode
 */
class TraitNode : public TUnifiable<TraitNode> {
protected:
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl);

private:
    TraitNode& operator = (const TraitNode&); ///< Do not copy-assign a \p TraitNode.
    TraitNode(const TraitNode& node);         ///< Do not copy-construct a \p TraitNode.

public:
    virtual bool equal(const Unifiable* other) const;
    virtual size_t hash() const;
    const TraitDecl* trait_decl() const { return trait_decl_; }
    bool is_error_trait() const { return trait_decl_ == nullptr; }
    virtual std::string to_string() const;
    bool add_super_trait(Trait);
    const UniSet<Trait>& super_traits() const { return super_traits_; }

    // all methods should be known, so nothing to do here
    virtual void refine() {}
    virtual bool is_known() const override { return true; }

    virtual bool unify_with(Unifiable*) { assert(false); return false; }

    /// return the type of the method with this name if it exists; otherwise return an empty type
    virtual Type find_method(Symbol name);
    bool has_method(Symbol name) { return !find_method(name).empty(); }

    virtual bool is_closed() const { return true; } // TODO

protected:
    Unifiable* vspecialize(SpecializeMap&);

    const TraitDecl* const trait_decl_;
    UniSet<Trait> super_traits_;

    friend class TypeTable;
    friend class TraitInstanceNode;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TraitInstanceNode : public TraitNode {
private:
    TraitInstanceNode(const Trait trait, const SpecializeMap& var_instances);
    TraitInstanceNode& operator = (const TraitInstanceNode&); ///< Do not copy-assign a \p TraitInstanceNode.
    TraitInstanceNode(const TraitInstanceNode& node);         ///< Do not copy-construct a \p TraitInstanceNode.

public:
    virtual bool equal(const Unifiable* other) const;
    virtual size_t hash() const;
    virtual std::string to_string() const;
    virtual void refine();
    virtual bool is_known() const override;
    virtual bool unify_with(Unifiable*);
    virtual Type find_method(Symbol name);
    virtual bool is_closed() const;

protected:
    Unifiable* vspecialize(SpecializeMap&);

private:
    const Trait trait() const { return trait_; }
    /// return a copy of the variable instances
    SpecializeMap var_instances() const { return var_instances_; }

    const Trait trait_;
    SpecializeMap var_instances_;

    friend class TypeVarNode;
    friend class Generic;
    friend class TypeTable;
};

class TraitImplNode : public TUnifiable<TraitImplNode> {
private:
    TraitImplNode(TypeTable& tt, const Impl* impl_decl, Trait trait)
        : TUnifiable(tt)
        , impl_decl_(impl_decl)
        , trait_(trait)
    {}
    TraitImplNode& operator = (const TraitImplNode&); ///< Do not copy-assign a \p TraitImpl.
    TraitImplNode(const TraitImplNode&);              ///< Do not copy-construct a \p TraitImpl.

public:
    virtual bool equal(const Unifiable* other) const { return this->impl_decl() == other->as<TraitImplNode>()->impl_decl(); }
    virtual size_t hash() const;
    const Impl* impl_decl() const { return impl_decl_; }
    Trait trait() const { return trait_; }

    // CHECK is this correct?
    virtual void refine() {}
    virtual bool is_known() const override { return true; }

    virtual bool unify_with(Unifiable*) { assert(false); return false; }

    virtual bool is_closed() const { return true; } // TODO

protected:
    Unifiable* vspecialize(SpecializeMap& m);

    virtual std::string to_string() const { return ""; } // TODO

private:
    const Impl* const impl_decl_;
    Trait trait_;

    friend class TypeTable;
};

}

#endif
