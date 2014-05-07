#ifndef IMPALA_SEMA_TRAIT_H
#define IMPALA_SEMA_TRAIT_H

#include "thorin/util/hash.h"

#include "impala/symbol.h"
#include "impala/sema/unifiable.h"

namespace impala {

class FnTypeNode;
class ImplItem;
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
 * @see BoundNode
 */
class TraitNode : public Unifiable {
private:
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl);

public:
    const TraitDecl* trait_decl() const { return trait_decl_; }
    const UniSet<Bound>& super_bounds() const { return super_bounds_; }
    bool is_error_trait() const { return trait_decl_ == nullptr; }
    bool add_super_bound(Bound) const;
    /// return the type of the method with this name if it exists; otherwise return an empty type
    Type find_method(Symbol name) const;
    bool has_method(Symbol name) const { return !find_method(name).empty(); }
    Bound instantiate(thorin::ArrayRef<Type> args);

    virtual void refine() const override {} // all methods should be known, so nothing to do here
    virtual bool equal(const Unifiable* other) const;
    virtual size_t hash() const;
    virtual bool is_known() const override { return true; }
    virtual bool unify_with(const Unifiable*) const override { assert(false); return false; }
    virtual bool is_closed() const { return true; } // TODO
    virtual std::string to_string() const;

protected:
    const TraitDecl* const trait_decl_;
    mutable UniSet<Bound> super_bounds_;

    friend class TypeTable;
    friend class BoundNode;
};

/// An instance of a trait is a trait where all type variables are instantiated by concrete types.
class BoundNode : public Unifiable {
private:
    BoundNode(const Trait trait, thorin::ArrayRef<Type> args);

public:
    const Trait trait() const { return trait_; }
    const Type arg(size_t i) const { return args_[i]; }
    thorin::ArrayRef<Type> args() const { return args_; }
    size_t num_args() const { return args_.size(); }
    Type find_method(Symbol name) const;
    virtual bool equal(const Unifiable* other) const override;
    virtual size_t hash() const override;
    virtual std::string to_string() const;
    virtual void refine() const override;
    virtual bool is_known() const override;
    virtual bool unify_with(const Unifiable*) const override;
    virtual bool is_closed() const;

private:
    const Trait trait_;
    thorin::Array<Type> args_;

    friend class TypeVarNode;
    friend class Generic;
    friend class TypeTable;
};

class ImplNode : public Unifiable {
private:
    ImplNode(TypeTable& tt, const ImplItem* impl_item, Bound bound)
        : Unifiable(tt)
        , impl_item_(impl_item)
        , bound_(bound)
    {}

public:
    virtual bool equal(const Unifiable* other) const { return this->impl_item() == other->as<ImplNode>()->impl_item(); }
    virtual size_t hash() const;
    const ImplItem* impl_item() const { return impl_item_; }
    Bound bound() const { return bound_; }

    // CHECK is this correct?
    virtual void refine() const override {}
    virtual bool is_known() const override { return true; }
    virtual bool unify_with(const Unifiable*) const override { assert(false); return false; }
    virtual bool is_closed() const { return true; } // TODO

protected:
    virtual std::string to_string() const { return ""; } // TODO

private:
    const ImplItem* const impl_item_;
    Bound bound_;

    friend class TypeTable;
};

}

#endif
