#ifndef IMPALA_SEMA_TRAIT_H
#define IMPALA_SEMA_TRAIT_H

#include "impala/sema/generic.h"

#include "impala/symbol.h"
#include "thorin/util/hash.h"

namespace impala {

class FnTypeNode;
class Impl;
class TraitDecl;
typedef Proxy<FnTypeNode> FnType;

typedef thorin::HashMap<const Symbol, Type, thorin::Hash<Symbol>> MethodTable;

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
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl);

private:
    TraitNode& operator = (const TraitNode&); ///< Do not copy-assign a \p Trait.
    TraitNode(const TraitNode& node);         ///< Do not copy-construct a \p Trait.

public:
    virtual bool equal(const TraitNode* other) const;
    virtual size_t hash() const;
    const TraitDecl* trait_decl() const { return trait_decl_; }
    bool is_error_trait() const { return trait_decl_ == nullptr; }
    virtual std::string to_string() const;

    // all methods should be known, so nothing to do here
    virtual void refine() {}
    virtual bool is_known() const override { return true; }

    virtual bool unify_with(TraitNode*) { assert(false); return false; }

    /// add a non-inherited method or return \p false if a method with this name already existed
    bool add_method(Symbol name, Type method_type) { return add_method(name, method_type, false); }
    /// return the type of the method with this name if it exists; otherwise return an empty type
    virtual Type find_method(Symbol name);
    bool has_method(Symbol name) { return !find_method(name).empty(); }
    virtual const MethodTable& all_methods() { return all_methods_; }
    /// return the number of methods
    size_t num_methods() { return all_methods().size(); }

    void add_super_trait(Trait);
    virtual const UniSet<Trait>& super_traits() { return super_traits_; }
    virtual bool is_closed() const { return true; } // TODO

protected:
    /// copy this trait but replace the sub-elements given in the mapping
    TraitNode* vspecialize(SpecializeMapping&);

private:
    bool add_method(Symbol name, Type method_type, bool inherited);
    /// return the names of all methods that were declared in this trait (this does not count methods in super traits)
    virtual const thorin::ArrayRef<Symbol> declared_methods() { return declared_methods_; }

    const TraitDecl* const trait_decl_;
    UniSet<Trait> super_traits_;
    std::vector<Symbol> declared_methods_;
    MethodTable all_methods_;

    friend class TypeTable;
    friend class TraitInstanceNode;
};

/**
 * An instance of a trait is a trait where all generic type variables are
 * instantiated by concrete types.
 */
class TraitInstanceNode : public TraitNode {
private:
    TraitInstanceNode(const Trait trait, const SpecializeMapping& var_instances);
    TraitInstanceNode& operator = (const TraitInstanceNode&); ///< Do not copy-assign a \p TraitInstance.
    TraitInstanceNode(const TraitInstanceNode& node);         ///< Do not copy-construct a \p TraitInstance.

public:
    virtual bool equal(const TraitNode* other) const;
    virtual size_t hash() const;
    virtual std::string to_string() const;

    virtual void refine();
    virtual bool is_known() const override;

    virtual bool unify_with(TraitNode*);

    virtual Type find_method(Symbol name);
    virtual const MethodTable& all_methods();

    virtual const UniSet<Trait>& super_traits();

    virtual bool is_closed() const;

protected:
    /// copy this trait but replace the sub-elements given in the mapping
    TraitNode* vspecialize(SpecializeMapping&);

private:
    const Trait trait() const { return trait_; }
    /// return a copy of the variable instances
    SpecializeMapping var_instances() const { return var_instances_; }
    virtual const thorin::ArrayRef<Symbol> declared_methods() { return trait()->declared_methods(); }

    const Trait trait_;
    SpecializeMapping var_instances_;

    friend class TypeVarNode;
    friend class Generic;
    friend class TypeTable;
};

class TraitImplNode : public Unifiable<TraitImplNode> {
private:
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

    // CHECK is this correct?
    virtual void refine() {}
    virtual bool is_known() const override { return true; }

    virtual bool unify_with(TraitImplNode*) { assert(false); return false; }

    virtual bool is_closed() const { return true; } // TODO

protected:
    /// copy this \p TraitImplNode but replace the sub-elements given in the mapping
    Generic* vspecialize(SpecializeMapping& m);

    virtual std::string to_string() const { return ""; } // TODO

private:
    const Impl* const impl_decl_;
    Trait trait_;

    friend class TypeTable;
};

}

#endif
