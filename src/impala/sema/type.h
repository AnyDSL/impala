#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/autoptr.h"
#include "thorin/util/array.h"
#include "thorin/util/hash.h"

#include "impala/sema/generic.h"
#include "impala/sema/trait.h"

namespace impala {

//------------------------------------------------------------------------------

enum Kind {
#define IMPALA_TYPE(itype, atype) Type_##itype,
#include "impala/tokenlist.h"
    Type_error,
    Type_noReturn,
    Type_fn,
    Type_tuple,
    Type_var,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Type_##itype,
#include "impala/tokenlist.h"
};

class TypeNode : public Unifiable<TypeNode> {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p TypeNode.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p TypeNode.

protected:
    TypeNode(TypeTable& typetable, Kind kind, size_t size)
        : Unifiable(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    void set(size_t i, Type n) { elems_[i] = n; }
    Type elem_(size_t i) const { return elems_[i]; }

public:
    Kind kind() const { return kind_; }
    thorin::ArrayRef<Type> elems() const { return thorin::ArrayRef<Type>(elems_); }
    const Type elem(size_t i) const { return elems_[i]; }
    /// Returns number of \p TypeNode operands (\p elems_).
    size_t size() const { return elems_.size(); }
    /// Returns true if this \p TypeNode does not have any \p TypeNode operands (\p elems_).
    bool is_empty() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return elems_.empty();
    }

    virtual bool equal(const TypeNode*) const;
    virtual size_t hash() const;

    void add_implementation(TraitImpl);
    virtual bool implements(Trait) const;
    virtual const UniSet<Trait>& trait_impls() const { return trait_impls_; }

    bool is_generic() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return Generic::is_generic();
    }

    /**
     * A type is closed if it contains no unbound type variables.
     */
    virtual bool is_closed() const;

    /// @return true if this is a subtype of super_type.
    bool is_subtype(const Type super_type) const;

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at().
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const;

private:
    const Kind kind_;
    UniSet<Trait> trait_impls_; // TODO do we want to have the impls or only the traits?

protected:
    std::vector<Type> elems_; ///< The operands of this type constructor.

    friend class TraitInstanceNode;
    friend class CompoundType;
    friend class TypeTable;
};

class TypeErrorNode : public TypeNode {
private:
    TypeErrorNode(TypeTable& typetable)
        : TypeNode(typetable, Type_error, 0)
    {}

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

public:
    virtual std::string to_string() const { return "<type error>"; }

    friend class TypeTable;
};

class NoReturnTypeNode : public TypeNode {
private:
    NoReturnTypeNode(TypeTable& typetable)
        : TypeNode(typetable, Type_noReturn, 0)
    {}

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

public:
    virtual std::string to_string() const { return "<type no-return>"; }

    friend class TypeTable;
};

class PrimTypeNode : public TypeNode {
private:
    PrimTypeNode(TypeTable& typetable, PrimTypeKind kind)
        : TypeNode(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

public:
    virtual std::string to_string() const;

    friend class TypeTable;
};

class CompoundType : public TypeNode {
protected:
    CompoundType(TypeTable& typetable, Kind kind, thorin::ArrayRef<Type> elems)
        : TypeNode(typetable, kind, elems.size())
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }

    std::string elems_to_string() const;

    thorin::Array<Type> specialize_elems(SpecializeMapping& mapping) const;
};

class FnTypeNode : public CompoundType {
private:
    FnTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

public:
    virtual std::string to_string() const { return std::string("fn") + bound_vars_to_string() + elems_to_string(); }

    FnType specialize_method(Type t) const;

    friend class TypeTable;
};

class TupleTypeNode : public CompoundType {
private:
    TupleTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

public:
    virtual std::string to_string() const { return std::string("tuple") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TypeVarNode : public TypeNode {
private:
    TypeVarNode(TypeTable& tt, Symbol name)
        : TypeNode(tt, Type_var, 0)
        , id_(counter++)
        , name_(name)
        , bound_at_(nullptr)
        , equiv_var_(nullptr)
    {}

    void set_equiv_variable(const TypeVarNode* v) const { assert(equiv_var_ == nullptr); assert(v != nullptr); equiv_var_ = v; }
    void unset_equiv_variable() const { assert(equiv_var_ != nullptr); equiv_var_ = nullptr; }
    void bind(const Generic* const e);
    bool bounds_equal(const TypeVar other) const;

public:
    const UniSet<Trait>& bounds() const { return bounds_; }
    const Generic* bound_at() const { return bound_at_; }
    void add_bound(Trait);
    virtual bool equal(const TypeNode* other) const;
    std::string to_string() const;

    virtual bool implements(Trait) const;
    virtual const UniSet<Trait>& trait_impls() const { return bounds(); }

    /**
     * A type variable is closed if it is bound and all restrictions are closed.
     * If a type variable is closed it must not be changed anymore!
     */
    virtual bool is_closed() const;

    // CHECK this->is_subtype(bound_at()); if bound_at is a Type, else it should occur in the method signatures
    virtual bool is_sane() const { return is_closed(); }

    /// Create a copy of this \p TypeVar that considers the specialization (the binding is not copied)
    TypeVar clone(SpecializeMapping&) const;

private:
    const int id_;       ///< Used for unambiguous dumping.
    Symbol name_;
    UniSet<Trait> bounds_;///< All traits that restrict the instantiation of this variable.
    /**
     * The type where this variable is bound.
     * If such a type is set, then the variable must not be changed anymore!
     */
    const Generic* bound_at_;
    mutable const TypeVarNode* equiv_var_;///< Used to define equivalence constraints when checking equality of types.
    static int counter;

protected:
    virtual Generic* vspecialize(SpecializeMapping&);

    /// re-add all elements to the bounds set -- needed if during unification the representatives of bounds change
    void refresh_bounds();

    friend class TypeTable;
    friend class TypeNode;
    friend class Generic;
};

typedef Proxy<TypeErrorNode> TypeError;
typedef Proxy<PrimTypeNode> PrimType;
typedef Proxy<NoReturnTypeNode> NoReturnType;
typedef Proxy<FnTypeNode> FnType;
typedef Proxy<TupleTypeNode> TupleType;

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void verify(thorin::ArrayRef<const Type> types);

}

#endif
