#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "impala/sema/trait.h"

#include "thorin/type.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/array.h"
#include "thorin/util/hash.h"

#include "impala/sema/unifiable.h"

namespace thorin {
    class World;
}

namespace impala {

//------------------------------------------------------------------------------

enum Kind {
#define IMPALA_TYPE(itype, atype) Type_##itype,
#include "impala/tokenlist.h"
    Type_error,
    Type_unknown,
    Type_noReturn,
    Type_fn,
    Type_tuple,
    Type_var,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Type_##itype,
#include "impala/tokenlist.h"
};

class TypeNode : public TUnifiable<TypeNode> {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p TypeNode.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p TypeNode.

protected:
    TypeNode(TypeTable& typetable)
        : TUnifiable(typetable)
    {}

public:
    virtual Kind kind() const = 0;

    virtual thorin::ArrayRef<Type> elems() const = 0;
    virtual const Type elem(size_t i) const = 0;
    /// Returns number of \p TypeNode operands (\p elems_).
    virtual size_t size() const = 0;
    /// Returns true if this \p TypeNode does not have any \p TypeNode operands (\p elems_).
    virtual bool is_empty() const = 0;

    virtual void add_implementation(TraitImpl) = 0;
    virtual bool implements(Trait) const = 0;
    /// @return The method type or an empty type if no method with this name was found
    virtual Type find_method(Symbol s) const = 0;

    /// A type is closed if it contains no unbound type variables.
    virtual bool is_closed() const = 0;

    /// @return true if this is a subtype of super_type.
    virtual bool is_subtype(const Type super_type) const = 0;

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at().
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const = 0;
    virtual thorin::Type convert(thorin::World&) const = 0;
    void convert_elems(thorin::World& world, std::vector<thorin::Type>& nelems) const;
};

class KnownTypeNode : public TypeNode {
protected:
    KnownTypeNode(TypeTable& typetable, Kind kind, size_t size)
        : TypeNode(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    void set(size_t i, Type n) { elems_[i] = n; }
    Type elem_(size_t i) const { return elems_[i]; }

public:
    virtual Kind kind() const { return kind_; }
    virtual thorin::ArrayRef<Type> elems() const { return thorin::ArrayRef<Type>(elems_); }
    virtual const Type elem(size_t i) const { return elems_[i]; }
    virtual size_t size() const { return elems_.size(); }
    virtual bool is_empty() const {
        assert (!elems_.empty() || type_vars_.empty());
        return elems_.empty();
    }

    virtual void refine();
    virtual bool is_known() const override;

    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;

    virtual bool unify_with(Unifiable*);

    virtual void add_implementation(TraitImpl);
    virtual bool implements(Trait) const;
    virtual Type find_method(Symbol s) const;

    virtual bool is_generic() const {
        assert (!elems_.empty() || type_vars_.empty());
        return Unifiable::is_generic();
    }

    virtual bool is_closed() const;
    virtual bool is_subtype(const Type super_type) const;
    virtual bool is_sane() const;

private:
    const Kind kind_;
    UniSet<Trait> trait_impls_; // TODO do we want to have the impls or only the traits?
    std::vector<TraitImpl> gen_trait_impls_; // TODO use a map trait_name -> impls to make this faster!

protected:
    std::vector<Type> elems_; ///< The operands of this type constructor.

    friend class TraitInstanceNode;
    friend class CompoundTypeNode;
    friend class TypeTable;
};

class UnknownTypeNode : public TypeNode {
private:
    UnknownTypeNode(TypeTable& typetable)
        : TypeNode(typetable)
        , id_(counter_++)
    {}

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    virtual Kind kind() const { return is_instantiated() ? instance()->kind() : Type_unknown; }
    virtual std::string to_string() const;

    virtual thorin::ArrayRef<Type> elems() const { return is_instantiated() ? instance()->elems() : thorin::ArrayRef<Type>(); }
    virtual const Type elem(size_t i) const { assert(is_instantiated()); return instance()->elem(i); }
    virtual size_t size() const { return is_instantiated() ? instance()->size() : 0; }
    virtual bool is_empty() const { return !is_instantiated() || instance()->is_empty(); }

    virtual void refine() { assert(false); }
    virtual bool is_known() const override { return false; }

    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;
    virtual bool unify_with(Unifiable*);

    virtual void add_implementation(TraitImpl) { assert(false); }
    virtual bool implements(Trait t) const { return is_instantiated() && instance()->implements(t); }
    virtual Type find_method(Symbol s) const { assert(is_instantiated()); return instance()->find_method(s); }

    virtual size_t num_type_vars() const { return is_instantiated() ? instance()->num_type_vars() : 0; }
    virtual thorin::ArrayRef<TypeVar> type_vars() const { return is_instantiated() ? instance()->type_vars() : thorin::ArrayRef<TypeVar>(); }
    virtual TypeVar type_var(size_t i) const { assert(is_instantiated()); return instance()->type_var(i); }
    virtual void add_type_var(TypeVar v)  { assert(false); }
    virtual bool is_generic() const { assert(type_vars_.empty()); return is_instantiated() ? instance()->is_generic() : false; }

    virtual bool is_closed() const { return is_instantiated() && instance()->is_closed(); }
    virtual bool is_subtype(const Type super_type) const { return is_instantiated() && instance()->is_subtype(super_type); }
    virtual bool is_sane() const { return is_instantiated() && instance()->is_sane(); }

    bool is_instantiated() const { return !instance_.empty(); }
    Type instance() const { return instance_; }
    void instantiate(Type instance) { assert(!is_instantiated()); instance_ = instance; }
    virtual thorin::Type convert(thorin::World&) const { assert(false); return thorin::Type(); }

private:
    const int id_;       ///< Used for unambiguous dumping.
    static int counter_;
    Type instance_;

    friend class TypeTable;
};

class TypeErrorNode : public KnownTypeNode {
private:
    TypeErrorNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Type_error, 0)
    {}

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    virtual std::string to_string() const { return "<type error>"; }
    virtual thorin::Type convert(thorin::World&) const { assert(false); return thorin::Type(); }

    friend class TypeTable;
};

class NoReturnTypeNode : public KnownTypeNode {
private:
    NoReturnTypeNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Type_noReturn, 0)
    {}

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    virtual std::string to_string() const { return "<type no-return>"; }
    virtual thorin::Type convert(thorin::World&) const override;

    friend class TypeTable;
};

class PrimTypeNode : public KnownTypeNode {
private:
    PrimTypeNode(TypeTable& typetable, PrimTypeKind kind)
        : KnownTypeNode(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    virtual std::string to_string() const;
    virtual thorin::Type convert(thorin::World&) const;

    friend class TypeTable;
};

class CompoundTypeNode : public KnownTypeNode {
protected:
    CompoundTypeNode(TypeTable& typetable, Kind kind, thorin::ArrayRef<Type> elems)
        : KnownTypeNode(typetable, kind, elems.size())
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }

    std::string elems_to_string() const;

    thorin::Array<Type> specialize_elems(SpecializeMap&) const;
};

class FnTypeNode : public CompoundTypeNode {
private:
    FnTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundTypeNode(typetable, Type_fn, elems)
    {}

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    Type return_type() const;
    virtual std::string to_string() const { return std::string("fn") + type_vars_to_string() + elems_to_string(); }
    virtual thorin::Type convert(thorin::World&) const;

    FnType specialize_method(Type t) const;

    friend class TypeTable;
};

class TupleTypeNode : public CompoundTypeNode {
private:
    TupleTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundTypeNode(typetable, Type_tuple, elems)
    {}

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

public:
    virtual std::string to_string() const { return type_vars_to_string() + elems_to_string(); }
    virtual thorin::Type convert(thorin::World&) const;

    friend class TypeTable;
};

class TypeVarNode : public KnownTypeNode {
private:
    TypeVarNode(TypeTable& tt, Symbol name)
        : KnownTypeNode(tt, Type_var, 0)
        , id_(counter_++)
        , name_(name)
        , bound_at_(nullptr)
        , equiv_(nullptr)
    {}

    bool bounds_equal(const TypeVar) const;

public:
    const UniSet<Trait>& bounds() const { return bounds_; }
    const Unifiable* bound_at() const { return bound_at_; }
    void add_bound(Trait);
    virtual bool equal(const Unifiable* other) const;
    std::string to_string() const;
    virtual bool implements(Trait) const;
    virtual Type find_method(Symbol s) const;
    virtual thorin::Type convert(thorin::World&) const { assert(false && "TODO"); return thorin::Type(); }

    /**
     * A type variable is closed if it is bound and all restrictions are closed.
     * If a type variable is closed it must not be changed anymore!
     */
    virtual bool is_closed() const;

    // CHECK this->is_subtype(bound_at()); if bound_at is a Type, else it should occur in the method signatures
    virtual bool is_sane() const { return is_closed(); }

    /// Create a copy of this \p TypeVar that considers the specialization (the binding is not copied)
    TypeVar clone(SpecializeMap&) const;

private:
    const int id_;       ///< Used for unambiguous dumping.
    Symbol name_;
    UniSet<Trait> bounds_;///< All traits that restrict the instantiation of this variable.
    /**
     * The type where this variable is bound.
     * If such a type is set, then the variable must not be changed anymore!
     */
    const Unifiable* bound_at_;
    mutable const TypeVarNode* equiv_;///< Used to define equivalence constraints when checking equality of types.
    static int counter_;

protected:
    virtual Unifiable* vspecialize(SpecializeMap&);

    /// re-add all elements to the bounds set -- needed if during unification the representatives of bounds change
    void refresh_bounds();

    friend class TypeTable;
    friend void Unifiable::bind(TypeVar);                     // maybe we can design things better to avoid this friend
    friend bool KnownTypeNode::equal(const Unifiable*) const; // same here
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
