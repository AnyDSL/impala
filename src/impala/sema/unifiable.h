#ifndef IMPALA_SEMA_UNIFIABLE_H
#define IMPALA_SEMA_UNIFIABLE_H

#include <vector>

#include "thorin/type.h"
#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

#include "impala/symbol.h"

namespace impala {

class CodeGen;
class ImplItem;
class StructDecl;
class TraitDecl;
class TypeTable;
class Unifiable;

template<class T> class Proxy;
class TypeErrorNode;    typedef Proxy<TypeErrorNode> TypeError;
class PrimTypeNode;     typedef Proxy<PrimTypeNode> PrimType;
class NoRetTypeNode;    typedef Proxy<NoRetTypeNode> NoRetType;
class FnTypeNode;       typedef Proxy<FnTypeNode> FnType;
class TupleTypeNode;    typedef Proxy<TupleTypeNode> TupleType;
class StructTypeNode;   typedef Proxy<StructTypeNode> StructType;
class ImplNode;         typedef Proxy<ImplNode> Impl;
class BoundNode;        typedef Proxy<BoundNode> Bound;
class TraitNode;        typedef Proxy<TraitNode> Trait;
class TypeNode;         typedef Proxy<TypeNode> Type;
class TypeVarNode;      typedef Proxy<TypeVarNode> TypeVar;
class UnknownTypeNode;  typedef Proxy<UnknownTypeNode> UnknownType;

//------------------------------------------------------------------------------

typedef thorin::HashMap<const TypeNode*, const TypeNode*> SpecializeMap;

SpecializeMap specialize_map(const Unifiable*, thorin::ArrayRef<Type>);
template<class T>
SpecializeMap specialize_map(Proxy<T> proxy, thorin::ArrayRef<Type> type_args) { return specialize_map(*proxy, type_args); }

/**
 * note: bound checking cannot be done during instantiation of the unknowns because of types like fn[A:T[B], B: T[A]](a: A, b: B)
 * therefore it is important to call \p check_bounds after all unknowns have been resolved!
 */
Type instantiate_unknown(Type, std::vector<Type>&);

//------------------------------------------------------------------------------

template<class T>
class Proxy {
public:
    typedef T BaseType;

    Proxy()
        : node_(nullptr)
    {}
    Proxy(const T* node)
        : node_(node)
    {}

    bool empty() const { return node_ == nullptr; }
    bool operator == (const Proxy<T>& other) const {
        assert(&node()->typetable() == &other.node()->typetable());
        if (this->node_ == other.node_) // TODO do we really wanna have this check?
            return true;
        this->node()->unify();
        other.node()->unify();
        return representative() == other.representative();
    }
    bool operator != (const Proxy<T>& other) const { return !(*this == other); }
    const T* representative() const { return node()->representative()->template as<T>(); }
    const T* node() const { assert(node_ != nullptr); return node_; }
    const T* operator  * () const { assert(node_ != nullptr); return node_->is_unified() ? representative() : node_; }
    const T* operator -> () const { return *(*this); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "U is not a base type of T");
        return Proxy<U>((**this)->template as<T>());
    }
    template<class U> Proxy<typename U::BaseType> isa() const { 
        return Proxy<typename U::BaseType>((*this)->isa<typename U::BaseType>()); 
    }
    template<class U> Proxy<typename U::BaseType> as() const { 
        return Proxy<typename U::BaseType>((*this)->as <typename U::BaseType>()); 
    }
    operator bool() { return !empty(); }
    Proxy<T>& operator= (Proxy<T> other) { 
        assert(node_ == nullptr || node_->template isa<UnknownTypeNode>()); 
        node_ = *other; 
        return *this; 
    }
    void clear() { assert(node_ != nullptr); node_ = nullptr; }

private:
    const T* node_;
};

//------------------------------------------------------------------------------

class Unifiable : public thorin::MagicCast<Unifiable> {
private:
    Unifiable& operator = (const Unifiable&); ///< Do not copy-assign a \p Unifiable.
    Unifiable(const Unifiable&);              ///< Do not copy-construct a \p Unifiable.

protected:
    Unifiable(TypeTable& tt)
        : typetable_(tt)
        , representative_(nullptr)
        , id_(counter_++)
    {}

public:
    TypeTable& typetable() const { return typetable_; }
    const Unifiable* representative() const { return representative_; }
    const int id() const { return id_; }
    bool is_unified() const { return representative_ != nullptr; }
    virtual size_t num_type_vars() const { return type_vars_.size(); }
    virtual thorin::ArrayRef<TypeVar> type_vars() const { return thorin::ArrayRef<TypeVar>(type_vars_); }
    virtual TypeVar type_var(size_t i) const { return type_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p type_vars_).
    virtual bool is_generic() const { return !type_vars_.empty(); }
    virtual bool is_closed() const = 0; // TODO
    virtual void bind(TypeVar v) const;
    virtual bool equal(const Unifiable*) const = 0;
    virtual size_t hash() const = 0;
    virtual std::string to_string() const = 0;
    virtual bool is_error() const { return false; }
    void set_representative(const Unifiable* repr) const;
    const Unifiable* unify() const;

    /**
     * Try to fill in missing type information by matching this possibly incomplete Unifiable with a complete Unifiable.
     * Example: fn(?0, ?1) unified_with fn(int, bool)  will set ?0=int and ?1=bool
     * @return \p true if unification worked, i.e. both generics were structurally equal
     *         and there were no contradictions during unification (a contradiction
     *         would be fn(?0, ?0) unified with fn(int, bool)).
     */
    virtual bool infer(const Unifiable*) const = 0;
    template<class T>
    bool infer(Proxy<T> other) const {
        assert(other->is_closed());
        bool b = infer(*other);
        assert(!b || is_closed());
        return b;
    }

    /// A \p Unifiable is known if it does not contain any \p UnknownTypeNode%s
    virtual bool is_known() const = 0;

    void dump() const;

protected:
    std::string type_vars_to_string() const;
    bool type_vars_known() const;

private:
    virtual thorin::Type convert(CodeGen&) const = 0;

    TypeTable& typetable_;
    mutable const Unifiable* representative_;
    const int id_;
    mutable thorin::Type thorin_type_;
    static int counter_;

protected:
    mutable std::vector<TypeVar> type_vars_;

    friend class CodeGen;
    friend class TypeTable;
};

//------------------------------------------------------------------------------

template<class T>
std::ostream& operator << (std::ostream& o, Proxy<T> u) { return o << u->to_string(); }

//------------------------------------------------------------------------------

template<class T> struct UniHash {
    size_t operator () (const T t) const { return t->hash(); }
};
template<class T> struct UniEqual {
    bool operator () (const T t1, const T t2) const { return (t1->is_unified() && t2->is_unified()) ? t1 == t2 : t1->equal(*t2); }
};
template<class T> using UniSet = thorin::HashSet<T, UniHash<T>, UniEqual<T>>;
template<class T, class U> using UniMap = thorin::HashMap<T, U, UniHash<T>, UniEqual<T>>;

//------------------------------------------------------------------------------

enum Kind {
#define IMPALA_TYPE(itype, atype) Type_##itype,
#include "impala/tokenlist.h"
    Type_error,
    Type_unknown,
    Type_noret,
    Type_fn,
    Type_tuple,
    Type_struct,
    Type_var,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Type_##itype,
#include "impala/tokenlist.h"
};

class TypeNode : public Unifiable {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p TypeNode.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p TypeNode.

protected:
    TypeNode(TypeTable& typetable)
        : Unifiable(typetable)
    {}

    thorin::Array<Type> specialize_elems(SpecializeMap&) const;
    void convert_elems(CodeGen& world, std::vector<thorin::Type>& nelems) const;
    std::string elems_to_string() const;

public:
    virtual Kind kind() const = 0;

    virtual thorin::ArrayRef<Type> elems() const = 0;
    virtual const Type elem(size_t i) const = 0;
    /// Returns number of \p TypeNode operands (\p elems_).
    virtual size_t size() const = 0;
    /// Returns true if this \p TypeNode does not have any \p TypeNode operands (\p elems_).
    virtual bool is_empty() const = 0;

    virtual void add_impl(Impl) const = 0;
    virtual bool implements(Bound, SpecializeMap&) const = 0;
    /// @return The method type or an empty type if no method with this name was found
    virtual Type find_method(Symbol s) const = 0;

    /// A type is closed if it contains no unbound type variables.
    virtual bool is_closed() const = 0;
    virtual bool is_noret() const { return false; }

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at().
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const = 0;
    Type specialize() const { SpecializeMap map; return specialize(map); }
    Type specialize(SpecializeMap&) const;
    virtual Type vspecialize(SpecializeMap&) const = 0;
};

class KnownTypeNode : public TypeNode {
protected:
    KnownTypeNode(TypeTable& typetable, Kind kind, size_t size)
        : TypeNode(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    void set(size_t i, Type n) { elems_[i] = n; }

public:
    const std::vector<Impl>& impls() const { return impls_; }
    virtual Kind kind() const { return kind_; }
    virtual thorin::ArrayRef<Type> elems() const { return thorin::ArrayRef<Type>(elems_); }
    virtual const Type elem(size_t i) const { return elems_[i]; }
    virtual size_t size() const { return elems_.size(); }
    virtual bool is_empty() const {
        assert (!elems_.empty() || type_vars_.empty());
        return elems_.empty();
    }

    virtual bool is_known() const override;
    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;
    virtual bool infer(const Unifiable*) const override;
    virtual void add_impl(Impl) const;
    virtual bool implements(Bound, SpecializeMap&) const;
    virtual Type find_method(Symbol s) const;
    virtual bool is_generic() const {
        assert (!elems_.empty() || type_vars_.empty());
        return Unifiable::is_generic();
    }
    virtual bool is_closed() const;
    virtual bool is_sane() const;

private:
    const Kind kind_; // TODO move kind_ to Unifiable
    mutable std::vector<Impl> impls_;

protected:
    std::vector<Type> elems_; ///< The operands of this type constructor.

    friend class BoundNode;
    friend class TypeTable;
};

class UnknownTypeNode : public TypeNode {
private:
    UnknownTypeNode(TypeTable& typetable)
        : TypeNode(typetable)
    {}

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    virtual Kind kind() const { return is_instantiated() ? instance()->kind() : Type_unknown; }
    virtual std::string to_string() const;

    virtual thorin::ArrayRef<Type> elems() const { return is_instantiated() ? instance()->elems() : thorin::ArrayRef<Type>(); }
    virtual const Type elem(size_t i) const { assert(is_instantiated()); return instance()->elem(i); }
    virtual size_t size() const { return is_instantiated() ? instance()->size() : 0; }
    virtual bool is_empty() const { return !is_instantiated() || instance()->is_empty(); }
    virtual bool is_known() const override { return false; }
    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;
    virtual bool infer(const Unifiable*) const override;
    virtual void add_impl(Impl) const { assert(false); }
    virtual bool implements(Bound bound, SpecializeMap& map) const { return is_instantiated() && instance()->implements(bound, map); }
    virtual Type find_method(Symbol s) const { assert(is_instantiated()); return instance()->find_method(s); }
    virtual size_t num_type_vars() const { return is_instantiated() ? instance()->num_type_vars() : 0; }
    virtual thorin::ArrayRef<TypeVar> type_vars() const { return is_instantiated() ? instance()->type_vars() : thorin::ArrayRef<TypeVar>(); }
    virtual TypeVar type_var(size_t i) const { assert(is_instantiated()); return instance()->type_var(i); }
    virtual void add_type_var(TypeVar v)  { assert(false); }
    virtual bool is_generic() const { assert(type_vars_.empty()); return is_instantiated() ? instance()->is_generic() : false; }
    virtual bool is_closed() const { assert(!is_instantiated() || instance()->is_closed()); return true; }
    virtual bool is_sane() const { return is_instantiated() && instance()->is_sane(); }
    virtual bool is_error() const override { return is_instantiated() ? instance()->is_error() : false; }
    virtual bool is_noret() const override { return is_instantiated() ? instance()->is_noret() : false; }
    bool is_instantiated() const { return !instance_.empty(); }
    Type instance() const { return instance_; }
    void instantiate(Type instance) const { assert(!is_instantiated()); instance_ = instance; }

private:
    virtual thorin::Type convert(CodeGen&) const { assert(false); return thorin::Type(); }

    mutable Type instance_;

    friend class TypeTable;
};

class TypeErrorNode : public KnownTypeNode {
private:
    TypeErrorNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Type_error, 0)
    {}

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    virtual bool is_error() const override { return true; }
    virtual std::string to_string() const { return "<type error>"; }

private:
    virtual thorin::Type convert(CodeGen&) const { assert(false); return thorin::Type(); }

    friend class TypeTable;
};

class NoRetTypeNode : public KnownTypeNode {
private:
    NoRetTypeNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Type_noret, 0)
    {}

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    virtual bool is_noret() const override { return true; }
    virtual std::string to_string() const override { return "<no-return>"; }

private:
    virtual thorin::Type convert(CodeGen&) const override;

    friend class TypeTable;
};

class PrimTypeNode : public KnownTypeNode {
private:
    PrimTypeNode(TypeTable& typetable, PrimTypeKind kind)
        : KnownTypeNode(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    virtual std::string to_string() const;

private:
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class FnTypeNode : public KnownTypeNode {
private:
    FnTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : KnownTypeNode(typetable, Type_fn, elems.size())
    {
        for (size_t i = 0, e = elems.size(); i != e; ++i)
            set(i, elems[i]);
    }

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    Type return_type() const;
    virtual std::string to_string() const { return std::string("fn") + type_vars_to_string() + elems_to_string(); }
    FnType specialize_method(Type t) const;

private:
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class TupleTypeNode : public KnownTypeNode {
private:
    TupleTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : KnownTypeNode(typetable, Type_tuple, elems.size())
    {
        for (size_t i = 0, e = elems.size(); i != e; ++i)
            set(i, elems[i]);
    }

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    virtual std::string to_string() const { return type_vars_to_string() + elems_to_string(); }

private:
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class StructTypeNode : public KnownTypeNode {
private:
    StructTypeNode(TypeTable& typetable, const StructDecl* struct_decl);

protected:
    virtual Type vspecialize(SpecializeMap&) const;

public:
    const StructDecl* struct_decl() const { return struct_decl_; }
    virtual std::string to_string() const { return "TODO"; }

private:
    virtual thorin::Type convert(CodeGen&) const;

    const StructDecl* struct_decl_;

    friend class TypeTable;
};

class TypeVarNode : public KnownTypeNode {
private:
    TypeVarNode(TypeTable& tt, Symbol name)
        : KnownTypeNode(tt, Type_var, 0)
        , name_(name)
        , bound_at_(nullptr)
        , equiv_(nullptr)
    {}

    bool bounds_equal(const TypeVar) const;

public:
    const UniSet<Bound>& bounds() const { return bounds_; }
    const Unifiable* bound_at() const { return bound_at_; }
    void add_bound(Bound) const;
    virtual bool equal(const Unifiable* other) const;
    std::string to_string() const;
    virtual bool implements(Bound, SpecializeMap&) const;
    virtual Type find_method(Symbol s) const;

    /**
     * A type variable is closed if it is bound and all restrictions are closed.
     * If a type variable is closed it must not be changed anymore!
     */
    virtual bool is_closed() const;
    virtual bool is_sane() const { return is_closed(); }
    TypeVar specialize_bounds(SpecializeMap&) const;

private:
    virtual thorin::Type convert(CodeGen&) const { assert(false); return thorin::Type(); }

    Symbol name_;
    mutable UniSet<Bound> bounds_;///< All traits that restrict the instantiation of this variable.
    /**
     * The type where this variable is bound.
     * If such a type is set, then the variable must not be changed anymore!
     */
    mutable const Unifiable* bound_at_;
    mutable const TypeVarNode* equiv_;///< Used to define equivalence constraints when checking equality of types.

protected:
    virtual Type vspecialize(SpecializeMap&) const;

    friend class TypeTable;
    friend void Unifiable::bind(TypeVar) const;               // maybe we can design things better to avoid this friend
    friend bool KnownTypeNode::equal(const Unifiable*) const; // same here
};

//------------------------------------------------------------------------------

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
    TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
        : Unifiable(tt)
        , trait_decl_(trait_decl)
    {}

public:
    const TraitDecl* trait_decl() const { return trait_decl_; }
    const UniSet<Bound>& super_bounds() const { return super_bounds_; }
    Bound super_bound(Trait trait) const;
    const thorin::HashSet<const TraitNode*>& sub_traits() const { return sub_traits_; }
    const std::vector<Impl>& type2impls(Type type) const { return type2impls_[type]; }
    const std::vector<Bound>& instances() const {return instances_; }
    bool is_error_trait() const { return trait_decl_ == nullptr; }
    bool add_super_bound(Bound) const;
    /// return the type of the method with this name if it exists; otherwise return an empty type
    Type find_method(Symbol name) const;
    bool has_method(Symbol name) const { return !find_method(name).empty(); }
    Bound instantiate(thorin::ArrayRef<Type> args) const;
    void add_impl(Impl impl) const;
    virtual bool equal(const Unifiable* other) const override;
    virtual size_t hash() const override;
    virtual bool is_known() const override { return true; }
    virtual bool infer(const Unifiable*) const override { assert(false); return false; }
    virtual bool is_closed() const { return true; } // TODO
    virtual bool is_error() const override { return trait_decl() == nullptr; }
    virtual std::string to_string() const;

private:
    virtual thorin::Type convert(CodeGen&) const override;

    const TraitDecl* const trait_decl_;
    mutable UniSet<Bound> super_bounds_;
    mutable thorin::HashSet<const TraitNode*> sub_traits_;
    mutable UniMap<Type, std::vector<Impl>> type2impls_;
    mutable std::vector<Bound> instances_;

    friend class TypeTable;
    friend class BoundNode;
};

//------------------------------------------------------------------------------

/// An instance of a trait is a trait where all type variables are instantiated by concrete types.
class BoundNode : public Unifiable {
private:
    BoundNode(const Trait trait, thorin::ArrayRef<Type> type_args)
        : Unifiable(trait->typetable())
        , trait_(trait)
        , type_args_(type_args)
    {
        trait->instances_.push_back(Bound(this));
        assert(trait_->num_type_vars() == num_type_args());
    }

public:
    const Trait trait() const { return trait_; }
    const Type type_arg(size_t i) const { return type_args_[i]; }
    thorin::ArrayRef<Type> type_args() const { return type_args_; }
    size_t num_type_args() const { return type_args_.size(); }
    Type find_method(Symbol name) const;
    virtual bool equal(const Unifiable* other) const override;
    virtual size_t hash() const override;
    virtual std::string to_string() const;
    virtual bool is_known() const override;
    virtual bool infer(const Unifiable*) const override;
    virtual bool is_closed() const;
    virtual bool is_error() const override { return trait()->is_error(); }
    Bound specialize(SpecializeMap&) const;

private:
    virtual thorin::Type convert(CodeGen&) const override;

    const Trait trait_;
    thorin::Array<Type> type_args_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class ImplNode : public Unifiable {
private:
    ImplNode(TypeTable& tt, const ImplItem* impl_item, Bound bound, Type type)
        : Unifiable(tt)
        , impl_item_(impl_item)
        , bound_(bound)
        , type_(type)
    {}

public:
    virtual bool equal(const Unifiable* other) const { return this->impl_item() == other->as<ImplNode>()->impl_item(); }
    virtual size_t hash() const;
    const ImplItem* impl_item() const { return impl_item_; }
    Bound bound() const { return bound_; }
    Type type() const { return type_; }
    Impl specialize(SpecializeMap& map) const;
    virtual bool is_known() const override { return true; }
    virtual bool infer(const Unifiable*) const override { assert(false); return false; }
    virtual bool is_closed() const { return true; } // TODO

protected:
    virtual std::string to_string() const { return ""; } // TODO

private:
    virtual thorin::Type convert(CodeGen&) const override;

    const ImplItem* const impl_item_;
    Bound bound_;
    Type type_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

}

#endif
