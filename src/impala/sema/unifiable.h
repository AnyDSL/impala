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
class BoundNode;        typedef Proxy<BoundNode>        Bound;
class FnTypeNode;       typedef Proxy<FnTypeNode>       FnType;
class ImplNode;         typedef Proxy<ImplNode>         Impl;
class KnownTypeNode;    typedef Proxy<KnownTypeNode>    KnownType;
class NoRetTypeNode;    typedef Proxy<NoRetTypeNode>    NoRetType;
class PrimTypeNode;     typedef Proxy<PrimTypeNode>     PrimType;
class StructTypeNode;   typedef Proxy<StructTypeNode>   StructType;
class TraitNode;        typedef Proxy<TraitNode>        Trait;
class TupleTypeNode;    typedef Proxy<TupleTypeNode>    TupleType;
class TypeErrorNode;    typedef Proxy<TypeErrorNode>    TypeError;
class TypeNode;         typedef Proxy<TypeNode>         Type;
class TypeVarNode;      typedef Proxy<TypeVarNode>      TypeVar;
class Unifiable;        typedef Proxy<Unifiable>        Uni;
class UnknownTypeNode;  typedef Proxy<UnknownTypeNode>  UnknownType;

//------------------------------------------------------------------------------

typedef thorin::HashMap<const TypeNode*, const TypeNode*> SpecializeMap;

SpecializeMap specialize_map(const Unifiable*, thorin::ArrayRef<Type>);
/// Creates a \p SpecializeMap by mapping each of \p type's type variable to the corresponding element in \p type_args.
template<class T>
SpecializeMap specialize_map(Proxy<T> type, thorin::ArrayRef<Type> type_args) { return specialize_map(*type, type_args); }

/**
 * note: bound checking cannot be done during instantiation of the unknowns because of types like fn[A:T[B], B: T[A]](a: A, b: B)
 * therefore it is important to call \p check_bounds after all unknowns have been resolved!
 */
Type instantiate_unknown(Type, std::vector<Type>&);

//------------------------------------------------------------------------------

/**
 * Try to fill in missing type information by matching this possibly incomplete Unifiable with a complete Unifiable.
 * Example: fn(?0, ?1) unified_with fn(int, bool)  will set ?0=int and ?1=bool
 * @return \p true if unification worked, i.e. both generics were structurally equal
 *         and there were no contradictions during unification (a contradiction
 *         would be fn(?0, ?0) unified with fn(int, bool)).
 */
bool infer(Uni, Uni);

template<class T>
class Proxy {
private:
    bool operator != (const Proxy<T>& other) const; ///< Always test positively to allow for automagic type inference.

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
        return infer(this->node()->unify(), other.node()->unify());
    }
    const T* representative() const { return node()->representative()->template as<T>(); }
    const T* node() const { assert(node_ != nullptr); return node_; }
    const T* operator  * () const { return node()->is_unified() ? representative() : node(); }
    const T* operator -> () const { return *(*this); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() const {
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
        assert(node_ == nullptr);
        node_ = *other; 
        return *this; 
    }
    void clear() { assert(node_ != nullptr); node_ = nullptr; }

private:
    const T* node_;
};

//------------------------------------------------------------------------------

enum Kind {
#define IMPALA_TYPE(itype, atype) Kind_##itype,
#include "impala/tokenlist.h"
    Kind_error,
    Kind_unknown,
    Kind_noret,
    Kind_fn,
    Kind_tuple,
    Kind_struct,
    Kind_type_var,
    Kind_trait,
    Kind_bound,
    Kind_impl,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Kind_##itype,
#include "impala/tokenlist.h"
};

class Unifiable : public thorin::MagicCast<Unifiable> {
private:
    Unifiable& operator = (const Unifiable&); ///< Do not copy-assign a \p Unifiable.
    Unifiable(const Unifiable&);              ///< Do not copy-construct a \p Unifiable.

protected:
    Unifiable(TypeTable& tt, Kind kind)
        : typetable_(tt)
        , kind_(kind)
        , representative_(nullptr)
        , id_(counter_++)
    {}

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    const Unifiable* representative() const { return representative_; }
    const int id() const { return id_; }
    bool is_unified() const { return representative_ != nullptr; }
    const Unifiable* unify() const;
    void dump() const;

    size_t num_type_vars() const { return type_vars_.size(); }
    thorin::ArrayRef<TypeVar> type_vars() const { return thorin::ArrayRef<TypeVar>(type_vars_); }
    TypeVar type_var(size_t i) const { return type_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p type_vars_).
    bool is_generic() const { return !type_vars_.empty(); }
    virtual bool is_closed() const = 0; // TODO
    virtual void bind(TypeVar v) const;
    virtual bool equal(const Unifiable*) const = 0;
    virtual size_t hash() const = 0;
    virtual std::string to_string() const = 0;
    virtual bool is_error() const { return false; }
    /// A \p Unifiable is known if it does not contain any \p UnknownTypeNode%s
    virtual bool is_known() const = 0;

protected:
    std::string type_vars_to_string() const;

private:
    virtual thorin::Type convert(CodeGen&) const = 0;
    void convert_type_vars(CodeGen&) const;
    void bind_type_vars(CodeGen&) const;

    static int counter_;

    TypeTable& typetable_;
    const Kind kind_;
    mutable const Unifiable* representative_;
    const int id_;
    mutable thorin::Type thorin_type_;

protected:
    mutable std::vector<TypeVar> type_vars_;

    friend class CodeGen;
    friend class TypeTable;
    friend bool infer(const Unifiable*, const Unifiable*);
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

class TypeNode : public Unifiable {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p TypeNode.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p TypeNode.

protected:
    TypeNode(TypeTable& typetable, Kind kind)
        : Unifiable(typetable, kind)
    {}

public:
    /// Specializes recursively this type while obeying \p map.
    Type specialize(SpecializeMap& map) const;
    /**
     * Type variables are removed from this type.
     * They must be found in \p map in order to specialize the resulting type.
     */
    Type instantiate(SpecializeMap& map) const;

    virtual bool implements(Bound, SpecializeMap&) const = 0;
    /// @return The method type or an empty type if no method with this name was found
    virtual Type find_method(Symbol s) const = 0;
    /// A type is closed if it contains no unbound type variables.
    virtual bool is_closed() const = 0;
    bool is_noret() const { return isa<NoRetTypeNode>(); }
    bool is(PrimTypeKind kind) const;
#define IMPALA_TYPE(itype, atype) bool is_##itype() const { return is(PrimType_##itype); }
#include "impala/tokenlist.h"

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at(). -- TODO WHAT?
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const = 0;

private:
    virtual Type vinstantiate(SpecializeMap&) const = 0;
};

class KnownTypeNode : public TypeNode {
protected:
    KnownTypeNode(TypeTable& typetable, Kind kind, size_t size)
        : TypeNode(typetable, kind)
        , elems_(size)
    {}

    void set(size_t i, Type n) { elems_[i] = n; }
    thorin::Array<Type> specialize_elems(SpecializeMap&) const;
    void convert_elems(CodeGen& world, std::vector<thorin::Type>& nelems) const;
    std::string elems_to_string() const;

public:
    thorin::ArrayRef<Type> elems() const { return thorin::ArrayRef<Type>(elems_); }
    const Type elem(size_t i) const { return elems_[i]; }
    size_t size() const { return elems_.size(); }
    bool is_empty() const { assert(!elems_.empty() || type_vars_.empty()); return elems_.empty(); }
    const std::vector<Impl>& impls() const { return impls_; }
    void add_impl(Impl) const;

    virtual bool is_known() const override;
    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;
    virtual bool implements(Bound, SpecializeMap&) const;
    virtual Type find_method(Symbol s) const;
    virtual bool is_closed() const;
    virtual bool is_sane() const;

private:
    mutable std::vector<Impl> impls_;

protected:
    std::vector<Type> elems_; ///< The operands of this type constructor.

    friend class TypeTable;
};

class UnknownTypeNode : public TypeNode {
private:
    UnknownTypeNode(TypeTable& typetable)
        : TypeNode(typetable, Kind_unknown)
    {}

public:
    virtual std::string to_string() const;

    virtual bool is_known() const override { return false; }
    virtual bool equal(const Unifiable*) const;
    virtual size_t hash() const;
    virtual bool implements(Bound bound, SpecializeMap& map) const { return is_unified() && instance()->implements(bound, map); }
    virtual Type find_method(Symbol s) const { assert(is_unified()); return instance()->find_method(s); }
    virtual bool is_closed() const { assert(!is_unified() || instance()->is_closed()); return true; }
    virtual bool is_sane() const { return is_unified() && instance()->is_sane(); }
    virtual bool is_error() const override { return is_unified() ? instance()->is_error() : false; }
    Type instance() const { return representative()->as<TypeNode>(); }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const { assert(false); return thorin::Type(); }

    friend class TypeTable;
};

class TypeErrorNode : public KnownTypeNode {
private:
    TypeErrorNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Kind_error, 0)
    {}

public:
    virtual bool is_error() const override { return true; }
    virtual std::string to_string() const { return "<type error>"; }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const { assert(false); return thorin::Type(); }

    friend class TypeTable;
};

class NoRetTypeNode : public KnownTypeNode {
private:
    NoRetTypeNode(TypeTable& typetable)
        : KnownTypeNode(typetable, Kind_noret, 0)
    {}

public:
    virtual std::string to_string() const override { return "<no-return>"; }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const override;

    friend class TypeTable;
};

class PrimTypeNode : public KnownTypeNode {
private:
    PrimTypeNode(TypeTable& typetable, PrimTypeKind kind)
        : KnownTypeNode(typetable, (Kind) kind, 0)
    {}

public:
    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }
    virtual std::string to_string() const;

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class FnTypeNode : public KnownTypeNode {
private:
    FnTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : KnownTypeNode(typetable, Kind_fn, elems.size())
    {
        for (size_t i = 0, e = elems.size(); i != e; ++i)
            set(i, elems[i]);
    }

public:
    Type return_type() const;
    FnType peel_first() const;
    virtual std::string to_string() const { return std::string("fn") + type_vars_to_string() + elems_to_string(); }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class TupleTypeNode : public KnownTypeNode {
private:
    TupleTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : KnownTypeNode(typetable, Kind_tuple, elems.size())
    {
        for (size_t i = 0, e = elems.size(); i != e; ++i)
            set(i, elems[i]);
    }

public:
    virtual std::string to_string() const { return type_vars_to_string() + elems_to_string(); }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const;

    friend class TypeTable;
};

class StructTypeNode : public KnownTypeNode {
private:
    StructTypeNode(TypeTable& typetable, const StructDecl* struct_decl);

public:
    const StructDecl* struct_decl() const { return struct_decl_; }
    virtual std::string to_string() const { return "TODO"; }

private:
    virtual Type vinstantiate(SpecializeMap&) const;
    virtual thorin::Type convert(CodeGen&) const;

    const StructDecl* struct_decl_;

    friend class TypeTable;
};

class TypeVarNode : public KnownTypeNode {
private:
    TypeVarNode(TypeTable& tt, Symbol name)
        : KnownTypeNode(tt, Kind_type_var, 0)
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

private:
    virtual Type vinstantiate(SpecializeMap&) const;

    friend class TypeTable;
    friend void Unifiable::bind(TypeVar) const;
    friend bool KnownTypeNode::equal(const Unifiable*) const;
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
        : Unifiable(tt, Kind_trait)
        , trait_decl_(trait_decl)
    {}

public:
    const TraitDecl* trait_decl() const { return trait_decl_; }
    const UniSet<Bound>& super_bounds() const { return super_bounds_; }
    Bound super_bound(Trait trait) const;
    const thorin::HashSet<const TraitNode*>& sub_traits() const { return sub_traits_; }
    const std::vector<Impl>& type2impls(Type type) const { return type2impls_[type]; }
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
    virtual bool is_closed() const { return true; } // TODO
    virtual bool is_error() const override { return trait_decl() == nullptr; }
    virtual std::string to_string() const;

private:
    virtual thorin::Type convert(CodeGen&) const override;

    const TraitDecl* const trait_decl_;
    mutable UniSet<Bound> super_bounds_;
    mutable thorin::HashSet<const TraitNode*> sub_traits_;
    mutable UniMap<Type, std::vector<Impl>> type2impls_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

/// An instance of a trait is a trait where all type variables are instantiated by concrete types.
class BoundNode : public Unifiable {
private:
    BoundNode(const Trait trait, thorin::ArrayRef<Type> type_args)
        : Unifiable(trait->typetable(), Kind_bound)
        , trait_(trait)
        , type_args_(type_args)
    {
        assert(trait_->num_type_vars() == num_type_args());
    }

public:
    const Trait trait() const { return trait_; }
    const Type type_arg(size_t i) const { return type_args_[i]; }
    thorin::ArrayRef<Type> type_args() const { return type_args_; }
    size_t num_type_args() const { return type_args_.size(); }
    Type find_method(Symbol name) const;
    Bound specialize(SpecializeMap&) const;

    virtual bool equal(const Unifiable* other) const override;
    virtual size_t hash() const override;
    virtual std::string to_string() const;
    virtual bool is_known() const override;
    virtual bool is_closed() const;
    virtual bool is_error() const override { return trait()->is_error(); }

private:
    virtual thorin::Type convert(CodeGen&) const override;

    const Trait trait_;
    thorin::Array<Type> type_args_;
    mutable thorin::HashMap<Symbol, Type> method_cache_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class ImplNode : public Unifiable {
private:
    ImplNode(TypeTable& tt, const ImplItem* impl_item, Bound bound, Type type)
        : Unifiable(tt, Kind_impl)
        , impl_item_(impl_item)
        , bound_(bound)
        , type_(type)
    {}

public:
    const ImplItem* impl_item() const { return impl_item_; }
    Bound bound() const { return bound_; }
    Type type() const { return type_; }
    Impl specialize(SpecializeMap& map) const;

    virtual bool equal(const Unifiable* other) const { return this->impl_item() == other->as<ImplNode>()->impl_item(); }
    virtual size_t hash() const;
    virtual bool is_known() const override { return true; }
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
