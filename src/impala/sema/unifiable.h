#ifndef IMPALA_SEMA_UNIFIABLE_H
#define IMPALA_SEMA_UNIFIABLE_H

#include <typeinfo>
#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

namespace impala {

class TraitImplNode;
class TraitNode;
class TypeNode;
class TypeTable;
class TypeVarNode;
class Unifiable;
class UnknownTypeNode;
template<class T> class Proxy;
template<class T> void unify(TypeTable&, const Proxy<T>&);
typedef Proxy<TraitImplNode> TraitImpl;
typedef Proxy<TraitNode> Trait;
typedef Proxy<TypeNode> Type;
typedef Proxy<TypeVarNode> TypeVar;
typedef Proxy<UnknownTypeNode> UnknownType;
typedef thorin::HashMap<const Unifiable*, Unifiable*> SpecializeMap;

//------------------------------------------------------------------------------

template<class T>
class Proxy {
public:
    typedef T BaseType;

    Proxy()
        : node_(nullptr)
    {}
    explicit Proxy(T* node)
        : node_(node)
    {}

    bool empty() const { return node_ == nullptr; }
    bool operator == (const Proxy<T>& other) const {
        assert(node_ != nullptr);         
        assert(&node()->typetable() == &other.node()->typetable());
        unify(node()->typetable(), *this);
        unify(node()->typetable(), other);
        return representative() == other.representative();
    }
    bool operator != (const Proxy<T>& other) const { assert(node_ != nullptr); return !(*this == other); }
    T* representative() const { assert(node_ != nullptr); return node_->representative()->template as<T>(); }
    T* node() const { assert(node_ != nullptr); return node_; }
    T* operator  * () const { assert(node_ != nullptr); return node_->is_unified() ? representative() : node_->template as<T>(); }
    T* operator -> () const { assert(node_ != nullptr); return *(*this); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "U is not a base type of T");
        assert(node_ != nullptr); return Proxy<U>((U*) node_);
    }
    template<class U> Proxy<typename U::BaseType> isa() const { 
        assert(node_ != nullptr); return Proxy<typename U::BaseType>((*this)->isa<typename U::BaseType>()); 
    }
    template<class U> Proxy<typename U::BaseType> as() const { 
        assert(node_ != nullptr); return Proxy<typename U::BaseType>((*this)->as <typename U::BaseType>()); 
    }
    operator bool() { return !empty(); }
    Proxy<T>& operator= (Proxy<T> other) { 
        assert(node_ == nullptr || node_->template isa<UnknownTypeNode>()); 
        node_ = *other; 
        return *this; 
    }
    void clear() { node_ = nullptr; }

private:
    T* node_;
};

//------------------------------------------------------------------------------

class Unifiable : public thorin::MagicCast<Unifiable> {
protected:
    Unifiable(TypeTable& tt)
        : typetable_(tt)
        , representative_(nullptr)
    {}

public:
    TypeTable& typetable() const { return typetable_; }

    Unifiable* representative() const { return representative_; }
    bool is_unified() const { return representative_ != nullptr; }
    virtual size_t num_bound_vars() const { return bound_vars_.size(); }
    virtual thorin::ArrayRef<TypeVar> bound_vars() const { return thorin::ArrayRef<TypeVar>(bound_vars_); }
    virtual TypeVar bound_var(size_t i) const { return bound_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    virtual bool is_generic() const { return !bound_vars_.empty(); }
    virtual bool is_closed() const = 0; // TODO
    virtual void add_bound_var(TypeVar v);
    virtual bool equal(const Unifiable*) const = 0;
    virtual size_t hash() const = 0;
    virtual std::string to_string() const = 0;

    /**
     * Try to fill in missing type information by matching this possibly incomplete Unifiable with a complete Unifiable.
     * Example: fn(?0, ?1) unified_with fn(int, bool)  will set ?0=int and ?1=bool
     * @return \p true if unification worked, i.e. both generics were structurally equal
     *         and there were no contradictions during unification (a contradiction
     *         would be fn(?0, ?0) unified with fn(int, bool)).
     */
    virtual bool unify_with(Unifiable*) = 0;
    template<class T>
    bool unify_with(Proxy<T> other) {
        assert(other->is_closed());
        bool b = unify_with(*other);
        assert(!b || is_closed());
        return b;
    }

    /**
     * Replace any \p UnknownTypeNode%s within this Unifiable with their instances
     * and set the representatives of these nodes to their instances
     */
    virtual void refine() = 0;
    /// A \p Unifiable is known if it does not contain any \p UnknownTypeNode%s
    virtual bool is_known() const = 0;

    void dump() const;

    /**
     * Instantiate a generic element using the map from TypeVar -> Type
     * @param var_instances A map that assigns each type variable that is bound at this generic an instance
     * @return the instantiated type
     * @see TypeTable::create_spec_map()
     */
    Unifiable* instantiate(SpecializeMap& var_instances);
    Unifiable* specialize(SpecializeMap& map);

protected:
    std::string bound_vars_to_string() const;
    bool unify_bound_vars(thorin::ArrayRef<TypeVar>);
    void refine_bound_vars();
    bool bound_vars_known() const;

    /// like specialize but does not care about generics (this method is called by specialize)
    virtual Unifiable* vspecialize(SpecializeMap&) = 0;

private:
    /// raise error if a type does not implement the required traits;
    void verify_instantiation(SpecializeMap&) const;
    void set_representative(Unifiable* representative) { representative_ = representative; }

    TypeTable& typetable_;
    Unifiable* representative_;

protected:
    std::vector<TypeVar> bound_vars_;

    friend class TypeTable;
};

template<class T>
class TUnifiable : public Unifiable {
public:
    TUnifiable(TypeTable& tt)
        : Unifiable(tt)
    {}

    Proxy<T> instantiate(SpecializeMap& map) { return Proxy<T>(Unifiable::instantiate(map)->as<T>()); }
    Proxy<T> specialize(SpecializeMap& map) { return Proxy<T>(Unifiable::specialize(map)->as<T>()); }
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

//------------------------------------------------------------------------------

}

#endif
