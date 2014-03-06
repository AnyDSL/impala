#ifndef IMPALA_SEMA_GENERIC_H
#define IMPALA_SEMA_GENERIC_H

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"

namespace impala {

class TypeTable;
template<class T> class Unifiable;

template<class T> struct NodeHash {
    size_t operator () (const T t) const { return thorin::hash_value(t.node()); }
};
template<class T> struct NodeEqual {
    bool operator () (const T t1, const T t2) const { return t1.node() == t2.node(); }
};
template<class T> using NodeSet = std::unordered_set<T, NodeHash<T>, NodeEqual<T>>;

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
        assert(&node()->typetable() == &other.node()->typetable());
        if (!node()->is_unified()) node()->typetable().unify(*this);
        if (!other->is_unified()) node()->typetable().unify(other);
        return representative() == other.representative();
    }
    bool operator != (const Proxy<T>& other) const { return !(*this == other); }
    //operator T* () const { return deref(); } // CHECK shouldn't we remove this?
    T* operator -> () const { return deref(); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "R is not a base type of L");
        return Proxy<U>((U*) node_);
    }
    template<class U> 
    Proxy<typename U::BaseType> isa() { return Proxy<typename U::BaseType>(node_->isa<typename U::BaseType>()); }
    template<class U> 
    Proxy<typename U::BaseType> as() { return Proxy<typename U::BaseType>(node_->as<typename U::BaseType>()); }
    operator bool() { return !empty(); }

private:
    T* deref() const { return node_->is_unified() ? representative() : node_->template as<T>(); }
    T* representative() const { return node_->representative()->template as<T>(); }
    T* node() const { return node_; }
    T* node_;

    friend class Generic;
    friend class TypeNode;
    friend class PrimTypeNode;
    friend class NoReturnTypeNode;
    friend class TypeErrorNode;
    friend class FnTypeNode;
    friend class TupleTypeNode;
    friend class TypeVarNode;
    friend class TraitInstanceNode;
    friend struct NodeHash<Proxy<T>>;
    friend struct NodeEqual<Proxy<T>>;
    friend class TypeTable;
};

class TypeNode;
class TypeVarNode;
class TraitNode;
class TraitImplNode;
typedef Proxy<TypeNode> Type;
typedef Proxy<TypeVarNode> TypeVar;
typedef Proxy<TraitNode> Trait;
typedef Proxy<TraitImplNode> TraitImpl;

class Generic;
typedef std::unordered_map<const Generic*, Generic*> SpecializeMapping;

//------------------------------------------------------------------------------

class Generic : public thorin::MagicCast<Generic> {
protected:
    Generic(TypeTable& tt)
        : typetable_(tt)
    {}

public:
    TypeTable& typetable() const { return typetable_; }
    size_t num_bound_vars() const { return bound_vars_.size(); }
    thorin::ArrayRef<TypeVar> bound_vars() const { return thorin::ArrayRef<TypeVar>(bound_vars_); }
    TypeVar bound_var(size_t i) const { return bound_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }
    virtual bool is_closed() const = 0; // TODO
    void add_bound_var(TypeVar v);
    virtual bool equal(const Generic*) const = 0;
    virtual size_t hash() const = 0;
    std::string bound_vars_to_string() const;
    virtual std::string to_string() const = 0;

    void dump() const;

protected:
    std::vector<TypeVar> bound_vars_;
    TypeTable& typetable_;

    Generic* ginstantiate(thorin::ArrayRef<Type> var_instances);
    /**
     * if this element is in the mapping return the mapped one;
     * otherwise copy this element with specialized sub-elements
     */
    Generic* specialize(SpecializeMapping&); // TODO one could always assert that this is only called on final representatives!

    /// like specialize but does not care about generics (this method is called by specialize)
    virtual Generic* vspecialize(SpecializeMapping&) = 0;

private:
    /// raise error if a type does not implement the required traits;
    SpecializeMapping check_instantiation(thorin::ArrayRef<Type>) const;

    friend class TypeVarNode;
    friend class TraitInstanceNode;
};

template<class T>
class Unifiable : public Generic {
protected:
    Unifiable(TypeTable& tt)
        : Generic(tt)
        , representative_(nullptr)
    {
        static_assert(std::is_base_of<Unifiable<T>, T>::value, "Unifiable<T> is not a base type of T");
    }

public:
    T* representative() const { return representative_; }
    bool is_final_representative() const { return representative() == this->template as<T>(); }
    bool is_unified() const { return representative_ != nullptr; }
    virtual bool equal(const T*) const = 0;
    virtual bool equal(const Generic* other) const {
        if (const T* t = other->isa<T>())
            return equal(t);
        return false;
    }

    Proxy<T> instantiate(thorin::ArrayRef<Type> var_instances) {
        Proxy<T> u = Proxy<T>(ginstantiate(var_instances)->as<T>());
        typetable().unify(u);
        return u;
    }

private:
    T* representative_;

    void set_representative(T* representative) { representative_ = representative; }

protected:
    std::vector<TypeVar> bound_vars_;

    friend class TypeTable;
};

template<class T>
std::ostream& operator << (std::ostream& o, Proxy<T> u) { return o << u->to_string(); }

}

#endif
