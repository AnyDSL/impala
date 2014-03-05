#ifndef IMPALA_SEMA_GENERIC_H
#define IMPALA_SEMA_GENERIC_H

#include <unordered_set>
#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"

namespace impala {

class TypeTable;
template<class T> class Unifiable;

template<class T>
class Proxy {
public:
    Proxy()
        : node_(nullptr)
    {}
    Proxy(T* node)
        : node_(node)
    {}

    bool empty() const { return node_ == nullptr; }
    bool is_unified() const { return deref()->is_unified(); }
    bool operator == (const Proxy<T>& other) const {
        assert(&node()->typetable() == &other.node()->typetable());
        if (!this->is_unified()) node()->typetable().unify(*this);
        if (!other.is_unified()) node()->typetable().unify(other);
        return deref() == other.deref();
    }
    bool operator != (const Proxy<T>& other) { return !(*this == other); }
    T* operator -> () const { return deref(); }
    operator T* () const { return deref(); }
    template<class U> operator Proxy<U>() {
        // TODO static assert that U is a super type of T
        return Proxy<U>((U*) node_);
    }
    T* deref() const { return node_->is_unified() ? representative() : node_->template as<T>(); }
    //Proxy<T>& operator = (T* other) { node_ = other; return *this; } CHECK isn't this just adding confusion?
    T* representative() const { return node_->representative()->template as<T>(); }
    T* node() const { return node_; }
    // FEATURE make most of this stuff private!

private:
    T* node_;

    friend class TypeNode;
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

template<class T> struct NodeHash {
    size_t operator () (const T t) const { return thorin::hash_value(t.node()); }
};
template<class T> struct NodeEqual {
    bool operator () (const T t1, const T t2) const { return t1.node() == t2.node(); }
};
template<class T> using NodeSet = std::unordered_set<T, NodeHash<T>, NodeEqual<T>>;

//------------------------------------------------------------------------------

class Generic { // : public thorin::MagicCast<Generic> { CHECK how do we add casts here?
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
    /// raise error if a type does not implement the required traits;
    void check_instantiation(thorin::ArrayRef<Type>) const;
    std::string bound_vars_to_string() const;

protected:
    std::vector<TypeVar> bound_vars_;
    TypeTable& typetable_;
};

template<class T>
class Unifiable : public Generic, public thorin::MagicCast<T> {
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
    virtual bool equal(const Unifiable<T>* u) const {
        // TODO
        return true;
    }
    virtual size_t hash() const = 0;

private:
    T* representative_;

    void set_representative(T* representative) { representative_ = representative; }

protected:
    std::vector<TypeVar> bound_vars_;

    friend class TypeTable;
};

}

#endif
