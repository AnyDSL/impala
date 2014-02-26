#ifndef IMPALA_SEMA_GENERIC_H
#define IMPALA_SEMA_GENERIC_H

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
    bool operator == (const Proxy<T>& other) const;
    //{
        //if (!this->is_unified()) deref()->typetable().unify(*this);
        //if (!other.is_unified()) deref()->typetable().unify(other);
        //return deref() == other.deref();
    //}
    bool operator != (const Proxy<T>& other) { return !(*this == other); }
    T* operator -> () const { return deref(); }
    operator T* () const { return deref(); }
    T* deref() const { return node_->representative()->template as<T>(); }
    Proxy<T>& operator = (T* other) { node_ = other; return *this; }

private:
    T* node_;
};

class TypeNode;
class TypeVarNode;
typedef Proxy<TypeNode> Type;
typedef Proxy<TypeVarNode> TypeVar;

template<class T>
class Unifiable : public thorin::MagicCast<T> {
protected:
    Unifiable(TypeTable& tt) 
        : typetable_(tt) 
        , representative_((T*) this)
        , unified_(false)
    {
        static_assert(std::is_base_of<Unifiable<T>, T>::value, "Unifiable<T> is not a base type of T");
    }

public:
    TypeTable& typetable() const { return typetable_; }
    T* representative() const { return representative_; }
    bool is_unified() const { return unified_; }
    virtual bool equal(const Unifiable<T>* u) const { 
        // todo;
        return true;
    }
    virtual size_t hash() const = 0;

private:
    TypeTable& typetable_;
    T* representative_;
    bool unified_;

protected:
    std::vector<TypeVar> bound_vars_;
};

template<class T>
class Generic : public Unifiable<T> {
protected:
    Generic(TypeTable& tt) 
        : Unifiable<T>(tt) 
    {}

public:
    size_t num_bound_vars() const { return bound_vars_.size(); }
    thorin::ArrayRef<TypeVar> bound_vars() const { return thorin::ArrayRef<TypeVar>(bound_vars_); }
    TypeVar bound_var(size_t i) const { return bound_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }
    void add_bound_var(TypeVar v);
    virtual bool equal(const Generic*) const = 0;
    virtual size_t hash() const = 0;
    /// raise error if a type does not implement the required traits;
    void check_instantiation(thorin::ArrayRef<Type>) const;
    std::string bound_vars_to_string() const;

protected:
    std::vector<TypeVar> bound_vars_;

};

}

#endif
