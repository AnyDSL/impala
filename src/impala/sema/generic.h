#ifndef IMPALA_SEMA_GENERIC_H
#define IMPALA_SEMA_GENERIC_H

#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

namespace impala {

class TypeTable;
template<class T> class Unifiable;

template<class T> struct NodeHash {
    size_t operator () (const T t) const { return thorin::hash_value(t.node()); }
};
template<class T> struct NodeEqual {
    bool operator () (const T t1, const T t2) const { return t1.node() == t2.node(); }
};
template<class T> using NodeSet = thorin::HashSet<T, NodeHash<T>, NodeEqual<T>>;

template<class T> struct UniHash {
    size_t operator () (const T t) const { return t->hash(); }
};
template<class T> struct UniEqual {
    bool operator () (const T t1, const T t2) const { return (t1->is_unified() && t2->is_unified()) ? t1 == t2 : t1->equal(*t2); }
};
template<class T> using UniSet = thorin::HashSet<T, UniHash<T>, UniEqual<T>>;

class TypeNode;

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
        node()->typetable().unify(*this);
        node()->typetable().unify(other);
        return representative() == other.representative();
    }
    bool operator != (const Proxy<T>& other) const { assert(node_ != nullptr); return !(*this == other); }
    T* representative() const { assert(node_ != nullptr); return node_->representative()->template as<T>(); }
    T* node() const { assert(node_ != nullptr); return node_; }
    T* operator  * () const { assert(node_ != nullptr); return node_->is_unified() ? representative() : node_->template as<T>(); }
    T* operator -> () const { assert(node_ != nullptr); return *(*this); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "R is not a base type of L");
        assert(node_ != nullptr); return Proxy<U>((U*) node_);
    }
    template<class U> Proxy<typename U::BaseType> isa() { 
        assert(node_ != nullptr); return Proxy<typename U::BaseType>(node_->isa<typename U::BaseType>()); 
    }
    template<class U> Proxy<typename U::BaseType> as() { 
        assert(node_ != nullptr); return Proxy<typename U::BaseType>(node_->as<typename U::BaseType>()); 
    }
    operator bool() { return !empty(); }
    Proxy<T>& operator= (Proxy<T> other) { /* assert(node_ == nullptr); CHECK do we need this? */ node_ = *other; return *this; }
    void clear() { node_ = nullptr; }

private:
    T* node_;
};

class TypeVarNode;
class TraitNode;
class TraitImplNode;
typedef Proxy<TypeNode> Type;
typedef Proxy<TypeVarNode> TypeVar;
typedef Proxy<TraitNode> Trait;
typedef Proxy<TraitImplNode> TraitImpl;

class Generic;
typedef thorin::HashMap<const Generic*, Generic*> SpecializeMapping;

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

    Generic* ginstantiate(SpecializeMapping& var_instances);
    Generic* gspecialize(SpecializeMapping&); // TODO one could always assert that this is only called on final representatives!

    /// like specialize but does not care about generics (this method is called by specialize)
    virtual Generic* vspecialize(SpecializeMapping&) = 0;

private:
    /// raise error if a type does not implement the required traits;
    void check_instantiation(SpecializeMapping&) const;

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

    /**
     * Instantiate a generic element using the mapping from TypeVar -> Type
     * @param var_instances A mapping that assigns each type variable that is bound at this generic an instance
     * @return the instantiated type
     * @see TypeTable::create_spec_mapping()
     */
    Proxy<T> instantiate(SpecializeMapping& var_instances) {
//        assert(is_final_representative()); CHECK it seems nothing gets broken w.o. this assertion - still I don't feel comfortable w.o. it
        // we can not unify yet because it could be that this type is not closed yet
        return Proxy<T>(ginstantiate(var_instances)->as<T>());
    }
    /**
     * if this element is in the mapping return the mapped one;
     * otherwise copy this element with specialized sub-elements
     */
    Proxy<T> specialize(SpecializeMapping& mapping) {
//        assert(is_final_representative()); CHECK it seems nothing gets broken w.o. this assertion - still I don't feel comfortable w.o. it
        return Proxy<T>(gspecialize(mapping)->as<T>());
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
