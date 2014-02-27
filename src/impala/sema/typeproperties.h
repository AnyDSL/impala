/*
 * type_properties.h
 *
 *  Created on: Jan 2, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef IMPALA_SEMA_TYPE_PROPERTIES_H
#define IMPALA_SEMA_TYPE_PROPERTIES_H

#include <unordered_set>
#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"

namespace impala {

class FnTypeNode;
class PrimTypeNode;
class Trait;
class TraitInstanceNode;
class TraitImpl;
class TupleTypeNode;
class TypeErrorNode;
class NoReturnTypeNode;
class TypeNode;
class TypeTable;
class TypeVarNode;

template<class T> class UnifiableProxy;

typedef UnifiableProxy<TypeNode> Type;
typedef UnifiableProxy<TypeErrorNode> TypeError;
typedef UnifiableProxy<NoReturnTypeNode> NoReturnType;
typedef UnifiableProxy<PrimTypeNode> PrimType;
typedef UnifiableProxy<FnTypeNode> FnType;
typedef UnifiableProxy<TupleTypeNode> TupleType;
typedef UnifiableProxy<TypeVarNode> TypeVar;
typedef UnifiableProxy<TraitInstanceNode> TraitInstance;
// TODO I don't think we need this set - we can grab the super traits via TraitDecl
typedef std::unordered_set<const Trait*> TraitSet;

//------------------------------------------------------------------------------

template<class T>
class Unifiable {
public:
    Unifiable(T* node)
        : representative_(node)
        , unified_(false)
    {}

    // Unified TypeNodes are removed by the type table
    ~Unifiable() { if (!is_unified()) delete representative_; }

    T* representative() const { return representative_; }
    template<class U> bool equal(Unifiable<U>* other) const {
        assert(this->is_unified() && other->is_unified());
        return this->representative() == other->representative();
    }
    template<class U> operator Unifiable<U>() { 
        // TODO static assert that U is a super type of T
        return Unifiable<U>((U*) representative_, unified_); 
    }
    bool is_unified() const { return unified_; }
    template<class U> Unifiable<U>* isa() { 
        return representative_->template isa<U>() ? reinterpret_cast<Unifiable<U>*>(this) : nullptr; 
    }

private:
    void set_representative(T* repr) {
        assert(!is_unified());
        delete representative_;
        representative_ = repr;
        unified_ = true;
    }

    void set_unified() {
        assert(!unified_);
        unified_ = true;
    }

    T* representative_;
    bool unified_;

    friend class UnifiableProxy<T>;
};

template<class T>
class UnifiableProxy {
public:
    typedef T BaseType;

    UnifiableProxy()
        : node_(nullptr)
    {}
    UnifiableProxy(Unifiable<T>* node)
        : node_(node)
    {}
    UnifiableProxy(T* node)
        : node_(new Unifiable<T>(node))
    {}

    bool empty() const { return node_ == nullptr; }
    bool is_unified() const { return node()->is_unified(); }
    bool operator == (const UnifiableProxy<T>& other) const {
        if (!this->is_unified()) representative()->typetable().unify(*this);
        if (!other.is_unified()) representative()->typetable().unify(other);
        return node()->equal(other.node());
    }
    bool operator != (const UnifiableProxy<T>& other) { return !(*this == other); }
    T* operator -> () const { return deref(); }
    template<class U> operator UnifiableProxy<U>() { 
        // TODO static assert that U is a super type of T
        return UnifiableProxy<U>(reinterpret_cast<Unifiable<U>*>(node_)); 
    }
    template<class U> 
    UnifiableProxy<typename U::BaseType> isa() { 
        return UnifiableProxy<typename U::BaseType>(node_->template isa<typename U::BaseType>()); 
    }
    operator bool() { return node_ != nullptr; }

private:
    T* representative() const { return node()->representative(); }
    T* deref() const { return representative(); }
    Unifiable<T>* node() const { return node_; }
    void set_representative(T* repr) { node()->set_representative(repr); }
    void set_unified() { node_->set_unified(); }

    Unifiable<T>* node_;

    friend class TypeTable;
    friend class TypeNode;
    friend class TypeVarNode;
    friend class TraitInstanceNode;
    friend class TraitInstanceHash;
    friend class TraitInstanceEqual;
    friend void verify(thorin::ArrayRef<const Type> types);
};

template<class T>
std::ostream& operator << (std::ostream& o, UnifiableProxy<T> u) { return o << u->to_string(); }

//------------------------------------------------------------------------------

class Generic : public thorin::MagicCast<Generic> {
public:
    TypeTable& typetable() const { return typetable_; }
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

protected:
    Generic(TypeTable& tt) : typetable_(tt) {}

    std::string bound_vars_to_string() const;

    std::vector<TypeVar> bound_vars_;

private:
    TypeTable& typetable_;
};

}

#endif
