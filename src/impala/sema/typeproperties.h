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
class TupleTypeNode;
class TypeVarNode;
class TypeNode;
class TypeErrorNode;

class TypeTrait;
class TypeTraitInstanceNode;

class TypeTable;

template<class T> class UnifiableProxy;

typedef UnifiableProxy<TypeNode> Type;
typedef UnifiableProxy<TypeErrorNode> TypeError;
typedef UnifiableProxy<PrimTypeNode> PrimType;
typedef UnifiableProxy<FnTypeNode> FnType;
typedef UnifiableProxy<TupleTypeNode> TupleType;
typedef UnifiableProxy<TypeVarNode> TypeVar;
typedef UnifiableProxy<TypeTraitInstanceNode> TypeTraitInstance;
typedef std::unordered_set<const TypeTrait*> TypeTraitSet;

//------------------------------------------------------------------------------

template<class T>
class Unifiable {
public:
    Unifiable(T* node)
        : representative_(node)
        , unified_(false)
    {}
    Unifiable(T* node, bool unified)
        : representative_(node)
        , unified_(unified)
    {}

    // Unified TypeNodes are removed by the type table
    ~Unifiable() { if (!is_unified()) delete representative_; }

    T* representative() const { return representative_; }

    template<class U> bool equal(Unifiable<U>* other) const {
        assert(this->is_unified() && other->is_unified());
        return this->representative() == other->representative();
    }

    template<class U> operator Unifiable<U>() { return Unifiable<U>((U*) representative_, unified_); }

    /// @see representative()
    bool is_unified() const { return unified_; }

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
    UnifiableProxy()
        : node_(nullptr)
    {}
    // TODO do we need both contructors?
    UnifiableProxy(Unifiable<T>* node)
        : node_(node)
    {}
    // TODO do we need both contructors?
    UnifiableProxy(T* node)
        : node_(new Unifiable<T>(node))
    {}

    bool empty() const { return node_ == nullptr; }
    bool is_unified() const { return node()->is_unified(); }
    bool operator == (const UnifiableProxy<T>& other) {
        if (!this->is_unified()) representative()->typetable().unify(*this);
        if (!other.is_unified()) representative()->typetable().unify(other);
        return node()->equal(other.node());
    }
    bool operator != (const UnifiableProxy<T>& other) { return !(*this == other); }
    T* operator -> () const { return deref(); }
    template<class U> operator UnifiableProxy<U>() { return UnifiableProxy<U>((Unifiable<U>*) node_); }

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
    friend class TypeTraitInstanceNode;
    friend class TypeTraitInstanceHash;
    friend class TypeTraitInstanceEqual;
    friend void verify(thorin::ArrayRef<const Type> types);
};

//------------------------------------------------------------------------------

class GenericElement : public thorin::MagicCast<GenericElement> {
public:
    size_t num_bound_vars() const { return bound_vars_.size(); }
    thorin::ArrayRef<TypeVar> bound_vars() const { return thorin::ArrayRef<TypeVar>(bound_vars_); }
    TypeVar bound_var(size_t i) const { return bound_vars_[i]; }
    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }
    void add_bound_var(TypeVar v);
    virtual bool equal(const GenericElement*) const = 0;
    virtual size_t hash() const = 0;

protected:
    std::vector<TypeVar> bound_vars_;
    std::string bound_vars_to_string() const;
};

}

#endif
