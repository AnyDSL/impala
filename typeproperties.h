/*
 * type_properties.h
 *
 *  Created on: Jan 2, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef TYPE_PROPERTIES_H_
#define TYPE_PROPERTIES_H_

#include <unordered_set>
#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/autoptr.h"
#include "thorin/util/cast.h"

class TypeTable;
class TypeNode;
template<class T> class UnifiableProxy;

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

    T* representative() const { return representative_; }

    template<class U> bool equal(Unifiable<U> other) const {
        if (this->is_unified() && other.is_unified()) {
            return this->representative() == other.representative();
        }
        return representative()->equal(other.representative());
    }

    template<class U> operator Unifiable<U>() { return Unifiable<U>((U*) representative_, unified_); }

    /// @see representative()
    bool is_unified() const { return unified_; }

private:
    void set_representative(T* repr) {
        assert(!is_unified());
        representative_ = repr;
        unified_ = true;
    }

    void set_unified() {
        assert(!unified_);
        unified_ = true;
    }

    thorin::AutoPtr<T> representative_;
    bool unified_;

    friend class UnifiableProxy<T>;
};

template<class T>
class UnifiableProxy {
public:
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
    T* representative() const { return node()->representative(); }
    bool is_unified() const { return node()->is_unified(); }
    T* deref() const { return node_->representative(); }
    T* operator *() const { return deref(); }
    bool operator == (const T* other) const { return this->deref()->equal(other.deref());; }
    operator T*() const { return deref(); }
    T* operator -> () const { return deref(); }
    template<class U> operator UnifiableProxy<U>() { return UnifiableProxy<U>((Unifiable<U>*) node_); }

private:
    Unifiable<T>* node() const { return node_; }
    void set_representative(T* repr) { node()->set_representative(repr); }
    void set_unified() { node_->set_unified(); }

    Unifiable<T>* node_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class FnTypeNode;
class PrimTypeNode;
class TupleTypeNode;
class TypeVarNode;
class TypeNode;
class TypeErrorNode;

class TypeTrait;
class TypeTraitInstanceNode;

class TypeTable;

typedef UnifiableProxy<TypeNode> Type;
typedef UnifiableProxy<TypeErrorNode> TypeError;
typedef UnifiableProxy<PrimTypeNode> PrimType;
typedef UnifiableProxy<FnTypeNode> FnType;
typedef UnifiableProxy<TupleTypeNode> TupleType;
typedef UnifiableProxy<TypeVarNode> TypeVar;
typedef UnifiableProxy<TypeTraitInstanceNode> TypeTraitInstance;
typedef std::unordered_set<const TypeTrait*> TypeTraitSet;

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

#endif
