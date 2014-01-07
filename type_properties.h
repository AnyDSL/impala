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

    T* get_representative() const { return representative_; }

    template<class U> bool equal(Unifiable<U> other) const {
        if (this->is_unified() && other.is_unified()) {
            return this->get_representative() == other.get_representative();
        }
        return get_representative()->equal(other.get_representative());
    }

    template<class U> operator Unifiable<U>() { return Unifiable<U>((U*) representative_, unified_); }

    /// @see get_representative()
    bool is_unified() const { return unified_; }

private:
    void set_representative(T* repr) {
        // TODO does this really hold? (is it set only once?)
        assert(!unified_);
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

    UnifiableProxy(Unifiable<T>* node)
        : node_(node)
    {}

    UnifiableProxy(T* node)
        : node_(new Unifiable<T>(node))
    {}

    bool empty() const { return node_ == nullptr; }
    T* deref() const { return node_->get_representative(); }
    T* operator *() const { return deref(); }
    bool operator == (const T* other) const { return deref() == other; } // TODO
    operator T*() const { return deref(); }
    T* operator -> () const { return deref(); }

    template<class U> operator UnifiableProxy<U>() { return UnifiableProxy<U>((Unifiable<U>*) node_); }

    template<class U> bool equal(const UnifiableProxy<U>* other) const { return node()->equal(other->node()); }
    T* get_representative() const { return node()->get_representative(); }
    bool is_unified() const { return node()->is_unified(); }

    Unifiable<T>* node() const { return node_; } // TODO make private
private:


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

typedef thorin::ArrayRef<Type> TypeArray;
typedef thorin::ArrayRef<TypeNode*> TypeNodeArray;
typedef thorin::ArrayRef<TypeVar> TypeVarArray;
typedef thorin::ArrayRef<TypeVarNode*> TypeVarNodeArray;
//typedef thorin::ArrayRef<const TypeTraitInstance*> TypeTraitInstArray;

typedef std::unordered_set<const TypeTrait*> TypeTraitSet;

//------------------------------------------------------------------------------

class GenericElement : public thorin::MagicCast<GenericElement> {
protected:
    std::vector<TypeVar> bound_vars_;

    std::string bound_vars_to_string() const;

public:
    size_t num_bound_vars() const { return bound_vars_.size(); }

    TypeVarArray bound_vars() const { return TypeVarArray(bound_vars_); }
    TypeVar bound_var(size_t i) const { return bound_vars_[i]; }

    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }

    void add_bound_var(TypeVar v);

    virtual bool equal(const GenericElement*) const = 0;
    virtual size_t hash() const = 0;
};

#endif /* TYPE_PROPERTIES_H_ */
