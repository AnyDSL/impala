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

template<class T>
class Unifiable {
public:
    Unifiable()
        : node_(nullptr)
        , unified_(false)
    {}

    Unifiable(T* node)
        : node_(node)
        , unified_(false)
    {}

    bool empty() const { return node_ == nullptr; }
    T* node() const { return node_; }
    T* get_representative() const { return node(); }

    template<class U> bool equal(Unifiable<U> other) const {
        if (this->is_unified() && other.is_unified()) {
            return this->node() == other.node();
        }
        return node()->equal(other.node());
    }

    T* operator *() const { return node(); }
    bool operator == (const T* other) const { return this->node() == other; }
    operator T*() const { return node(); }
    T* operator -> () const { return node(); }

    template<class U> operator Unifiable() { return UnificationProxy(node_); }

    /// @see get_representative()
    bool is_unified() const { return unified_; }

private:
    void set_representative(T* repr) {
        // TODO does this really hold? (is it set only once?)
        assert(!unified_);
        node_ = repr;
        unified_ = true;
    }

    void set_unified() {
        assert(!unified_);
        unified_ = true;
    }

    T* node_;
    bool unified_;

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

typedef Unifiable<TypeNode> Type;
typedef Unifiable<TypeErrorNode> TypeError;
typedef Unifiable<PrimTypeNode> PrimType;
typedef Unifiable<FnTypeNode> FnType;
typedef Unifiable<TupleTypeNode> TupleType;
typedef Unifiable<TypeVarNode> TypeVar;
typedef Unifiable<TypeTraitInstanceNode> TypeTraitInstance;

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
