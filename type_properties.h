/*
 * type_properties.h
 *
 *  Created on: Jan 2, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef TYPE_PROPERTIES_H_
#define TYPE_PROPERTIES_H_

#include <vector>

#include "thorin/util/array.h"
#include "thorin/util/cast.h"

class TypeVarNode;

typedef thorin::ArrayRef<TypeVarNode*> TypeVarNodeArray;

class GenericElement : public thorin::MagicCast<GenericElement> {
protected:
    std::vector<TypeVarNode*> bound_vars_;

    std::string bound_vars_to_string() const;

public:
    size_t num_bound_vars() const { return bound_vars_.size(); }

    TypeVarNodeArray bound_vars() const { return TypeVarNodeArray(bound_vars_); }
    TypeVarNode* bound_var(size_t i) const { return bound_vars_[i]; }

    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }

    void add_bound_var(TypeVarNode* v);

    virtual bool equal(const GenericElement*) const = 0;
    virtual size_t hash() const = 0;
};

template<class T>
class Unifiable {
public:
    /**
     * Get the unambiguous representative of this type.
     * All operations should only be done with this representative.
     *
     * (representative == nullptr) means that this type has not yet been unified.
     * Otherwise it either points to itself or to another type.
     */
    const T* get_representative() const {
        assert((representative_ == nullptr) || representative_->is_final_representative());
        return representative_;
    }

    bool is_final_representative() const {
        return representative_ == this;
    }

    /// @see get_representative()
    bool is_unified() const { return get_representative() != nullptr; }

protected:
    Unifiable()
        : representative_(nullptr)
    {}

    /// @see get_representative()
    void set_representative(const T* repr) {
        // TODO does this really hold? (is it set only once?)
        assert(representative_ == nullptr);
        representative_ = repr;
        assert(representative_->is_final_representative());
    }

private:
    const T* representative_;
};

/**
 * This class acts as a proxy for \p TypeNode pointers.
 * This proxy hides that a \p TypeNode may have been replaced by another one.
 * It automatically forwards to the replaced node.
 * If in doubt use a \p UnifiableProxy instead of \p TypeNode*.
 * You almost never have to use a \p TypeNode* directly.
 */
template<class T>
class UnifiableProxy {
public:
    UnifiableProxy()
        : node_(nullptr)
    {}
    UnifiableProxy(T* node)
        : node_(node)
    {}

    bool empty() const { return node_ == nullptr; }
    T* node() const { return node_; }
    T* deref() const {
        T* r = node()->get_representative();
        return (r == nullptr) ? node() : r;
    }
    T* operator *() const { return deref(); }
    bool operator == (const T* other) const { return this->deref() == other; }
    operator T*() const { return deref(); }
    T* operator -> () const { return deref(); }

private:
    mutable T* node_;
};

#endif /* TYPE_PROPERTIES_H_ */
