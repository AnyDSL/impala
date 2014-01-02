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

class TypeVar;

typedef thorin::ArrayRef<const TypeVar*> TypeVarArray;

class GenericElement : public thorin::MagicCast<GenericElement> {
protected:
    std::vector<const TypeVar*> bound_vars_;

    std::string bound_vars_to_string() const;

public:
    size_t num_bound_vars() const { return bound_vars_.size(); }

    TypeVarArray bound_vars() const { return TypeVarArray(bound_vars_); }
    const TypeVar* bound_var(size_t i) const { return bound_vars()[i]; }

    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const { return !bound_vars_.empty(); }

    void add_bound_var(TypeVar* v);

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

#endif /* TYPE_PROPERTIES_H_ */
