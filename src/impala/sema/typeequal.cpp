/*
 * typeequal.cpp
 *
 *  Created on: Feb 24, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

size_t TypeNode::hash() const {
    // FEATURE take type variables of generic types better into the equation
    size_t seed = hash_combine(hash_value((int) kind()), size());
    seed = hash_combine(seed, num_bound_vars());
    for (auto elem : elems_)
        seed = hash_combine(seed, elem->hash());

    return hash_combine(seed, 0); // 0 because this is a TypeNode
}

size_t TraitNode::hash() const {
    return hash_combine(hash_value(trait_decl()), 1); // 1 because this is a TraitNode
}

// FEATURE better hash function
size_t TraitInstanceNode::hash() const { return trait()->hash(); }

size_t TraitImplNode::hash() const {
    return hash_combine(hash_value(impl_decl()), 2); // 2 because this is a TraitImplNode
}

//----------------------------------------------------------------------------------------

bool TypeNode::equal(const TypeNode* other) const {
    //assert(this != other && "double insert"); // TODO what happens in this case?
    bool result = this->kind() == other->kind();
    result &= this->size() == other->size();
    result &= this->num_bound_vars() == other->num_bound_vars();

    if (!result)
        return false;

    // CHECK is deref below correct? -- two times below!
    // set equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i)
        this->bound_var(i)->set_equiv_variable(*other->bound_var(i));

    // check equality of the restrictions of the type variables
    for (size_t i = 0, e = num_bound_vars(); i != e && result; ++i)
        result &= this->bound_var(i)->bounds_equal(other->bound_var(i));

    for (size_t i = 0, e = size(); i != e && result; ++i)
        result &= this->elem(i)->equal(*other->elem(i));

    // unset equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i)
        this->bound_var(i)->unset_equiv_variable();

    return result;
}

bool TypeVarNode::bounds_equal(const TypeVar other) const {
    if (this->bounds().size() != other->bounds().size())
        return false;

    // FEATURE this works but seems too much effort, at least use a set that uses representatives
    TypetableSet<TraitNode> obounds;
    for (Trait r : other->bounds()) {
        auto p = obounds.insert(*r); // TODO is deref here and below correct?
        assert(p.second && "hash/equal broken");
    }

    // this->bounds() subset of trestr
    for (Trait r : this->bounds()) {
        if (obounds.find(*r) == obounds.end())
            return false;
    }

    return true;
}

bool TypeVarNode::equal(const TypeNode* other) const {
    if (this == other)
        return true;

    if (const TypeVarNode* t = other->isa<TypeVarNode>()) {
        if ((this->equiv_var_ == nullptr) && (t->equiv_var_ == nullptr)) {
            if (this->bound_at() == nullptr) { // unbound type vars are by definition unequal
                return false;
            } else {
                // two type vars are equal if the types where they are bound are
                // equal and they are bound at the same position
                bool result = bound_at()->num_bound_vars() == t->bound_at()->num_bound_vars();
                size_t i;
                for (i = 0; (i < bound_at()->num_bound_vars()) && result; ++i) {
                    if (bound_at()->bound_var(i).node() == this) { // CHECK is node() here and below correct?
                        result &= t->bound_at()->bound_var(i).node() == t;
                        break;
                    }
                }
                assert(i < bound_at()->num_bound_vars()); // it should have been found!

                return result && bound_at()->equal(t->bound_at());
            }

        } else {
            // we do not use && because for performance reasons we only set the
            // equiv_var on one side (even the right side of the || should never
            // be executed)
            return (this->equiv_var_ == t) || (t->equiv_var_ == this);
        }
    }
    return false;
}

bool TraitNode::equal(const TraitNode* other) const {
    return (this->trait_decl() == other->trait_decl()) && (this->num_bound_vars() == other->num_bound_vars());
}

bool TraitInstanceNode::equal(const TraitNode* other) const {
    if (auto instance = other->isa<TraitInstanceNode>()) {
        if (trait() != instance->trait())
            return false;

        assert(var_instances_.size() == instance->var_instances_.size());
        for (auto p : var_instances_) {
            assert(instance->var_instances_.find(p.first) != instance->var_instances_.end());
            if (! p.second->equal(instance->var_instances_.find(p.first)->second)) {
                return false;
            }
        }
        return true;
    }
    return false;
}

}
