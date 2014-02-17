/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/ast.h"
#include "impala/sema/trait.h"

namespace impala {

const std::string Trait::top_trait_name = std::string("");

bool Trait::equal(const GenericElement* other) const {
    // TODO is this correct for a instanceof-equivalent?
    if (const Trait* t = other->isa<Trait>())
        return equal(t);
    return false;
}

std::string Trait::to_string() const { return trait_decl()->symbol().str(); }

void Trait::add_method(const std::string name, FnType type) {
    assert(type.is_unified() && "Method types must be closed");
    assert(type->is_closed());
    TraitMethod* m = new TraitMethod();
    m->name = name;
    m->type = type;
    methods_.push_back(m);
}

TraitInstanceNode::TraitInstanceNode(const Trait* trait, thorin::ArrayRef<Type> var_instances)
    : trait_(trait)
    , var_instances_(var_instances.size())
{
    assert(var_instances.size() == trait->bound_vars().size() && "Wrong number of instances for bound type variables");
    size_t i = 0;
    for (auto elem : var_instances)
        var_instances_[i++] = elem;
}

bool TraitInstanceNode::equal(const TraitInstanceNode* other) const {
    // TODO use equal?
    if (trait_ != other->trait_)
        return false;

    assert(var_instances_.size() == other->var_instances_.size());
    for (size_t i = 0; i < var_instances_.size(); ++i) {
        if (! var_instances_[i].representative()->equal(other->var_instances_[i].representative())) {
            return false;
        }
    }
    return true;
}

// TODO better hash function
size_t TraitInstanceNode::hash() const { return trait_->hash(); }

bool TraitInstanceNode::is_closed() const {
    for (auto i : var_instances_) {
        if (!i->is_closed())
            return false;
    }
    return true;
}

}
