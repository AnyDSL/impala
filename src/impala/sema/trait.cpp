/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

void Trait::add_method(const std::string name, FnType type) {
    assert(type.is_unified() && "Method types must be closed");
    assert(type->is_closed());
    TraitMethod* m = new TraitMethod();
    m->name = name;
    m->type = type;
    methods_.push_back(m);
}

TraitInstanceNode::TraitInstanceNode(TypeTable& typetable, const Trait* trait, thorin::ArrayRef<Type> var_instances)
    : Unifiable<TraitInstanceNode>(typetable)
    , trait_(trait)
    , var_instances_(var_instances.size())
{
    trait->check_instantiation(var_instances);
    size_t i = 0;
    for (auto elem : var_instances)
        var_instances_[i++] = elem;
}

bool TraitInstanceNode::is_closed() const {
    for (auto i : var_instances_) {
        if (!i->is_closed())
            return false;
    }
    return true;
}

TraitInstance TraitInstanceNode::specialize(SpecializeMapping& mapping) const {
    std::vector<Type> instances;
    for (auto i : var_instances())
        instances.push_back(i->specialize(mapping));

    return typetable().instantiate_trait(trait(), instances);
}

}
