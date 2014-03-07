/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

bool TraitInstanceNode::is_closed() const {
    // TODO review this
    for (auto i : var_instances_) {
        if (!i.second->is_closed())
            return false;
    }
    return true;
}

TraitNode* TraitNode::vspecialize(SpecializeMapping& mapping) {
    return is_generic() ? typetable().instantiate_trait(this, mapping) : this;
}

TraitNode* TraitInstanceNode::vspecialize(SpecializeMapping& mapping) {
    SpecializeMapping m;
    for (auto i : var_instances())
        m[i.first] = i.second->specialize(mapping);

    return typetable().instantiate_trait(trait(), m);
}

}
