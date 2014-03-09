/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : Unifiable(tt)
    , trait_decl_(trait_decl)
    , methods_()
{}

bool TraitInstanceNode::is_closed() const {
    // TODO review this
    for (auto i : var_instances_) {
        if (!i.second->is_closed())
            return false;
    }
    return true;
}

bool TraitNode::add_method(Symbol name, Type method_type) {
    assert(!is_unified() && "Unified traits must not be changed anymore!");
    if (methods_.contains(name)) {
        return false;
    } else {
        methods_[name] = method_type;
        return true;
    }
}

Type TraitNode::find_method(Symbol name) {
    auto it = methods_.find(name);
    return it == methods_.end() ? Type() : it->second;
}

Type TraitInstanceNode::find_method(Symbol name) {
    // FEATURE store specialized methods
    Type fn = trait()->find_method(name);
    if (fn.empty()) {
        return fn;
    } else {
        SpecializeMapping m = var_instances();
        return fn->instantiate(m);
    }

}

TraitNode* TraitNode::vspecialize(SpecializeMapping& mapping) {
    return is_generic() ? typetable().instantiate_trait(this, mapping) : this;
}

TraitNode* TraitInstanceNode::vspecialize(SpecializeMapping& mapping) {
    SpecializeMapping m;
    for (auto i : var_instances_)
        m[i.first] = i.second->gspecialize(mapping);

    return typetable().instantiate_trait(trait(), m);
}

}
