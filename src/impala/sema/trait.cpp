/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl, thorin::ArrayRef<Trait> super_traits)
    : Unifiable(tt)
    , trait_decl_(trait_decl)
    , super_traits_()
    , methods_()
{
    for (auto t : super_traits) {
        assert(!t.empty());
        super_traits_.push_back(t);
    }
}

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
    assert(!methods_.contains(name));
    if (has_method(name)) { // this means a super trait already declares this method
        return false;
    } else {
        methods_[name] = method_type;
        return true;
    }
}

Type TraitNode::find_method(Symbol name) {
    auto it = methods_.find(name);
    if (it == methods_.end()) {
        for (Trait super : super_traits()) {
            Type t = super->find_method(name);
            if (!t.empty())
                return t;
        }
        return Type();
    } else {
        return it->second;
    }
}

Type TraitInstanceNode::find_method(Symbol name) {
    auto it = methods_.find(name);
    if (it != methods_.end()) {
        return it->second;
    } else {
        Type fn = trait()->find_method(name);
        if (fn.empty()) {
            return fn;
        } else {
            SpecializeMapping m = var_instances();
            Type t = fn->instantiate(m);
            typetable().unify(t);
            return methods_[name] = t;
        }
    }
}

const MethodTable TraitInstanceNode::methods() {
    if (methods_.size() < trait()->num_methods()) {
        for (auto p : trait()->methods())
            find_method(p.first); // this will insert the specialized method
    }
    assert(methods_.size() == trait()->num_methods());
    return methods_;
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
