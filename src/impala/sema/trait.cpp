/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"
#include "impala/ast.h"

namespace impala {

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : Unifiable(tt)
    , trait_decl_(trait_decl)
    , super_traits_()
    , all_methods_()
    , declared_methods_()
{}

void TraitNode::add_super_trait(Trait t) {
    typetable().unify(t);
    super_traits_.insert(t);
    super_traits_.insert(t->super_traits().begin(), t->super_traits().end());

    for (Symbol mname : t->declared_methods()) {
        assert(t->has_method(mname));
        if (!add_method(mname, t->find_method(mname), true))
            typetable().error(this->trait_decl()) << "conflicting method name in super traits: '" << mname << "'\n";
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

bool TraitNode::add_method(Symbol name, Type method_type, bool inherited) {
    assert(!is_unified() && "Unified traits must not be changed anymore!");
    if (has_method(name)) {
        return false;
    } else {
        all_methods_[name] = method_type;
        if (!inherited)
            declared_methods_.push_back(name);
        return true;
    }
}

Type TraitNode::find_method(Symbol name) {
    auto it = all_methods_.find(name);
    return (it == all_methods_.end()) ? Type() : it->second;
}

Type TraitInstanceNode::find_method(Symbol name) {
    auto it = all_methods_.find(name);
    if (it != all_methods_.end()) {
        return it->second;
    } else {
        Type fn = trait()->find_method(name);
        if (fn.empty()) {
            return fn;
        } else {
            SpecializeMapping m = var_instances();
            Type t = fn->instantiate(m);
            typetable().unify(t);
            return all_methods_[name] = t;
        }
    }
}

const MethodTable& TraitInstanceNode::all_methods() {
    if (all_methods_.size() < trait()->num_methods()) {
        for (auto p : trait()->all_methods())
            find_method(p.first); // this will insert the specialized method
    }
    assert(all_methods_.size() == trait()->num_methods());
    return all_methods_;
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
