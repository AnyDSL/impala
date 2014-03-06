/*
 * generic.cpp
 *
 *  Created on: Mar 5, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/generic.h"

#include "thorin/util/assert.h"
#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

std::string Generic::bound_vars_to_string() const {
    std::string result;

    if (!is_generic())
        return result;

    const char* separator = "[";
    for (auto v : bound_vars()) {
        result += separator + v->to_string();

        const NodeSet<Trait> restr = v->bounds();

        if (!restr.empty()) {
            auto inner_sep = ":";
            for (auto t : restr) {
                result += inner_sep + t->to_string();
                inner_sep = "+";
            }
        }

        separator = ",";
    }
    return result + ']';
}

void Generic::add_bound_var(TypeVar v) {
    assert(!v->is_closed() && "Type variables already bound");

    // CHECK should variables only be bound in this case? does this also hold for traits?
    //assert(v->is_subtype(this) && "Type variables can only be bound at t if they are a subtype of t!");
    // CHECK should 'forall a, a' be forbidden?
    //assert(type->kind() != Type_var && "Types like 'forall a, a' are forbidden!");

    v->bind(this);
    bound_vars_.push_back(v);
}

void Generic::check_instantiation(thorin::ArrayRef<Type> var_instances) const {
    // TODO better error handling
    assert(is_generic() && "Can't instantiate non-generic element!");
    assert(var_instances.size() == bound_vars().size() && "Wrong number of instances for bound type variables");

    for (size_t i = 0; i < var_instances.size(); ++i) {
        Type instance = var_instances[i];

        for (Trait bound : bound_var(i)->bounds())
            // TODO better error handling
            assert(instance->implements(bound));
    }
}

Generic* Generic::ginstantiate(thorin::ArrayRef<Type> var_instances) {
    check_instantiation(var_instances);

    SpecializeMapping mapping;
    assert(num_bound_vars() == var_instances.size());
    size_t i = 0;
    for (TypeVar v : bound_vars())
        mapping[v.node()] = var_instances[i];

    Generic* instance = vspecialize(mapping);
    return instance;
}

Generic* Generic::specialize(SpecializeMapping& mapping) {
    // FEATURE this could be faster if we copy only types were something changed inside
    auto it = mapping.find(this);
    if (it != mapping.end())
        return it->second;

    for (TypeVar v : bound_vars()) {
        assert(mapping.find(v.node()) == mapping.end());
        mapping[v.node()] = v->clone(mapping);
    }

    Generic* t = vspecialize(mapping);

    for (auto v : bound_vars()) {
        assert(mapping.find(v.node()) != mapping.end());
        t->add_bound_var(TypeVar(mapping[v.node()]->as<TypeVarNode>()));
    }

    return t;
}

}
