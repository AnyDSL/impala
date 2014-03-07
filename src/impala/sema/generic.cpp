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

        const UniSet<Trait> restr = v->bounds();

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

SpecializeMapping Generic::check_instantiation(thorin::ArrayRef<Type> var_instances) const {
    // TODO better error handling
    assert(is_generic() && "Can't instantiate non-generic element!");
    assert(var_instances.size() == num_bound_vars() && "Wrong number of instances for bound type variables");

    // create a mapping TypeVar -> Type
    SpecializeMapping mapping;
    size_t i = 0;
    for (TypeVar v : bound_vars())
        mapping[*v] = *var_instances[i++]; // CHECK ist deref correct here and below?
    assert(mapping.size() == var_instances.size());

    // check the bounds
    for (TypeVar v : bound_vars()) {
        auto it = mapping.find(*v);
        assert(it != mapping.end());
        Type instance = Type(it->second->as<TypeNode>());

        for (Trait bound : v->bounds()) {
            SpecializeMapping m(mapping); // copy the mapping
            Trait spec_bound = Trait(bound->specialize(m)->as<TraitNode>());
            spec_bound->typetable().unify(spec_bound);
            // TODO better error handling
            assert(instance->implements(spec_bound));
        }
    }

    return mapping;
}

Generic* Generic::ginstantiate(thorin::ArrayRef<Type> var_instances) {
    SpecializeMapping m = check_instantiation(var_instances);
    return vspecialize(m);
}

Generic* Generic::specialize(SpecializeMapping& mapping) {
    // FEATURE this could be faster if we copy only types were something changed inside
    auto it = mapping.find(this);
    if (it != mapping.end())
        return it->second;

    for (TypeVar v : bound_vars()) {
        // CHECK is representative really correct or do we need node()? -- see also below!
        assert(!mapping.contains(v.representative()));
        mapping[v.representative()] = v->clone(mapping).node(); // CHECK is node() correct here?
    }

    Generic* t = vspecialize(mapping);

    for (auto v : bound_vars()) {
        assert(mapping.find(v.representative()) != mapping.end());
        t->add_bound_var(TypeVar(mapping[v.representative()]->as<TypeVarNode>()));
    }

    return t;
}

}
