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

template<class T> void unify(TypeTable& tt, const Proxy<T>& p) { tt.unify(p); }

template void unify(TypeTable&, const Proxy<TypeNode>&);
template void unify(TypeTable&, const Proxy<TraitNode>&);
//template void unify(TypeTable&, const Proxy<TraitImplNode>&);

//------------------------------------------------------------------------------

bool Generic::unify_bound_vars(thorin::ArrayRef<TypeVar> other_vars) {
    if (num_bound_vars() == other_vars.size()) {
        return !is_generic(); // TODO enable unification of generic elements!
    }
    return false;
}

void Generic::refine_bound_vars() {
    for (auto v : bound_vars())
        v->refine();
}

bool Generic::bound_vars_known() const {
    bool result = true;
    for (auto v : bound_vars())
        result = result && v->is_known();
    return result;
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

void Generic::check_instantiation(SpecializeMapping& mapping) const {
    assert(mapping.size() == num_bound_vars());

    // check the bounds
    for (TypeVar v : bound_vars()) {
        auto it = mapping.find(*v);
        assert(it != mapping.end());
        Type instance = Type(it->second->as<TypeNode>());

        for (Trait bound : v->bounds()) {
            SpecializeMapping m(mapping); // copy the mapping
            Trait spec_bound = Trait(bound->specialize(m)->as<TraitNode>());
            spec_bound->typetable().unify(spec_bound);
            assert(instance->implements(spec_bound));
        }
    }
}

Generic* Generic::ginstantiate(SpecializeMapping& var_instances) {
/*#ifndef NDEBUG
    check_instantiation(var_instances);
#endif*/
    assert(var_instances.size() == num_bound_vars());
    return vspecialize(var_instances);
}

Generic* Generic::gspecialize(SpecializeMapping& mapping) {
    // FEATURE this could be faster if we copy only types were something changed inside
    auto it = mapping.find(this);
    if (it != mapping.end())
        return it->second;

    for (TypeVar v : bound_vars()) {
        // CHECK is representative really correct or do we need node()? -- see also below!
        assert(!mapping.contains(v.representative()));
        v->clone(mapping); // CHECK is node() correct here?
    }

    Generic* t = vspecialize(mapping);

    for (auto v : bound_vars()) {
        assert(mapping.find(v.representative()) != mapping.end());
        t->add_bound_var(TypeVar(mapping[v.representative()]->as<TypeVarNode>()));
    }

    return t;
}

}
