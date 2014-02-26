/*
 * type_properties.cpp
 *
 *  Created on: Jan 2, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/typeproperties.h"

#include "thorin/util/assert.h"
#include "impala/sema/type.h"
#include "impala/sema/trait.h"

namespace impala {

std::string Generic::bound_vars_to_string() const {
    std::string result;

    if (!is_generic())
        return result;

    const char* separator = "[";
    for (auto v : bound_vars()) {
        result += separator + v->to_string();

        const TraitInstSet restr = v->bounds();

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
    assert(var_instances.size() == bound_vars().size() && "Wrong number of instances for bound type variables");

    for (size_t i = 0; i < var_instances.size(); ++i) {
        Type instance = var_instances[i];

        for (TraitInstance bound : bound_var(i)->bounds())
            // TODO better error handling
            assert(instance->implements(bound));
    }
}

}
