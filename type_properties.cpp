/*
 * type_properties.cpp
 *
 *  Created on: Jan 2, 2014
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "type_properties.h"

#include "thorin/util/assert.h"
#include "type.h"
#include "trait.h"

std::string GenericElement::bound_vars_to_string() const {
    std::string result;

    if (!is_generic())
        return result;

    const char* separator = "<";
    for (auto v : bound_vars()) {
        result += separator + v->to_string();

        const TypeTraitInstSet* restr = v->restricted_by();

        // if v is unified it should at least be restricted by the top trait
        assert((!v->is_unified()) || (restr->size() > 0));

        // do not print restrictions if only restricted by top trait
        if ((restr->size() != 1) || (!(*restr->begin())->is_top_trait())) {
            auto inner_sep = ":";
            for (auto t : *restr) {
                result += inner_sep + t->to_string();
                inner_sep = "+";
            }

        }

        separator = ",";
    }
    return result + '>';
}

void GenericElement::add_bound_var(TypeVarNode* v) {
    if (v->is_closed())
        throw IllegalTypeException("Type variables already bound");

    // TODO should variables only be bound in this case? does this also hold for traits?
    //if (!v->is_subtype(this))
    //   throw IllegalTypeException("Type variables can only be bound at t if they are a subtype of t!");

    // TODO should this be forbidden?
    //if (type->kind() == Type_var)
    //   throw IllegalTypeException("Types like 'forall a, a' are forbidden!");

    v->bind(this);
    bound_vars_.push_back(v);
}
