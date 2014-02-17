/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/ast.h"
#include "impala/sema/trait.h"

namespace impala {

const std::string TypeTrait::top_trait_name = std::string("");

bool TypeTrait::equal(const GenericElement* other) const {
    // TODO is this correct for a instanceof-equivalent?
    if (const TypeTrait* t = other->isa<TypeTrait>())
        return equal(t);
    return false;
}

std::string TypeTrait::to_string() const { return trait_decl()->symbol().str(); }

void TypeTrait::add_method(const std::string name, FnType type) {
    assert(type.is_unified() && "Method types must be closed");
    assert(type->is_closed());
    TypeTraitMethod* m = new TypeTraitMethod();
    m->name = name;
    m->type = type;
    methods_.push_back(m);
}

TypeTraitInstanceNode::TypeTraitInstanceNode(const TypeTrait* trait, thorin::ArrayRef<Type> var_instances)
    : trait_(trait)
    , var_instances_(var_instances.size())
{
    assert(var_instances.size() == trait->bound_vars().size() && "Wrong number of instances for bound type variables");
    size_t i = 0;
    for (auto elem : var_instances)
        var_instances_[i++] = elem;
}

bool TypeTraitInstanceNode::equal(const TypeTraitInstanceNode* other) const {
    // TODO use equal?
    if (trait_ != other->trait_)
        return false;

    assert(var_instances_.size() == other->var_instances_.size());
    for (size_t i = 0; i < var_instances_.size(); ++i) {
        if (! var_instances_[i].representative()->equal(other->var_instances_[i].representative())) {
            return false;
        }
    }
    return true;
}

// TODO better hash function
size_t TypeTraitInstanceNode::hash() const { return trait_->hash(); }

bool TypeTraitInstanceNode::is_closed() const {
    for (auto i : var_instances_) {
        if (!i->is_closed())
            return false;
    }
    return true;
}

// TODO
std::string TypeTraitInstanceNode::to_string() const {
    std::string result = trait_->to_string();

    if (var_inst_size() == 0)
        return result;

    const char* separator = "[";
    for (auto v : var_instances_) {
        result += separator + v->to_string();
        separator = ",";
    }

    return result + "]";
}

}
