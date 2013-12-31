/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "trait.h"

const std::string TypeTrait::top_trait_name = std::string("");

bool TypeTrait::equal(const GenericElement* other) const {
    // TODO is this correct for a instanceof-equivalent?
    if (const TypeTrait* t = other->isa<TypeTrait>()) {
        return equal(t);
    }
    return false;
}

bool TypeTrait::equal(const TypeTrait* other) const {
    return name_.compare(other->name_) == 0;
}

size_t TypeTrait::hash() const { return thorin::hash_value(name_); }

// TODO
std::string TypeTrait::to_string() const {
    return name_;
}

void TypeTrait::add_method(const std::string name, const FnType* type) {
    if (! type->is_unified()) {
        throw new IllegalTypeException("Method types must be closed");
    }
    assert(type->is_closed());
    TypeTraitMethod* m = new TypeTraitMethod();
    m->name = name;
    m->type = type;
    methods_.push_back(m);
}

bool TypeTraitInstance::equal(const TypeTraitInstance* other) const {
    if (trait_ != other->trait_)
        return false;

    assert(var_instances_.size() == other->var_instances_.size());

    for (int i = 0; i < var_instances_.size(); ++i) {
        if (! var_instances_[i]->equal(other->var_instances_[i])) {
            return false;
        }
    }
    return true;
}

// TODO
size_t TypeTraitInstance::hash() const { return 0; }

// TODO
std::string TypeTraitInstance::to_string() const {
    return trait_->to_string();
}
