/*
 * trait.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "trait.h"

const std::string TypeTrait::top_trait_name = std::string("");

bool TypeTrait::equal(const TypeTrait* other) const {
    return name_.compare(other->name_) == 0;
}

size_t TypeTrait::hash() const { return anydsl2::hash_value(name_); }

// TODO
std::string TypeTrait::to_string() const {
    return name_;
}

// TODO
bool TypeTraitInstance::equal(const TypeTraitInstance* other) const {
    return false;
}

// TODO
size_t TypeTraitInstance::hash() const { return 0; }

// TODO
std::string TypeTraitInstance::to_string() const {
    return trait_->to_string();
}
