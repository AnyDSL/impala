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
