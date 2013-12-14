/*
 * trait.h
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#ifndef TRAIT_H_
#define TRAIT_H_

#include "type.h"

class TypeTable;


class TypeTrait {
private:
    /// create the global top type trait (like Object in java)
    TypeTrait(TypeTable& tt)
        : typetable_(tt)
        , name_(top_trait_name)
        , super_traits_()
    {}

    TypeTrait(TypeTable& tt, std::string name, const TypeTraitSet super_traits)
        : typetable_(tt)
        , name_(name)
        , super_traits_(super_traits)
    {}

    TypeTrait& operator = (const Type&); ///< Do not copy-assign a \p TypeTrait.
    TypeTrait(const Type& node);         ///< Do not copy-construct a \p TypeTrait.

    TypeTable& typetable_;
    std::string name_;
    const TypeTraitSet super_traits_;

    static const std::string top_trait_name;

public:
    bool equal(const TypeTrait* t) const;
    size_t hash() const;

    std::string to_string() const { return name_; }

    /// true if this is the global super type trait (like Object in java)
    bool is_top_trait() const {
        assert(super_traits_.size() != 0 || name_.compare(top_trait_name) == 0);
        return super_traits_.size() == 0;
    }

    friend class TypeTable;
};


#endif /* TRAIT_H_ */
