/*
 * TypeTable.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "TypeTable.h"

TypeTable::TypeTable()
    : types_()
    , traits_()
#define PRIMTYPE(T) , T##_(unify_new(new PrimType(*this, PrimType_##T)))
#include "primtypes.h"
    , type_error_(unify_new(new TypeError(*this)))
    , top_trait_(unify_trait(new TypeTrait(*this)))
    , top_trait_inst_(instantiate_trait(top_trait_, {}))
{}

FnType* TypeTable::fntype_simple(TypeArray params, Type* return_type) {
    FnType* retfun = fntype({return_type});

    size_t psize = params.size();

    Type** p = new Type*[psize + 1];

    for (int i = 0; i < psize; ++i) {
        p[i] = params[i];
    }
    p[psize] = retfun;

    return fntype(TypeArray(p, psize + 1));
}

void TypeTable::insert_new(Type* type) {
    assert(!type->is_unified());

    type->set_representative(type);

    for (auto elem : type->elems_) {
        if (!elem->is_unified()) {
            unify(elem);
            assert(elem->is_unified());
        }
    }

    // TODO unify traits

    if (type->kind() != Type_var) {
        // TODO is this a correct instanceof test?
        assert(!type->isa<TypeVar>());
        auto p = types_.insert(type);
        assert(p.second && "hash/equal broken");
    }
}

void TypeTable::change_repr(Type* t, const Type* repr) const {
    assert(repr->is_final_representative());

    if (t->is_unified()) {
        assert(t->get_representative() == repr);
        return;
    }

    assert(t->size() == repr->size());
    for (size_t i = 0, e = t->size(); i != e; ++i) {
        change_repr(t->elem_(i), repr->elem(i));
    }

    // TODO unify trait instances

    t->set_representative(repr);
}

Type* TypeTable::unify_base(Type* type) {
    if (type->is_unified()) {
        throw new IllegalTypeException("Type is already unified!");
    }

    // unify only closed types (i.e. only types where all type variables have been bound)
    if (! type->is_closed()) {
        throw new IllegalTypeException("Only closed types can be unified!");
    }

    auto i = types_.find(type);

    if (i != types_.end()) {
        assert(*i != type);
        change_repr(type, *i);
        return *i;
    } else {
        insert_new(type);
        assert(type->is_unified());
        return type;
    }
}

const TypeTrait* TypeTable::unify_trait(TypeTrait* trait) {
    auto i = traits_.find(trait);
    if (i != traits_.end()) {
        delete trait;
        return *i;
    }

    auto p = traits_.insert(trait);
    assert(p.second && "hash/equal broken");
    return trait;
}

const TypeTraitInstance* TypeTable::unify_trait_inst(TypeTraitInstance* trait_inst) {
    auto i = trait_instances_.find(trait_inst);
    if (i != trait_instances_.end()) {
        delete trait_inst;
        return *i;
    }

    auto p = trait_instances_.insert(trait_inst);
    assert(p.second && "hash/equal broken");
    return trait_inst;
}

const PrimType* TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define PRIMTYPE(T) case PrimType_##T: return T##_;
#include "primtypes.h"
        default: THORIN_UNREACHABLE;
    }
}

/*const Type* TypeTable::gentype_base(TypeVarArray tvars, const Type* type) {
   // all closed types should be unified and the other way round!
   assert(type->is_unified() == type->is_closed());

   if (type->is_unified())
       throw IllegalTypeException("Cannot create a generic type from an already unified one!");
   if (type->kind() == Type_var)
       throw IllegalTypeException("Types like 'forall a, a' are forbidden!");

   for (auto v : tvars) {
       if (!v->is_subtype(type))
           throw IllegalTypeException("Type variables can only be bound at t if they are a subtype of t!");

       v->bind(type);
       type->add_bound_var(v);
   }
   return unify(type);
}*/

void TypeTable::check_sanity() const {
    for (auto t : types_) {
        assert(t->is_unified());
        assert(t->is_final_representative());
        assert(t->is_sane());
    }
}

