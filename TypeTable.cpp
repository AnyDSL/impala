/*
 * TypeTable.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "TypeTable.h"

TypeTable::TypeTable()
    : types_()
//    , traits_()
#define PRIMTYPE(T) , T##_(unify_new(new PrimType(*this, PrimType_##T)))
#include "primtypes.h"
    , type_error_(unify_new(new TypeError(*this)))
    , top_trait_(new TypeTrait(*this))
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

    for (auto v : type->bound_vars()) {
        for (auto r : v->restricted_by_) {
            if (!r->is_unified()) {
                unify(r);
                assert(r->is_unified());
            }
        }
    }

    if (type->kind() != Type_var) {
        // TODO is this a correct instanceof test?
        assert(!type->isa<TypeVar>());
        auto p = types_.insert(type);
        assert(p.second && "hash/equal broken");
    }
}

void TypeTable::insert_new(TypeTraitInstance* tti) {
    assert(!tti->is_unified());

    tti->set_representative(tti);

    for (size_t i = 0, e = tti->var_inst_size(); i != e; ++i) {
        auto vi = tti->var_inst_(i);
        if (!vi->is_unified()) {
            unify(vi);
            assert(vi->is_unified());
        }
    }

    auto p = trait_instances_.insert(tti);
    assert(p.second && "hash/equal broken");
}

void TypeTable::change_repr_rec(Type* t, const Type* repr) const {
    // first unify all bounded variables
    assert(t->bound_vars().size() == repr->bound_vars().size());
    for (size_t i = 0, e = t->bound_vars().size(); i != e; ++i) {
        change_repr(t->bound_var(i), repr->bound_var(i));
    }

    // unify restrictions of bounded variables
    assert(t->bound_vars().size() == repr->bound_vars().size());
    for (size_t i = 0, e = t->bound_vars().size(); i != e; ++i) {
        auto tv = t->bound_var(i);
        auto reprv = repr->bound_var(i);

        assert(tv->restricted_by()->size() == reprv->restricted_by()->size());

        // TODO this does work but seems too much effort
        TraitInstanceTableSet ttis;
        for (auto r : *reprv->restricted_by()) {
            auto p = ttis.insert(r);
            assert(p.second && "hash/equal broken");
        }

        // this->restricted_by() subset of trestr
        for (auto restr : *tv->restricted_by()) {
            auto repr_restr = ttis.find(restr);
            assert(repr_restr != ttis.end());
            change_repr(restr, *repr_restr);
        }
    }

    // unify sub elements
    assert(t->size() == repr->size());
    for (size_t i = 0, e = t->size(); i != e; ++i) {
        change_repr(t->elem_(i), repr->elem(i));
    }
}

void TypeTable::change_repr_rec(TypeTraitInstance* tti, const TypeTraitInstance* repr) const {
    assert(tti->var_inst_size() == repr->var_inst_size());
    for (size_t i = 0, e = tti->var_inst_size(); i != e; ++i) {
        change_repr(tti->var_inst_(i), repr->var_inst(i));
    }
}

template<class T>
void TypeTable::change_repr(T* t, const T* repr) const {
    assert(repr->is_final_representative());

    if (t->is_unified()) {
        assert(t->get_representative() == repr);
        return;
    }

    change_repr_rec(t, repr);

    t->set_representative(repr);
}

Type* TypeTable::unify_base(Type* type) {
    if (type->is_unified()) {
        throw IllegalTypeException("Type is already unified!");
    }

    // unify only closed types (i.e. only types where all type variables have been bound)
    if (! type->is_closed()) {
        throw IllegalTypeException("Only closed types can be unified!");
    }

    auto i = types_.find(type);

    if (i != types_.end()) {
        assert(*i != type);
        change_repr(type, *i);
        assert(type->get_representative() == *i);
        return *i;
    } else {
        insert_new(type);
        assert(type->is_unified());
        assert(type->is_final_representative());
        return type;
    }
}

/*TypeTrait* TypeTable::unify_trait(TypeTrait* trait) {
    auto i = traits_.find(trait);
    if (i != traits_.end()) {
        delete trait;
        return *i;
    }

    auto p = traits_.insert(trait);
    assert(p.second && "hash/equal broken");
    return trait;
}*/

//const TypeTraitInstance* TypeTable::unify_trait_inst(TypeTraitInstance* trait_inst) {
TypeTraitInstance* TypeTable::unify_base(TypeTraitInstance* trait_inst) {
    if (trait_inst->is_unified()) {
        throw IllegalTypeException("trait instance already unified");
    }

    auto i = trait_instances_.find(trait_inst);
    if (i != trait_instances_.end()) {
        assert(*i != trait_inst);
        change_repr(trait_inst, *i);
        assert(trait_inst->get_representative() == *i);
        return *i;
    } else {
        insert_new(trait_inst);
        assert(trait_inst->is_unified());
        assert(trait_inst->is_final_representative());
        return trait_inst;
    }
}

const PrimType* TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define PRIMTYPE(T) case PrimType_##T: return T##_;
#include "primtypes.h"
        default: THORIN_UNREACHABLE;
    }
}

void TypeTable::check_sanity() const {
    for (auto t : types_) {
        assert(t->is_unified());
        assert(t->is_final_representative());
        assert(t->is_sane());
    }
}

