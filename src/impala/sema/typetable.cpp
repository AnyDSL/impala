/*
 * TypeTable.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

UnifiableSet::~UnifiableSet() {
    for (auto t : trait_instances_) delete t;
    for (auto t : types_) delete t;
    for (auto t : traits_) delete t;
    for (auto t : trait_impls_) delete t;
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : types_()
#define IMPALA_TYPE(itype, atype) , itype##_(new_type(new PrimTypeNode(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
    , type_error_(new_type(new TypeErrorNode(*this)))
    , type_noreturn_(new_type(new NoReturnTypeNode(*this)))
{
#define IMPALA_TYPE(itype, atype) unify(itype##_);
#include "impala/tokenlist.h"
    unify(type_error_);
    unify(type_noreturn_);
}

TypeTable::~TypeTable() { 
    for (auto type : types_) delete type; 
    for (auto trait : trait_instances_) delete trait;
}

FnType TypeTable::fntype(thorin::ArrayRef<Type> params, Type return_type) {
    thorin::Array<Type> p(params.size() + 1);
    *std::copy(params.begin(), params.end(), p.begin()) = fntype({return_type});
    return fntype(p);
}

void TypeTable::insert_new(Type type) {
    assert(!type.is_unified());
    type.set_unified();

    for (Type elem : type->elems_) {
        if (!elem.is_unified()) {
            unify(elem);
            assert(elem.is_unified());
        }
    }

    for (TypeVar v : type->bound_vars()) {
        bool changed = false;
        for (auto r : v->bounds()) {
            if (!r.is_unified()) {
                changed = unify(r) || changed;
                assert(r.is_unified());
            }
        }
        // we have to renew the bounds set because the hashes may have changed during unification
        if (changed)
            v->refresh_bounds();
    }

    // CHECK does it cause any problems to put TypeVars into the type-set?
    //if (type->kind() != Type_var) {
        //assert(!type.representative()->isa<TypeVarNode>());
        auto p = types_.insert(type.representative());
        assert(p.second && "hash/equal broken");
    //}
}

void TypeTable::insert_new(TraitInstance tti) {
    assert(!tti.is_unified());
    tti.set_unified();

    for (size_t i = 0, e = tti->var_inst_size(); i != e; ++i) {
        auto vi = tti->var_inst_(i);
        if (!vi.is_unified()) {
            unify(vi);
            assert(vi.is_unified());
        }
    }

    auto p = trait_instances_.insert(tti.representative());
    assert(p.second && "hash/equal broken");
}

void TypeTable::change_repr_rec(TraitInstance tti, TraitInstanceNode* repr) const {
    assert(tti->var_inst_size() == repr->var_inst_size());
    for (size_t i = 0, e = tti->var_inst_size(); i != e; ++i)
        change_repr(tti->var_inst_(i), repr->var_inst_(i).representative());
}

// change_repr_rec for types, but because TypeVar !< Type we need templates here
template<class T> void TypeTable::change_repr_rec(UnifiableProxy<T> t, T* repr) const {
    // first change the representative of all bound variables but remember the old ones
    std::vector<TraitInstSet*> var_restrictions;
    assert(t->bound_vars().size() == repr->bound_vars().size());
    for (size_t i = 0, e = t->bound_vars().size(); i != e; ++i) {
        var_restrictions.push_back(new TraitInstSet(t->bound_var(i)->bounds()));
        change_repr(t->bound_var(i), repr->bound_var(i).representative());
    }

    // change representatives of the bounds (i.e. \p TraitInstances) of type variables
    size_t num_bound_vars = var_restrictions.size();
    assert(num_bound_vars == repr->bound_vars().size());

    for (size_t i = 0; i != num_bound_vars; ++i) {
        auto old_bounds = var_restrictions[i];
        auto repr_bounds = repr->bound_var(i)->bounds();

        assert(old_bounds->size() == repr_bounds.size());

        // FEATURE this works but seems too much effort
        TraitInstanceNodeTableSet ttis;
        for (auto r : repr_bounds) {
            auto p = ttis.insert(r.representative());
            assert(p.second && "hash/equal broken");
        }

        for (TraitInstance bound : *old_bounds) {
            auto repr_bound = ttis.find(bound.representative());
            assert(repr_bound != ttis.end());
            change_repr(bound, *repr_bound);
        }
    }

    for (auto vr : var_restrictions)
        delete vr;

    // change representative of all sub elements
    assert(t->size() == repr->size());
    for (size_t i = 0, e = t->size(); i != e; ++i)
        change_repr(t->elem_(i), repr->elem(i).representative());
}

template<class T>
void TypeTable::change_repr(UnifiableProxy<T> t, T* repr) const {
    if (!t.is_unified()) {
        change_repr_rec(t, repr);
        t.set_representative(repr);
    } else
        assert(t.representative() == repr);
}

void TypeTable::unify_base(Type type) {
    assert(!type.is_unified() && "Type is already unified!");
    assert(type->is_closed() && "Only closed types can be unified!");

    auto i = types_.find(type.representative());

    if (i != types_.end()) {
        assert(*i != type.representative());
        change_repr(type, *i);
        assert(type.representative() == (*i));
    } else {
        insert_new(type);
        assert(type.is_unified());
    }
}

bool TypeTable::unify(TraitInstance trait_inst) {
    assert(!trait_inst.is_unified() && "trait instance already unified");
    assert(trait_inst->is_closed() && "Only closed trait instances can be unified!");

    auto i = trait_instances_.find(trait_inst.representative());
    if (i != trait_instances_.end()) {
        assert(*i != trait_inst.representative());
        change_repr(trait_inst, *i);
        assert(trait_inst.representative() == *i);
        return true;
    } else {
        insert_new(trait_inst);
        assert(trait_inst.is_unified());
        return false;
    }
}

PrimType TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

void TypeTable::verify() const {
    for (auto t : types_)
        assert(t->is_sane());

    for (auto t : unifiables_.trait_instances_) {
        if (t->is_unified()) {
            auto i = trait_instances_.find(t->representative());
            assert(i != trait_instances_.end());
        }
    }
    for (auto t : unifiables_.types_) {
        if (t->is_unified()) {
            auto i = types_.find(t->representative());
            assert(i != types_.end());
        }
    }
}

}
