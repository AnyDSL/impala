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
    for (auto t : types_) delete t;
    for (auto t : traits_) delete t;
    for (auto t : trait_impls_) delete t;
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : types_()
#define IMPALA_TYPE(itype, atype) , itype##_(new_unifiable(new PrimTypeNode(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
    , type_error_(new_unifiable(new TypeErrorNode(*this)))
{
    Type t;
#define IMPALA_TYPE(itype, atype) unify(t = itype##_);   // TODO remove this hacked cast
#include "impala/tokenlist.h"
    unify(t = type_error_); // TODO remove this hacked cast
}

template<class T> void TypeTable::insert_new(TypetableSet<T>& set, T* unifiable) {
    assert(!unifiable->is_unified());
    unifiable->set_representative(unifiable);

    for (TypeVar v : unifiable->bound_vars()) {
        bool changed = false;
        for (auto r : v->bounds()) {
            if (!r.is_unified()) {
                changed = unify(r) || changed;
                assert(r.is_unified());
            }
        }
        // we have to renew the bounds set because the hashes may have changed during unification
        if (changed) v->refresh_bounds();
    }

    insert_new_rec(unifiable);

    // CHECK does it cause any problems to put TypeVars into the type-set?
    auto p = set.insert(unifiable->representative());
    assert(p.second && "hash/equal broken");
}

void TypeTable::insert_new_rec(TypeNode* type) {
    for (Type elem : type->elems_) {
        if (!elem.is_unified()) {
            unify(elem);
            assert(elem.is_unified());
        }
    }
}

void TypeTable::insert_new_rec(TraitNode* t) {
    // TODO insert methods
}

template<class T>
void TypeTable::change_repr_generic(T* t, T* repr) const {
    // first change the representative of all bound variables
    assert(t->bound_vars().size() == repr->bound_vars().size());
    for (size_t i = 0, e = t->bound_vars().size(); i != e; ++i) {
        change_repr(t->bound_var(i).node(), repr->bound_var(i).representative());
    }

    // change representatives of the bounds (i.e. Traits) of type variables
    assert(t->num_bound_vars() == repr->num_bound_vars());
    for (size_t i = 0; i != t->num_bound_vars(); ++i) {
        NodeSet<Trait> old_bounds = t->bound_var(i)->bounds();
        NodeSet<Trait> repr_bounds = repr->bound_var(i)->bounds();

        assert(old_bounds.size() == repr_bounds.size());

        // FEATURE this works but seems too much effort
        // put them in a set were they are equal using the equal methods not ==;
        // FEATURE if this is changed in the bounds of a TypeVar this is not needed any more
        TypetableSet<TraitNode> bounds;
        for (Trait r : repr_bounds) {
            auto p = bounds.insert(r.representative());
            assert(p.second && "hash/equal broken");
        }

        for (Trait bound : old_bounds) {
            auto repr_bound = bounds.find(bound);
            assert(repr_bound != bounds.end());
            change_repr(bound.node(), (*repr_bound)->representative());
        }
    }
}

void TypeTable::change_repr_rec(TypeNode* t, TypeNode* repr) const {
    // change representative of all sub elements
    assert(t->size() == repr->size());
    for (size_t i = 0, e = t->size(); i != e; ++i)
        change_repr(t->elem_(i).node_, repr->elem(i).representative());
}

template<class T>
void TypeTable::change_repr(T* t, T* repr) const {
    if (!t->is_unified()) {
        change_repr_generic(t, repr);
        change_repr_rec(t, repr);
        t->set_representative(repr);
    } else
        assert(t->representative() == repr);
}

template<class T>
bool TypeTable::unify_base(TypetableSet<T>& set, T* unifiable) {
    assert(!unifiable->is_unified() && "Unifiable is already unified!");
    assert(unifiable->is_closed() && "Only closed unifiables can be unified!");

    auto i = set.find(unifiable);

    if (i != set.end()) {
        assert(*i != unifiable && "Already unified");
        change_repr(unifiable, *i);
        assert(unifiable->representative() == (*i));
        return true;
    } else {
        insert_new(set, unifiable);
        assert(unifiable->representative() == unifiable);
        return false;
    }
}

// force instantiation
template bool TypeTable::unify_base(TypetableSet<TypeNode>& set, TypeNode* unifiable);
template bool TypeTable::unify_base(TypetableSet<TraitNode>& set, TraitNode* unifiable);
template bool TypeTable::unify_base(TypetableSet<TraitImplNode>& set, TraitImplNode* unifiable);

PrimType TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

void TypeTable::verify() const {
    for (auto t : types_) {
        assert(t->is_sane());
        assert(t->is_final_representative());
    }
    for (auto t : traits_) {
        assert(t->is_closed());
        assert(t->is_final_representative());
    }
    for (auto t : trait_impls_) {
        assert(t->is_closed());
        assert(t->is_final_representative());
    }

    for (auto t : unifiables_.types_) {
        if (t->is_unified()) {
            auto i = types_.find(t->representative());
            assert(i != types_.end());
        }
    }
    for (auto t : unifiables_.traits_) {
        if (t->is_unified()) {
            auto i = traits_.find(t->representative());
            assert(i != traits_.end());
        }
    }
    for (auto t : unifiables_.trait_impls_) {
        if (t->is_unified()) {
            auto i = trait_impls_.find(t->representative());
            assert(i != trait_impls_.end());
        }
    }
}

}
