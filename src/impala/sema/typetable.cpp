/*
 * TypeTable.cpp
 *
 *  Created on: Dec 14, 2013
 *      Author: David Poetzsch-Heffter <s9dapoet@stud.uni-saarland.de>
 */

#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : unifiables_()
#define IMPALA_TYPE(itype, atype) , itype##_(new_unifiable(new PrimTypeNode(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
    , trait_error_(trait(nullptr))
    , type_error_(new_unifiable(new TypeErrorNode(*this)))
    , type_noreturn_(new_unifiable(new NoReturnTypeNode(*this)))
{
#define IMPALA_TYPE(itype, atype) unify(Type(itype##_));
#include "impala/tokenlist.h"
    unify(Type(type_error_));
    unify(Type(type_noreturn_));
}

TypeTable::~TypeTable() { for (Generic* g : garbage_) delete g; }

template<class T> void TypeTable::insert_new(T* unifiable) {
    assert(!unifiable->is_unified());
    unifiable->set_representative(unifiable);

    for (TypeVar v : unifiable->bound_vars()) {
        bool changed = false;
        for (Trait r : v->bounds()) {
            if (!r->is_unified()) {
                changed = unify(r) || changed;
                assert(r->is_unified());
            }
        }
        // we have to renew the bounds set because the hashes may have changed during unification
        if (changed) v->refresh_bounds();
    }

    insert_new_rec(unifiable);

    // CHECK does it cause any problems to put TypeVars into the type-set?
    auto p = unifiables_.insert(unifiable->representative());
    assert(p.second && "hash/equal broken");
}

void TypeTable::insert_new_rec(TypeNode* type) {
    for (Type elem : type->elems_) {
        if (!elem->is_unified()) {
            unify(elem);
            assert(elem->is_unified());
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
            auto p = bounds.insert(r.representative()); // CHECK ist representative here and node() below correct?
            assert(p.second && "hash/equal broken");
        }

        for (Trait bound : old_bounds) {
            auto repr_bound = bounds.find(bound.node());
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
bool TypeTable::unify(Proxy<T> elem) {
    T* unifiable = elem.node();

    assert(!unifiable->is_unified() && "Unifiable is already unified!");
    assert(unifiable->is_closed() && "Only closed unifiables can be unified!");

    auto i = unifiables_.find(unifiable);

    if (i != unifiables_.end()) {
        T* repr = (*i)->template as<T>();
        assert(repr != unifiable && "Already unified");
        change_repr(unifiable, repr);
        assert(unifiable->representative() == repr);
        return true;
    } else {
        insert_new(unifiable);
        assert(unifiable->representative() == unifiable);
        return false;
    }
}

// force instantiation REMINDER remove this
/*template bool TypeTable::unify_base(TypetableSet<TypeNode>& set, TypeNode* unifiable);
template bool TypeTable::unify_base(TypetableSet<TraitNode>& set, TraitNode* unifiable);
template bool TypeTable::unify_base(TypetableSet<TraitImplNode>& set, TraitImplNode* unifiable);*/

PrimType TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

void TypeTable::verify() const {
    for (auto g : unifiables_) {
        if (auto type = g->isa<TypeNode>()) {
            assert(type->is_sane());
            assert(type->is_final_representative());
        } else if (auto trait = g->isa<TraitNode>()) {
            assert(trait->is_closed());
            assert(trait->is_final_representative());
        } else if (auto impl = g->isa<TraitImplNode>()) {
            assert(impl->is_closed());
            assert(impl->is_final_representative());
        }
    }

    for (auto g : garbage_) {
        if (auto type = g->isa<TypeNode>()) {
            if (type->is_unified())
                assert(unifiables_.find(type->representative()) != unifiables_.end());
        } else if (auto trait = g->isa<TraitNode>()) {
            if (trait->is_unified())
                assert(unifiables_.find(trait->representative()) != unifiables_.end());
        } else if (auto impl = g->isa<TraitImplNode>()) {
            if (impl->is_unified())
                assert(unifiables_.find(impl->representative()) != unifiables_.end());
        }
    }
}

}
