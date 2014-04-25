#include "impala/sema/unifiable.h"

#include "thorin/util/assert.h"

#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

bool Unifiable::unify_type_vars(thorin::ArrayRef<TypeVar> other_vars) {
    if (num_type_vars() == other_vars.size())
        return !is_generic(); // TODO enable unification of generic elements!
    return false;
}

void Unifiable::refine_type_vars() {
    for (auto v : type_vars())
        v->refine();
}

bool Unifiable::type_vars_known() const {
    for (auto v : type_vars()) {
        if (!v->is_known())
            return false;
    }
    return true;
}

void Unifiable::bind(TypeVar v) {
    assert(!v->is_closed() && "type variables already bound");
    assert(!is_unified() && "type already unified");
    assert(v->bound_at_ == nullptr && "type variables can only be bound once");
    // CHECK should variables only be bound in this case? does this also hold for traits?
    //assert(v->is_subtype(this) && "Type variables can only be bound at t if they are a subtype of t!");
    // CHECK should 'forall a, a' be forbidden?
    //assert(type->kind() != Type_var && "Types like 'forall a, a' are forbidden!");

    v->bound_at_ = this;
    type_vars_.push_back(v);
}

Unifiable* Unifiable::instantiate(SpecializeMap& var_instances) {
/*#ifndef NDEBUG
    verify_instantiation(var_instances);
#endif*/
    assert(var_instances.size() == num_type_vars());
    return vspecialize(var_instances);
}

Unifiable* Unifiable::specialize(SpecializeMap& map) {
    // FEATURE this could be faster if we copy only types where something changed inside
    if (auto result = thorin::find(map, this))
        return result;

    for (auto v : type_vars()) {
        // CHECK is representative really correct or do we need node()? -- see also below!
        assert(!map.contains(v.representative()));
        v->clone(map); // CHECK is node() correct here?
    }

    Unifiable* t = vspecialize(map);

    for (auto v : type_vars()) {
        assert(map.contains(v.representative()));
        t->bind(TypeVar(map[v.representative()]->as<TypeVarNode>()));
    }

    return t;
}

bool Unifiable::unify() { return typetable().unify(this); }

void Unifiable::change_repr_unifiable(Unifiable* repr) const {
    // first change the representative of all bound variables
    assert(type_vars().size() == repr->type_vars().size());
    for (size_t i = 0, e = type_vars().size(); i != e; ++i) {
        bound_var(i).node()->change_repr(repr->bound_var(i).representative());
    }

    // change representatives of the bounds (i.e. Traits) of type variables
    assert(num_type_vars() == repr->num_type_vars());
    for (size_t i = 0; i != num_type_vars(); ++i) {
        UniSet<Trait> old_bounds = bound_var(i)->bounds();
        UniSet<Trait> repr_bounds = repr->bound_var(i)->bounds();

        assert(old_bounds.size() == repr_bounds.size());

        // FEATURE this works but seems too much effort
        // put them in a set were they are equal using the equal methods not ==;
        // FEATURE if this is changed in the bounds of a TypeVar this is not needed any more
        TypetableSet<TraitNode> bounds;
        for (auto r : repr_bounds) {
            auto p = bounds.insert(r.representative()); // CHECK ist representative here and node() below correct?
            assert(p.second && "hash/equal broken");
        }

        for (auto bound : old_bounds) {
            auto repr_bound = bounds.find(bound.node());
            assert(repr_bound != bounds.end());
            bound.node()->change_repr((*repr_bound)->representative());
        }
    }
}

void Unifiable::change_repr_rec(Unifiable* repr) const {
    if (auto ty = isa<TypeNode>()) {
        auto t = ty->as<KnownTypeNode>();
        auto ktn = repr->as<KnownTypeNode>();

        // change representative of all sub elements
        assert(t->size() == ktn->size());
        for (size_t i = 0, e = t->size(); i != e; ++i)
            t->elem(i).node()->change_repr(ktn->elem(i).representative());
    }
}

void Unifiable::change_repr(Unifiable* repr) const {
    if (!is_unified()) {
        change_repr_unifiable(repr);
        change_repr_rec(repr);
        set_representative(repr);
    } else
        assert(representative() == repr);
}

}
