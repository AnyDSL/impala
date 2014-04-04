#include "impala/sema/generic.h"

#include "thorin/util/assert.h"
#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

template<class T> void unify(TypeTable& tt, const Proxy<T>& p) { tt.unify(p); }

template void unify(TypeTable&, const Proxy<TypeNode>&);
template void unify(TypeTable&, const Proxy<TraitNode>&);
//template void unify(TypeTable&, const Proxy<TraitImplNode>&);

//------------------------------------------------------------------------------

bool Unifiable::unify_bound_vars(thorin::ArrayRef<TypeVar> other_vars) {
    if (num_bound_vars() == other_vars.size())
        return !is_generic(); // TODO enable unification of generic elements!
    return false;
}

void Unifiable::refine_bound_vars() {
    for (auto v : bound_vars())
        v->refine();
}

bool Unifiable::bound_vars_known() const {
    for (auto v : bound_vars()) {
        if (!v->is_known())
            return false;
    }
    return true;
}

void Unifiable::add_bound_var(TypeVar v) {
    assert(!v->is_closed() && "Type variables already bound");

    // CHECK should variables only be bound in this case? does this also hold for traits?
    //assert(v->is_subtype(this) && "Type variables can only be bound at t if they are a subtype of t!");
    // CHECK should 'forall a, a' be forbidden?
    //assert(type->kind() != Type_var && "Types like 'forall a, a' are forbidden!");

    v->bind(this);
    bound_vars_.push_back(v);
}

Unifiable* Unifiable::instantiate(SpecializeMap& var_instances) {
/*#ifndef NDEBUG
    verify_instantiation(var_instances);
#endif*/
    assert(var_instances.size() == num_bound_vars());
    return vspecialize(var_instances);
}

Unifiable* Unifiable::specialize(SpecializeMap& map) {
    // FEATURE this could be faster if we copy only types where something changed inside
    if (auto result = thorin::find(map, this))
        return result;

    for (auto v : bound_vars()) {
        // CHECK is representative really correct or do we need node()? -- see also below!
        assert(!map.contains(v.representative()));
        v->clone(map); // CHECK is node() correct here?
    }

    Unifiable* t = vspecialize(map);

    for (auto v : bound_vars()) {
        assert(map.contains(v.representative()));
        t->add_bound_var(TypeVar(map[v.representative()]->as<TypeVarNode>()));
    }

    return t;
}

}
