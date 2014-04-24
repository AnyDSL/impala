#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : trait_error_(trait(nullptr))
    , type_error_(new_unifiable(new TypeErrorNode(*this)))
    , type_noreturn_(new_unifiable(new NoReturnTypeNode(*this)))
#define IMPALA_TYPE(itype, atype) , itype##_(new_unifiable(new PrimTypeNode(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
{
#define IMPALA_TYPE(itype, atype) unify(Type(itype##_));
#include "impala/tokenlist.h"
    unify(Type(type_error_));
    unify(Type(type_noreturn_));
}

TypeTable::~TypeTable() { 
    for (auto g : garbage_) 
        delete g; 
}

void TypeTable::insert_new(Unifiable* unifiable) {
    assert(!unifiable->is_unified());
    unifiable->set_representative(unifiable);

    for (auto v : unifiable->type_vars()) {
        bool changed = false;
        for (auto r : v->bounds()) {
            if (!r->is_unified()) {
                changed = unify(r) || changed;
                assert(r->is_unified());
            }
        }
        // we have to renew the bounds set because the hashes may have changed during unification
        if (changed) 
            v->refresh_bounds();
    }

    if (auto ktn = unifiable->isa<KnownTypeNode>()) {
        for (auto elem : ktn->elems()) {
            if (!elem->is_unified()) {
                unify(elem);
                assert(elem->is_unified());
            }
        }
    } else {
        // TODO insert methods for traits
    }

    // CHECK does it cause any problems to put TypeVars into the type-set?
    auto p = unifiables_.insert(unifiable->representative());
    assert(p.second && "hash/equal broken");
}

Unifiable* TypeTable::instantiate_unknown(Unifiable* unifiable, std::vector<Type>& types) {
    for (size_t i = 0; i < unifiable->num_type_vars(); ++i) 
        types.push_back(unknown_type());
    auto map = infer(unifiable, types);
    return unifiable->instantiate(map);
}

SpecializeMap TypeTable::infer(Unifiable* unifiable, thorin::ArrayRef<Type> var_instances) const {
    assert(unifiable->num_type_vars() == var_instances.size());
    SpecializeMap map;
    size_t i = 0;
    for (TypeVar v : unifiable->type_vars())
        map[*v] = *var_instances[i++]; // CHECK ist deref correct here and below?
    assert(map.size() == var_instances.size());
    return map;
}

void TypeTable::change_repr_unifiable(Unifiable* u, Unifiable* repr) const {
    // first change the representative of all bound variables
    assert(u->type_vars().size() == repr->type_vars().size());
    for (size_t i = 0, e = u->type_vars().size(); i != e; ++i) {
        change_repr(u->bound_var(i).node(), repr->bound_var(i).representative());
    }

    // change representatives of the bounds (i.e. Traits) of type variables
    assert(u->num_type_vars() == repr->num_type_vars());
    for (size_t i = 0; i != u->num_type_vars(); ++i) {
        UniSet<Trait> old_bounds = u->bound_var(i)->bounds();
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
            change_repr(bound.node(), (*repr_bound)->representative());
        }
    }
}

void TypeTable::change_repr_rec(Unifiable* u, Unifiable* repr) const {
    if (auto ty = u->isa<TypeNode>()) {
        auto t = ty->as<KnownTypeNode>();
        auto ktn = repr->as<KnownTypeNode>();

        // change representative of all sub elements
        assert(t->size() == ktn->size());
        for (size_t i = 0, e = t->size(); i != e; ++i)
            change_repr(t->elem_(i).node(), ktn->elem(i).representative());
    }
}

void TypeTable::change_repr(Unifiable* u, Unifiable* repr) const {
    if (!u->is_unified()) {
        change_repr_unifiable(u, repr);
        change_repr_rec(u, repr);
        u->set_representative(repr);
    } else
        assert(u->representative() == repr);
}

bool TypeTable::unify(Unifiable* unifiable) {
    if (unifiable->is_unified())
        return false;

    assert(unifiable->is_closed() && "Only closed unifiables can be unified!");

    if (auto utn = unifiable->isa<UnknownTypeNode>()) {
        bool res = unify(utn->instance());
        utn->set_representative(*utn->instance());
        return res;
    }

    unifiable->refine();

    auto i = unifiables_.find(unifiable);
    if (i != unifiables_.end()) {
        auto repr = *i;
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

PrimType TypeTable::primtype(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

bool TypeTable::check_bounds(const ASTNode* loc, Unifiable* unifiable, thorin::ArrayRef<Type> inst_types, SpecializeMap& map) {
    assert(inst_types.size() == unifiable->num_type_vars());
    assert(inst_types.size() == map.size());

    bool no_error = true;

    // check the bounds
    for (size_t i = 0; i < unifiable->num_type_vars(); ++i) {
        TypeVar v = unifiable->bound_var(i);
        Type instance = inst_types[i];
        assert(map.contains(*v));
        assert(map.find(*v)->second == *instance);

        for (auto bound : v->bounds()) {
            SpecializeMap m(map); // copy the map
            Trait spec_bound = bound->specialize(m);
            unify(spec_bound);

            if (instance != type_error() && spec_bound != trait_error()) {
                check_impls(); // first we need to check all implementations to be up-to-date
                if (!instance->implements(spec_bound)) {
                    if (loc) {
                        error(loc) << "'" << instance << "' (instance for '" << v << "') does not implement bound '" 
                            << spec_bound << "'\n";
                    }
                    no_error = false;
                }
            }
        }
    }

    return no_error;
}

}
