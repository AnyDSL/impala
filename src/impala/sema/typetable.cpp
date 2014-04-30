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

bool TypeTable::unify(Unifiable* unifiable) {
    if (unifiable->is_unified())
        return false;

    assert(unifiable->is_closed() && "only closed unifiables can be unified!");

    if (auto utn = unifiable->isa<UnknownTypeNode>()) {
        bool res = unify(utn->instance());
        utn->representative_ = *utn->instance();
        return res;
    }

    unifiable->refine();

    auto i = unifiables_.find(unifiable);
    if (i != unifiables_.end()) {
        auto repr = *i;
        assert(repr != unifiable && "already unified");
        unifiable->set_representative(repr);
        assert(unifiable->representative() == repr);
        return true;
    } else {
        assert(!unifiable->is_unified());
        unifiable->representative_ = unifiable;

        if (auto ktn = unifiable->isa<KnownTypeNode>()) {
            for (auto elem : ktn->elems()) {
                if (!elem->is_unified()) {
                    unify(elem);
                    assert(elem->is_unified());
                }
            }

            if (auto type_var = ktn->isa<TypeVarNode>()) {
                for (auto bound : type_var->bounds())
                    unify(bound);
            }
        } 

        auto p = unifiables_.insert(unifiable->representative());
        assert(p.second && "hash/equal broken");
        assert(unifiable->representative() == unifiable);
        return false;
    }
}

PrimType TypeTable::type(const PrimTypeKind kind) {
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
        TypeVar v = unifiable->type_var(i);
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
