#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : type_error_(unify(new_unifiable(new TypeErrorNode(*this))))
    , trait_error_(unify(trait(nullptr)))
    , bound_error_(unify(bound(trait_error(), {})))
    , type_noret_(unify(new_unifiable(new NoRetTypeNode(*this))))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(new_unifiable(new PrimTypeNode(*this, PrimType_##itype))))
#include "impala/tokenlist.h"
{}

TypeTable::~TypeTable() { 
    for (auto g : garbage_) 
        delete g; 
}

Type TypeTable::instantiate_unknown(Type type, std::vector<Type>& type_args) {
    for (size_t i = 0, e = type->num_type_vars(); i != e;  ++i) 
        type_args.push_back(unknown_type());
    auto map = specialize_map(type, type_args);
    return type->vspecialize(map);
}

SpecializeMap TypeTable::specialize_map(const Unifiable* unifiable, thorin::ArrayRef<Type> type_args) {
    assert(unifiable->num_type_vars() == type_args.size());
    SpecializeMap map;
    size_t i = 0;
    for (TypeVar v : unifiable->type_vars())
        map[*v] = *type_args[i++];
    assert(map.size() == type_args.size());
    return map;
}

const Unifiable* TypeTable::unify(const Unifiable* unifiable) {
    if (unifiable->is_unified())
        return unifiable->representative();

    assert(unifiable->is_closed() && "only closed unifiables can be unified!");

    if (auto utn = unifiable->isa<UnknownTypeNode>())
        return utn->representative_ = *unify(utn->instance());

    auto i = unifiables_.find(unifiable);
    if (i != unifiables_.end()) {
        auto repr = *i;
        assert(repr != unifiable && "already unified");
        unifiable->set_representative(repr);
        assert(unifiable->representative() == repr);
        return repr;
    } else {
        assert(!unifiable->is_unified());
        unifiable->representative_ = unifiable;

        if (auto ktn = unifiable->isa<KnownTypeNode>()) {
            for (auto elem : ktn->elems())
                unify(elem);

            if (auto type_var = ktn->isa<TypeVarNode>()) {
                for (auto bound : type_var->bounds())
                    unify(bound);
            }
        } 

        auto p = unifiables_.insert(unifiable);
        assert(unifiable->representative() == unifiable);
        assert(p.second && "hash/equal broken");
        return unifiable;
    }
}

PrimType TypeTable::type(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

}
