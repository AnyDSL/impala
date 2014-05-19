#include "impala/sema/typetable.h"

namespace impala {

TypeTable::TypeTable()
    : type_error_(unify(join(new TypeErrorNode(*this))))
    , trait_error_(unify(trait(nullptr)))
    , bound_error_(unify(bound(trait_error(), {})))
    , type_noret_(unify(join(new NoRetTypeNode(*this))))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(join(new PrimTypeNode(*this, PrimType_##itype))))
#include "impala/tokenlist.h"
{}

TypeTable::~TypeTable() { 
    for (auto g : garbage_) 
        delete g; 
}

PrimType TypeTable::type(const PrimTypeKind kind) {
    switch (kind) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const Unifiable* TypeTable::unify(const Unifiable* unifiable) {
    assert(unifiable->is_closed() && "only closed unifiables can be unified!");

    if (unifiable->is_unified())
        return unifiable->representative();

    if (!unifiable->is_known())
        return unifiable;

    assert(!unifiable->isa<UnknownTypeNode>());
    auto i = unifiables_.find(unifiable);
    if (i != unifiables_.end()) {
        auto repr = *i;
        assert(repr != unifiable && "already unified");
        unifiable->representative_ = repr;
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
        } else if (auto bound = unifiable->isa<BoundNode>()) {
            for (auto type_arg : bound->type_args())
                type_arg->unify();
        }

        auto p = unifiables_.insert(unifiable);
        assert(unifiable->representative() == unifiable);
        assert(p.second && "hash/equal broken");
        return unifiable;
    }
}

void TypeTable::verify() const {
    for (auto g : unifiables_) {
        assert(g != nullptr);
        if (auto type = g->isa<TypeNode>()) {
            assert(type->is_known());
            assert(type->is_sane());
        } else if (auto trait = g->isa<TraitNode>()) {
            assert(trait->is_closed());
        } else if (auto impl = g->isa<ImplNode>()) {
            assert(impl->is_closed());
        }
    }

    for (size_t i = 0; i < garbage_.size(); ++i) {
        auto g = garbage_[i];
        assert(g != nullptr);

        // no element should be twice in the garbage vector - else deletion will fail!
        for (size_t j = i+1; j < garbage_.size(); ++j)
            assert(g != garbage_[j]);

        if (auto type = g->isa<TypeNode>()) {
            if (type->is_unified())
                assert(unifiables_.contains(type->representative()));
        } else if (auto trait = g->isa<TraitNode>()) {
            if (trait->is_unified())
                assert(unifiables_.contains(trait->representative()));
        } else if (auto impl = g->isa<ImplNode>()) {
            if (impl->is_unified())
                assert(unifiables_.contains(impl->representative()));
        }
    }
}

}
