#include "impala/sema/typetable.h"

namespace impala {

TypeTable::TypeTable()
    : type_noret_(unify(join(new NoRetTypeNode(*this))))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(join(new PrimTypeNode(*this, PrimType_##itype))))
#include "impala/tokenlist.h"
    , type_error_(unify(join(new TypeErrorNode(*this))))
{
    trait_abs_error_ = trait_abs(nullptr);
    trait_abs_error_->bind(type_var());
    unify(trait_abs_error_);
    trait_app_error_ = unify(trait_app(trait_abs_error(), {type_error()}));
}

TypeTable::~TypeTable() {
    for (auto g : garbage_)
        delete g;
}

PrimType TypeTable::prim_type(const PrimTypeKind kind) {
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

    auto i = unifiables_.find(unifiable);
    assert(!unifiable->is_unified());

    if (i != unifiables_.end()) {
        auto repr = *i;
        assert(repr != unifiable && "already unified");
        unifiable->representative_ = repr;
        return repr;
    } else {
        assert(!unifiable->is_unified());
        unifiable->representative_ = unifiable;

        for (auto type_var : unifiable->type_vars())
            unify(type_var);

        for (auto arg : unifiable->args()) {
            if (arg)
                unify(arg);
        }

        if (auto type_var = unifiable->isa<TypeVarNode>()) {
            for (auto bound : type_var->bounds())
                unify(bound);
            std::stable_sort(type_var->bounds_.begin(), type_var->bounds_.end(), TraitAppLT());
        }

        const auto& p = unifiables_.insert(unifiable);
        assert(unifiable->representative() == unifiable);
        assert_unused(p.second && "hash/equal broken");
        return unifiable;
    }
}

void TypeTable::verify() const {
    for (auto g : unifiables_) {
        assert(g != nullptr);
        if (auto type = g->isa<TypeNode>()) {
            assert_unused(type->is_known());
            assert_unused(type->is_sane());
        } else if (auto trait = g->isa<TraitAbsNode>()) {
            assert_unused(trait->is_closed());
        } else if (auto impl = g->isa<ImplNode>()) {
            assert_unused(impl->is_closed());
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
        } else if (auto trait = g->isa<TraitAbsNode>()) {
            if (trait->is_unified())
                assert(unifiables_.contains(trait->representative()));
        } else if (auto impl = g->isa<ImplNode>()) {
            if (impl->is_unified())
                assert(unifiables_.contains(impl->representative()));
        }
    }
}

}
