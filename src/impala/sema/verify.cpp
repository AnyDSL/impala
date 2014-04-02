#include "impala/sema/typetable.h"

namespace impala {

void Generic::verify_instantiation(SpecializeMap& map) const {
    assert(map.size() == num_bound_vars());

    // check the bounds
    for (auto v : bound_vars()) {
        auto it = map.find(*v);
        assert(it != map.end());
        Type instance = Type(it->second->as<TypeNode>());

        for (auto bound : v->bounds()) {
            SpecializeMap m(map); // copy the map
            Trait spec_bound = Trait(bound->specialize(m)->as<TraitNode>());
            spec_bound->typetable().unify(spec_bound);
            assert(instance->implements(spec_bound));
        }
    }
}

void TypeTable::verify() const {
    for (auto g : unifiables_) {
        assert(g != nullptr);
        if (auto type = g->isa<TypeNode>()) {
            assert(type->is_known());
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

    for (size_t i = 0; i < garbage_.size(); ++i) {
        Generic* g = garbage_[i];
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
        } else if (auto impl = g->isa<TraitImplNode>()) {
            if (impl->is_unified())
                assert(unifiables_.contains(impl->representative()));
        }
    }
}

void verify(thorin::ArrayRef<const Type> types) {
    for (auto t : types)
        assert(t->is_sane());

    for (auto ty1 : types) {
        TypeNode* t1 = ty1.node();
        for (auto ty2 : types) {
            TypeNode* t2 = ty2.node();
            if (t1->is_unified() && t2->is_unified()) {
                if (!((!t1->equal(t2)) || (t1->representative() == t2->representative()))) {
                    t1->dump();
                    t2->dump();
                    assert(false);
                }
            }
        }
    }
}

}
