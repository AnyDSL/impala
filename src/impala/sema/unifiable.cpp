#include "impala/sema/unifiable.h"

#include "thorin/util/assert.h"

#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

int Unifiable::counter_ = 0;

bool Unifiable::unify_type_vars(thorin::ArrayRef<TypeVar> other_vars) const {
    if (num_type_vars() == other_vars.size())
        return !is_generic(); // TODO enable unification of generic elements!
    return false;
}

void Unifiable::refine_type_vars() const {
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

void Unifiable::bind(TypeVar v) const {
    assert(!v->is_closed() && "type variables already bound");
    assert(!is_unified() && "type already unified");
    assert(v->bound_at_ == nullptr && "type variables can only be bound once");
    // CHECK should 'forall a, a' be forbidden?
    //assert(type->kind() != Type_var && "Types like 'forall a, a' are forbidden!");

    v->bound_at_ = this;
    type_vars_.push_back(v);
}

bool Unifiable::unify() const { return typetable().unify(this); }

void Unifiable::set_representative(const Unifiable* repr) const {
    assert(repr == repr->representative_);
    assert(num_type_vars() == repr->num_type_vars());

    if (representative_ != repr) {
        representative_ = repr;

        for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
            auto& bounds = type_var(i)->bounds();
            type_var(i).node()->set_representative(repr->type_var(i).representative());

            // change the representative of all bound variables
            for (auto repr_bound : repr->type_var(i)->bounds()) {
                assert(repr_bound.node() == repr_bound->representative_);
                assert(bounds.contains(repr_bound));
                (*bounds.find(repr_bound)).node()->representative_ = *repr_bound;
            }
        }

        if (auto ktn = isa<KnownTypeNode>()) {
            auto repr_ktn = repr->as<KnownTypeNode>();

            // recursively change representative of all sub elements
            assert(ktn->size() == repr_ktn->size());
            for (size_t i = 0, e = ktn->size(); i != e; ++i)
                ktn->elem(i).node()->set_representative(repr_ktn->elem(i).representative());
        }
    }
}

}
