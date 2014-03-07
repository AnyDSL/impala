#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

int TypeVarNode::counter = 0;

// TODO review this
size_t TraitImplHash::operator () (const Trait t) const{ return t->hash(); }
bool TraitImplEqual::operator () (const Trait t1, const Trait t2) const {
    // FEATURE consider generic implementations, ...
    return t1 == t2;
}

//------------------------------------------------------------------------------

bool TypeNode::is_closed() const {
    for (auto v : bound_vars()) {
        for (auto r : v->bounds()) {
            if (! r->is_closed())
                return false;
        }
    }

    for (auto t : elems()) {
        if (! t->is_closed())
            return false;
    }
    return true;
}

// TODO test this
bool TypeNode::is_subtype(const Type super_type) const {
    if (this == *super_type)
        return true;

    for (Type t : super_type->elems()) {
        if (this->is_subtype(t))
            return true;
    }
    return false;
}

bool TypeNode::is_sane() const {
    for (auto t : elems_) {
        if (!t->is_sane()) {
            return false;
        }
    }
    assert(is_closed());
    return true;
}

//------------------------------------------------------------------------------

void TypeVarNode::bind(const Generic* const e) {
    assert(bound_at_ == nullptr && "type variables can only be bound once!");
    bound_at_ = e;
}

void TypeVarNode::add_bound(Trait bound) {
    assert(!is_closed() && "Closed type variables must not be changed!");
    auto p = bounds_.insert(bound);
    assert(p.second && "hash/equal broken");
}

bool TypeVarNode::is_closed() const {
    return bound_at_ != nullptr;
}

//------------------------------------------------------------------------------

void TypeNode::add_implementation(TraitImpl impl) {
    auto p = trait_impls_.insert(impl->trait());
    assert(p.second && "hash/equal broken");
}

bool TypeNode::implements(Trait trait) const {
    // CHECK is this enough?
    return trait_impls_.find(trait) != trait_impls_.end();
}

bool TypeVarNode::implements(Trait trait) const {
    // CHECK is this enough?
    return bounds().find(trait) != bounds().end();
}

//------------------------------------------------------------------------------

thorin::Array<Type> CompoundType::specialize_elems(SpecializeMapping& mapping) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = Type(elem(i)->specialize(mapping)->as<TypeNode>());
    return nelems;
}

Generic* TypeErrorNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().type_error().node(); }
Generic* NoReturnTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().type_noreturn().node(); }
Generic* PrimTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().primtype(primtype_kind()).node(); }
Generic* FnTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().fntype(specialize_elems(mapping)).node(); }
Generic* TupleTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().tupletype(specialize_elems(mapping)).node(); }

Generic* TypeVarNode::vspecialize(SpecializeMapping& mapping) {
    // was not bound in the specialized type -> return orginal type var
    // CHECK do we need to create a new copy here? unification lead to segmentation faults in the past...
    return mapping[this] = this;
}

TypeVar TypeVarNode::clone(SpecializeMapping& mapping) const {
    TypeVar v = typetable().typevar();

    // copy bounds!
    for (Trait b : bounds())
        v->add_bound(Trait(b->specialize(mapping)->as<TraitNode>()));

    return v;
}

void TypeVarNode::refresh_bounds() {
    std::vector<Trait> tmp;
    for (Trait i : bounds())
        tmp.push_back(i);
    bounds_.clear();
    for (Trait i : tmp) {
        auto p = bounds_.insert(i);
        assert(p.second && "hash/equal broken");
    }
}

//------------------------------------------------------------------------------

void verify(thorin::ArrayRef<const Type> types) {
    for (auto t : types)
        assert(t->is_sane());

    for (Type ty1 : types) {
        TypeNode* t1 = ty1.node();
        for (Type ty2 : types) {
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
