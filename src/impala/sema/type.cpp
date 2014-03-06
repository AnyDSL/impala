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

bool TypeNode::is_subtype(const TypeNode* super_type) const {
    assert(super_type != nullptr);

    if (this == super_type)
        return true;

    for (auto t : super_type->elems_) {
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

void TypeNode::add_implementation(Trait impl) {
    auto p = trait_impls_.insert(impl);
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
    /*for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = elem(i)->specialize(mapping); FIXME how can we cast this? */
    return nelems;
}

// FIXME this is only a hack
Generic* TypeErrorNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = (Generic*) typetable().type_error().node(); }
Generic* PrimTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = (Generic*) typetable().primtype(primtype_kind()).node(); }
Generic* FnTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = (Generic*) typetable().fntype(specialize_elems(mapping)).node(); }
Generic* TupleTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = (Generic*) typetable().tupletype(specialize_elems(mapping)).node(); }

Generic* TypeVarNode::vspecialize(SpecializeMapping& mapping) {
    // was not bound in the specialized type -> return orginal type var
    return mapping[const_cast<TypeVarNode*>(this)] = (Generic*) typetable().new_unifiable(this).node(); // HACK
}

TypeVar TypeVarNode::clone(SpecializeMapping& mapping) const {
    TypeVar v = typetable().typevar();

    // copy bounds!
    /*for (Trait b : bounds())
        v->add_bound(b->specialize(mapping)); FIXME how can we cast this?*/

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

    for (auto t1 : types) {
        for (auto t2 : types) {
            if (t1.is_unified() && t2.is_unified()) {
                if (!((!t1->equal(t2)) || (t1 == t2))) {
                    t1->dump();
                    t2->dump();
                    assert(false);
                }
            }
        }
    }
}

}
