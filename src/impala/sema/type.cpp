#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

int TypeVarNode::counter = 0;

size_t TraitImplHash::operator () (const TraitInstance t) const{ return t->hash(); }
bool TraitImplEqual::operator () (const TraitInstance t1, const TraitInstance t2) const {
    // FEATURE consider generic implementations -- this will need some type inference magic!
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
        if (this->is_subtype(t.representative()))
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

void TypeVarNode::add_bound(TraitInstance restriction) {
    assert(!is_closed() && "Closed type variables must not be changed!");
    auto p = bounds_.insert(restriction);
    assert(p.second && "hash/equal broken");
}

bool TypeVarNode::is_closed() const {
    return bound_at_ != nullptr;
}

//------------------------------------------------------------------------------

void TypeNode::add_implementation(const TraitImpl* impl) {
    auto p = trait_impls_.insert(impl->trait_inst());
    assert(p.second && "hash/equal broken");
}

bool TypeNode::implements(TraitInstance trait) const {
    // CHECK is this enough?
    return trait_impls_.find(trait) != trait_impls_.end();
}

bool TypeVarNode::implements(TraitInstance trait) const {
    // CHECK is this enough?
    return bounds().find(trait) != bounds().end();
}

//------------------------------------------------------------------------------

Type TypeNode::instantiate(thorin::ArrayRef<Type> var_instances) const {
    SpecializeMapping mapping = check_instantiation(var_instances);

    Type instance = vspecialize(mapping);
    typetable().unify(instance);

    return instance;
}

Type TypeNode::specialize(SpecializeMapping& mapping) const {
    // FEATURE this could be faster if we copy only types were something changed inside
    auto it = mapping.find(this);
    if (it != mapping.end())
        return it->second;

    for (TypeVar v : bound_vars()) {
        assert(mapping.find(v.representative()) == mapping.end());
        mapping[v.representative()] = v->clone(mapping);
    }

    Type t = vspecialize(mapping);

    for (TypeVar v : bound_vars()) {
        assert(mapping.find(v.representative()) != mapping.end());
        t->add_bound_var(mapping[v.representative()]);
    }

    return t;
}

thorin::Array<Type> CompoundType::specialize_elems(SpecializeMapping& mapping) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = elem(i)->specialize(mapping);
    return nelems;
}

Type TypeErrorNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().type_error(); }
Type NoReturnTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().type_noreturn(); }
Type PrimTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().primtype(primtype_kind()); }
Type FnTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().fntype(specialize_elems(mapping)); }
Type TupleTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().tupletype(specialize_elems(mapping)); }

Type TypeVarNode::vspecialize(SpecializeMapping& mapping) const {
    // was not bound in the specialized type -> return orginal type var
    // FIXME we need to create a new copy here - else unification will lead to segmentation faults!!
    return mapping[this] = typetable().new_type(this);
}

TypeVar TypeVarNode::clone(SpecializeMapping& mapping) const {
    TypeVar v = typetable().typevar();

    // copy bounds!
    for (TraitInstance b : bounds())
        v->add_bound(b->specialize(mapping));

    return v;
}

void TypeVarNode::refresh_bounds() {
    std::vector<TraitInstance> tmp;
    for (TraitInstance i : bounds())
        tmp.push_back(i);
    bounds_.clear();
    for (TraitInstance i : tmp) {
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
                if (!((!t1.representative()->equal(t2.representative())) || (t1.representative() == t2.representative()))) {
                    t1->dump();
                    t2->dump();
                    assert(false);
                }
            }
        }
    }
}

}
