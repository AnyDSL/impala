#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

#include <iostream>

using namespace thorin;

namespace impala {

int TypeVarNode::counter = 0;

//------------------------------------------------------------------------------

size_t TypeNode::hash() const {
    // FEATURE take type variables of generic types better into the equation
    size_t seed = hash_combine(hash_value((int) kind()), size());
    seed = hash_combine(seed, num_bound_vars());
    for (auto elem : elems_)
        seed = hash_combine(seed, elem->hash());

    return seed;
}

bool TypeNode::equal(const GenericElement* other) const {
    if (const TypeNode* t = other->isa<TypeNode>()) {
        return equal(t);
    }
    return false;
}

bool TypeNode::equal(const TypeNode* other) const {
    bool result = this->kind() == other->kind();
    result &= this->size() == other->size();
    result &= this->num_bound_vars() == other->num_bound_vars();

    if (!result)
        return false;

    // set equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i)
        this->bound_var(i)->set_equiv_variable(other->bound_var(i).representative());

    // check equality of the restrictions of the type variables
    for (size_t i = 0, e = num_bound_vars(); i != e && result; ++i)
        result &= this->bound_var(i)->restrictions_equal(other->bound_var(i));

    for (size_t i = 0, e = size(); i != e && result; ++i)
        result &= this->elem(i)->equal(other->elem(i).representative());

    // unset equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i)
        this->bound_var(i)->unset_equiv_variable();

    return result;
}

bool TypeNode::is_closed() const {
    for (auto v : bound_vars()) {
        for (auto r : *v->bounds()) {
            if (! r->is_closed())
                return false;
        }
    }

    for (auto t : elems_) {
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

void TypeNode::dump() const { std::cout << to_string() << std::endl; }

//------------------------------------------------------------------------------

bool TypeVarNode::restrictions_equal(const TypeVar other) const {
    auto trestr = other->bounds();

    if (this->bounds()->size() != trestr->size())
        return false;

    // FEATURE this does work but seems too much effort, at least use a set that uses representatives
    TraitInstanceNodeTableSet ttis;
    for (auto r : *trestr) {
        auto p = ttis.insert(r.representative());
        assert(p.second && "hash/equal broken");
    }

    // this->bounds() subset of trestr
    for (auto r : *this->bounds()) {
        if (ttis.find(r.representative()) == ttis.end()) {
            return false;
        }
    }

    return true;
}

bool TypeVarNode::equal(const TypeNode* other) const {
    if (this == other)
        return true;

    if (const TypeVarNode* t = other->isa<TypeVarNode>()) {
        if ((this->equiv_var_ == nullptr) && (t->equiv_var_ == nullptr)) {
            if (this->bound_at() == nullptr) { // unbound type vars are by definition unequal
                return false;
            } else {
                // two type vars are equal if the types where they are bound are
                // equal and they are bound at the same position
                bool result = bound_at()->num_bound_vars() == t->bound_at()->num_bound_vars();
                size_t i;
                for (i = 0; (i < bound_at()->num_bound_vars()) && result; ++i) {
                    if (bound_at()->bound_var(i).representative() == this) {
                        result &= t->bound_at()->bound_var(i).representative() == t;
                        break;
                    }
                }
                assert(i < bound_at()->num_bound_vars()); // it should have been found!

                return result && bound_at()->equal(t->bound_at());
            }

        } else {
            // we do not use && because for performance reasons we only set the
            // equiv_var on one side (even the right side of the || should never
            // be executed)
            return (this->equiv_var_ == t) || (t->equiv_var_ == this);
        }
    }
    return false;
}

void TypeVarNode::bind(const GenericElement* const e) {
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

Type TypeNode::instantiate(thorin::ArrayRef<Type> var_instances) const {
    check_instantiation(var_instances);

    SpecializeMapping mapping;
    assert(num_bound_vars() == var_instances.size());
    size_t i = 0;
    for (TypeVar v : bound_vars())
        mapping[v.representative()] = var_instances[i];

    Type instance = vspecialize(mapping);
    assert(instance->is_sane());
    return instance;
}

Type TypeNode::specialize(SpecializeMapping& mapping) const {
    // FEATURE this could be faster if we copy only types were something changed inside
    auto it = mapping.find(this);
    if (it != mapping.end())
        return it->second;

    for (TypeVar v : bound_vars()) {
        assert(mapping.find(v.representative()) == mapping.end());
        mapping[v.representative()] = v->clone();
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
Type PrimTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().primtype(primtype_kind()); }
Type FnTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().fntype(specialize_elems(mapping)); }
Type TupleTypeNode::vspecialize(SpecializeMapping& mapping) const { return mapping[this] = typetable().tupletype(specialize_elems(mapping)); }

Type TypeVarNode::vspecialize(SpecializeMapping& mapping) const {
    // was not bound in the specialized type -> return orginal type var
    return mapping[this] = typetable().new_type(this);
}

TypeVar TypeVarNode::clone() const {
    return typetable().typevar();
    // FEATURE consider bounds!
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
