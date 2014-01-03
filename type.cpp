#include "type.h"
#include "trait.h"
#include "TypeTable.h"

#include <iostream>

using namespace thorin;

int TypeVar::counter = 0;

//------------------------------------------------------------------------------

size_t Type::hash() const {
    // TODO take type variables of generic types better into the equation
    // TODO perhaps store this hash so it does not need to be recomputed all the time
    size_t seed = hash_combine(hash_value((int) kind()), size());
    seed = hash_combine(seed, num_bound_vars());
    for (auto elem : elems_)
        seed = hash_combine(seed, elem->hash());

    return seed;
}

bool Type::equal(const GenericElement* other) const {
    // TODO is this correct for a instanceof-equivalent?
    if (const Type* t = other->isa<Type>()) {
        return equal(t);
    }
    return false;
}

bool Type::equal(const Type* other) const {
    if (this->is_unified() && other->is_unified()) {
        return this->get_representative() == other->get_representative();
    }

    bool result = this->kind() == other->kind();
    result &= this->size() == other->size();
    result &= this->num_bound_vars() == other->num_bound_vars();

    // set equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i) {
        this->bound_var(i)->set_equiv_variable(other->bound_var(i));
    }

    // check equality of the restrictions of the type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i) {
        result &= this->bound_var(i)->restrictions_equal(other->bound_var(i));
    }

    for (size_t i = 0, e = size(); i != e && result; ++i) {
        result &= this->elem(i)->equal(other->elem(i));
    }

    // unset equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i) {
        this->bound_var(i)->unset_equiv_variable();
    }

    return result;
}

bool Type::is_closed() const {
    for (auto v : bound_vars()) {
        for (auto r : *v->restricted_by()) {
            if (! r->is_closed()) {
                return false;
            }
        }
    }

    for (auto t : elems_) {
        if (! t->is_closed()) {
            return false;
        }
    }
    return true;
}

bool Type::is_subtype(const Type* super_type) const {
    assert(super_type != nullptr);

    if (this == super_type)
        return true;

    for (auto t : super_type->elems_) {
        if (this->is_subtype(t)) {
            return true;
        }
    }
    return false;
}

bool Type::is_sane() const {
    for (auto t : elems_) {
        if (!t->is_sane()) {
            return false;
        }
    }
    assert(is_closed());
    return true;
}

void Type::dump() const { std::cout << to_string() << std::endl; }

//------------------------------------------------------------------------------

std::string PrimType::to_string() const {
    switch (primtype_kind()) {
#define PRIMTYPE(T) case PrimType_##T: return #T;
#include "primtypes.h"
        default: THORIN_UNREACHABLE;
    }
}

std::string CompoundType::elems_to_string() const {
    std::string result;

    if (is_empty())
        return result;

    const char* separator = "(";
    for (auto elem : elems_) {
        result += separator + elem->to_string();
        separator = ", ";
    }
    return result + ')';
}

bool TypeVar::restrictions_equal(const TypeVar* other) const {
    auto trestr = other->restricted_by();

    if (this->restricted_by()->size() != trestr->size())
        return false;

    TraitInstanceTableSet ttis;
    for (auto r : *trestr) {
        auto p = ttis.insert(r);
        assert(p.second && "hash/equal broken");
    }

    // TODO this will not work!
    // this->restricted_by() subset of trestr
    for (auto r : *this->restricted_by()) {
        if (ttis.find(r) == ttis.end()) {
            return false;
        }
    }

    return true;
}

bool TypeVar::equal(const Type* other) const {
    if (this->is_unified() && other->is_unified()) {
        return this->get_representative() == other->get_representative();
    }

    // TODO is this correct for a instanceof-equivalent?
    if (const TypeVar* t = other->isa<TypeVar>()) {
        if ((this->equiv_var_ == nullptr) && (t->equiv_var_ == nullptr)) {
            assert(this->bound_at_ != nullptr);
            return this->bound_at_->equal(t->bound_at_);
        } else {
            // we do not use && because for performance reasons we only set the
            // equiv_var on one side (even the right side of the || should never
            // be executed)
            return (this->equiv_var_ == t) || (t->equiv_var_ == this);
        }
    }
    return false;
}

void TypeVar::bind(const GenericElement* const e) {
    if (bound_at_ != nullptr) {
        throw IllegalTypeException("type variables can only be bound once!");
    }
    // restrict type variables by top trait if there are no other restrictions
    if (restricted_by()->empty()) {
        add_restriction(typetable().top_trait_inst());
    }
    bound_at_ = e;
}

void TypeVar::add_restriction(TypeTraitInstance* restriction) {
    if (is_closed())
        throw IllegalTypeException("Closed type variables must not be changed!");

    auto p = restricted_by_.insert(restriction);
    assert(p.second && "hash/equal broken");
}

bool TypeVar::is_closed() const {
    return bound_at_ != nullptr;
}

std::string TypeVar::to_string() const {
    if (id_ < 26) {
        return std::string(1, 'A' + id_);
    } else {
        return std::string("Z") + std::to_string(id_);
    }
}

//------------------------------------------------------------------------------

void check_sanity(thorin::ArrayRef<const Type*> types) {
    for (auto t : types) {
        assert(t->is_sane());
    }

    for (auto t1 : types) {
        for (auto t2 : types) {
            if (t1->is_unified() && t2->is_unified()) {
                if (!((!t1->equal(t2)) || (t1->get_representative() == t2->get_representative()))) {
                    t1->dump();
                    t2->dump();
                    assert(false);
                }
            }
        }
    }
}

