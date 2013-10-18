#include "type.h"

#include <iostream>

using namespace anydsl2;

int TypeVar::counter = 0;

//------------------------------------------------------------------------------

size_t Type::hash() const {
    // TODO take type variables of generic types better into the equation
    size_t seed = hash_combine(hash_value((int) kind()), size());
    seed = hash_combine(seed, num_bound_vars());
    for (auto elem : elems())
        seed = hash_combine(seed, elem);
    return seed;
}

bool Type::equal(const Type* other) const {
    if (this->get_representative() == other->get_representative()) {
        return true;
    } else if (this->is_unified() && other->is_unified()) {
        return false;
    }

    bool result = this->kind() == other->kind();
    result &= this->size() == other->size();
    result &= this->num_bound_vars() == other->num_bound_vars();

    // set equivalence constraints for type variables
    for (size_t i = 0, e = num_bound_vars(); i != e; ++i) {
        this->bound_var(i)->set_equiv_variable(other->bound_var(i));
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
    for (auto t : elems_) {
        if (! t->is_closed()) {
            return false;
        }
    }
    return true;
}

std::string Type::bound_vars_to_string() const {
    std::string result;

    if (!is_generic())
        return result;

    const char* separator = "<";
    for (auto v : bound_vars()) {
        result += separator + v->to_string();
        separator = ", ";
    }
    return result + '>';
}


void Type::dump() const { std::cout << to_string() << std::endl; }

//------------------------------------------------------------------------------

std::string PrimType::to_string() const {
    switch (primtype_kind()) {
#define PRIMTYPE(T) case PrimType_##T: return #T;
#include "primtypes.h"
        default: ANYDSL2_UNREACHABLE;
    }
}

std::string CompoundType::elems_to_string() const {
    std::string result;

    if (is_empty())
        return result;

    const char* separator = "(";
    for (auto elem : elems()) {
        result += separator + elem->to_string();
        separator = ", ";
    }
    return result + ')';
}

bool TypeVar::equal(const Type* other) const {
    // TODO is this correct for a instanceof-equivalent?
    if (const TypeVar* t = other->isa<TypeVar>()) {
        // we do not use && because for performance reasons we only set the
        // equiv_var on one side (even the right side of the || should never
        // be executed)
        return (*this->equiv_var_ == t) || (*t->equiv_var_ == this);
    }
    return false;
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : types_()
#define PRIMTYPE(T) , T##_(unify(new PrimType(*this, PrimType_##T)))
#include "primtypes.h"
    , type_error_(unify(new TypeError(*this)))
{}

FnType* TypeTable::fntype_simple(TypeArray params, Type* return_type) {
    FnType* retfun = fntype( { return_type });

    size_t psize = params.size();

    Type** p = new Type*[psize + 1];

    for (int i = 0; i < psize; ++i) {
        p[i] = params[i];
    }
    p[psize] = retfun;

    return fntype(TypeArray(p, psize + 1));
}

void TypeTable::insert_new(Type* type) {
    assert(!type->is_unified());

    // TODO is this a correct instanceof test? -- maybe use a virtual method instead?
    if (type->isa<TypeVar>())
        return;

    for (auto elem : type->elems()) {
        if (!elem->is_unified()) {
            insert_new(elem);
        }
    }

    type->set_representative(type);

    auto p = types_.insert(type);
    assert(p.second && "hash/equal broken");
}

/**
 * Recursivly change the representatives of the not-unified types in t to the
 * corresponding types in repr.
 */
void change_repr(const Type* repr, Type* t) {
    assert(repr->is_final_representative());

    if (t->is_unified()) {
        assert(t == repr);
        return;
    }

    for (size_t i = 0, e = t->size(); i != e; ++i) {
        change_repr(repr->elem(i), t->elem(i));
    }

    t->set_representative(repr);
}

Type* TypeTable::unify_base(Type* type) {
    // unify only closed types (i.e. only types where all type variables have been bound)
    if (! type->is_closed()) {
        return type;
    }

    assert(!type->is_unified());

    auto i = types_.find(type);

    if (i != types_.end()) {
        if (*i != type) {
            // TODO reset the representative of all not-unified (sub-)types
            assert((*i)->is_final_representative());

            change_repr(*i, type);
        }
        return *i;
    }

    insert_new(type);

    assert(type->is_unified());
    return type;
}

PrimType* TypeTable::primtype(PrimTypeKind kind) {
    switch (kind) {
#define PRIMTYPE(T) case PrimType_##T: return T##_;
#include "primtypes.h"
        default: ANYDSL2_UNREACHABLE;
    }
}

//------------------------------------------------------------------------------
