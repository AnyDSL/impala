#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

#include <iostream>

using namespace thorin;

namespace impala {

int TypeVarNode::counter = 0;

//------------------------------------------------------------------------------

size_t TypeNode::hash() const {
    // TODO take type variables of generic types better into the equation
    // TODO perhaps store this hash so it does not need to be recomputed all the time
    size_t seed = hash_combine(hash_value((int) kind()), size());
    seed = hash_combine(seed, num_bound_vars());
    for (auto elem : elems_)
        seed = hash_combine(seed, elem->hash());

    return seed;
}

bool TypeNode::equal(const GenericElement* other) const {
    // TODO is this correct for a instanceof-equivalent?
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
        for (auto r : *v->restricted_by()) {
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
    auto trestr = other->restricted_by();

    if (this->restricted_by()->size() != trestr->size())
        return false;

    // TODO this does work but seems too much effort, at least use a set that uses representatives
    TraitInstanceNodeTableSet ttis;
    for (auto r : *trestr) {
        auto p = ttis.insert(r.representative());
        assert(p.second && "hash/equal broken");
    }

    // this->restricted_by() subset of trestr
    for (auto r : *this->restricted_by()) {
        if (ttis.find(r.representative()) == ttis.end()) {
            return false;
        }
    }

    return true;
}

bool TypeVarNode::equal(const TypeNode* other) const {
    if (this == other)
        return true;

    // TODO is this correct for a instanceof-equivalent?
    if (const TypeVarNode* t = other->isa<TypeVarNode>()) {
        if ((this->equiv_var_ == nullptr) && (t->equiv_var_ == nullptr)) {
            if (this->bound_at_ == nullptr) {
                return false;
            } else {
                //return this->bound_at_->equal(t->bound_at_); TODO AND: they must be bound at the same position!
                return false;
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

void TypeVarNode::add_restriction(TraitInstance restriction) {
    assert(!is_closed() && "Closed type variables must not be changed!");
    auto p = restricted_by_.insert(restriction);
    assert(p.second && "hash/equal broken");
}

bool TypeVarNode::is_closed() const {
    return bound_at_ != nullptr;
}

//------------------------------------------------------------------------------

Type TypeNode::clone() const {
    if (clone_.empty()) {
        for (TypeVar v : bound_vars())
            v->set_bound_clone();

        set_clone();
        assert(!clone_.empty());

        for (TypeVar v : bound_vars())
            clone_->add_bound_var(v->clone());
    }

    return clone_;
}

thorin::AutoPtr<std::vector<Type>> CompoundType::clone_elems() const {
    thorin::AutoPtr<std::vector<Type>> clones(new std::vector<Type>());
    for (size_t i = 0; i < size(); ++i)
        clones->push_back(elem(i)->clone());
    return clones;
}

void TypeErrorNode::set_clone() const { clone_ = typetable().type_error(); }
void PrimTypeNode::set_clone() const { clone_ = typetable().primtype(primtype_kind()); }
void FnTypeNode::set_clone() const { clone_ = typetable().fntype(*clone_elems()); }
void TupleTypeNode::set_clone() const { /*clone_ = typetable().tupletype(*clone_elems()); TODO*/ }

void TypeVarNode::set_clone() const {
    // was not bound in the cloned type -> return orginal type var
    assert(clone_.empty());
    clone_ = typetable().new_type(this);
}

void TypeVarNode::set_bound_clone() const {
    assert(clone_.empty());
    clone_ = typetable().typevar();
    // TODO consider bounds!
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
