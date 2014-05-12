#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

size_t UnknownTypeNode::hash() const {
    return is_instantiated() ? instance()->hash() : hash_begin((int) kind());
}

size_t KnownTypeNode::hash() const {
    // FEATURE take type variables of generic types better into the equation
    size_t seed = hash_combine(hash_begin((int) kind()), size());
    seed = hash_combine(seed, num_type_vars());
    for (auto elem : elems_)
        seed = hash_combine(seed, elem->hash());

    return seed;
}

size_t TraitNode::hash() const { return hash_value(trait_decl()); }
size_t BoundNode::hash() const { return trait()->hash(); } // FEATURE better hash function
size_t ImplNode::hash() const { return hash_value(impl_item()); }

//----------------------------------------------------------------------------------------

bool UnknownTypeNode::equal(const Unifiable* other) const {
    return is_instantiated() ? instance()->equal(other) : this == other;
}

bool KnownTypeNode::equal(const Unifiable* t) const {
    if (this == t) return true;
    if (auto utn = t->isa<const UnknownTypeNode>()) return utn->equal(this);

    if (auto other = t->isa<const KnownTypeNode>()) {
        bool result = this->kind() == other->kind() && this->size() == other->size() 
            && this->num_type_vars() == other->num_type_vars();

        if (result) {
            // set equivalence constraints for type variables
            for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
                assert(this->type_var(i)->equiv_ == nullptr);
                this->type_var(i)->equiv_ = *other->type_var(i);
            }

            // check equality of the restrictions of the type variables
            for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
                result &= this->type_var(i)->bounds_equal(other->type_var(i));

            for (size_t i = 0, e = size(); i != e && result; ++i)
                result &= this->elem(i)->equal(*other->elem(i));

            // unset equivalence constraints for type variables
            for (auto var : type_vars())
                var->equiv_ = nullptr;
        }

        return result;
    }

    return false;
}

bool TypeVarNode::bounds_equal(const TypeVar other) const {
    if (this->bounds().size() != other->bounds().size())
        return false;

    // FEATURE this works but seems too much effort, at least use a set that uses representatives
    TypetableSet<const BoundNode> obounds;
    for (auto r : other->bounds()) {
        auto p = obounds.insert(*r); // TODO is deref here and below correct?
        assert(p.second && "hash/equal broken");
    }

    // this->bounds() subset of trestr
    for (auto r : this->bounds()) {
        if (!obounds.contains(*r))
            return false;
    }

    return true;
}

bool TypeVarNode::equal(const Unifiable* other) const {
    if (auto type_var = other->isa<TypeVarNode>())
        return this == other || (this->equiv_ == nullptr && this->equiv_ == type_var);
    return false;
}

bool TraitNode::equal(const Unifiable* other) const {
    if (auto trait = other->isa<TraitNode>())
        return (this->trait_decl() == trait->trait_decl());
    return false;
}

bool BoundNode::equal(const Unifiable* other) const {
    if (auto bound = other->isa<BoundNode>()) {
        if (this->trait() == bound->trait()) {
            assert(this->num_args() == bound->num_args());
            for (size_t i = 0, e = num_args(); i != e; ++i) {
                if (this->arg(i) != bound->arg(i))
                    return false;
            }
            return true;
        }
    }
    return false;
}

}
