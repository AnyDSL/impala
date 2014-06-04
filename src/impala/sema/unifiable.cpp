#include "impala/sema/unifiable.h"

#include <iostream>

#include "thorin/util/assert.h"
#include "thorin/util/queue.h"

#include "impala/ast.h"
#include "impala/sema/typetable.h"

using thorin::hash_begin;
using thorin::hash_combine;
using thorin::hash_value;

namespace impala {

//------------------------------------------------------------------------------

int Unifiable::counter_ = 0;

void Unifiable::bind(TypeVar v) const {
    assert(!v->is_closed() && "type variables already bound");
    assert(!is_unified() && "type already unified");
    assert(v->bound_at_ == nullptr && "type variables can only be bound once");
    // CHECK should 'forall a, a' be forbidden?
    //assert(type->kind() != Type_var && "Types like 'forall a, a' are forbidden!");

    v->bound_at_ = this;
    type_vars_.push_back(v);
}

const Unifiable* Unifiable::unify() const { return typetable().unify(this); }
bool TypeNode::is(PrimTypeKind kind) const { return isa<PrimTypeNode>() && as<PrimTypeNode>()->primtype_kind() == kind; }

void KnownTypeNode::add_impl(Impl impl) const {
    impls_.push_back(impl);
#if 0
    // TODO fail if a method was implemented multiple times!
    if (impl->is_generic()) {
        gen_trait_impls_.push_back(impl);
    } else {
        Trait trait = impl->trait();
        if (!trait_impls_.insert(trait).second)
            typetable().error(impl->impl_item()) << "duplicated implementation of trait '" << trait << "'\n";
        for (auto super : trait->super_traits()) {
            if (!trait_impls_.insert(super).second)
                typetable().error(impl->impl_decl()) << "duplicated implementation of trait '" << super << "'\n";
        }
    }
#endif
}

Type FnTypeNode::return_type() const {
    if (!is_empty()) {
        if (auto fn = elems().back().isa<FnType>()) {
            if (fn->num_elems() == 1)
                return fn->elems().front();
            return typetable().tuple_type(fn->elems());
        }
    }
    return typetable().type_noret();
}

StructTypeNode::StructTypeNode(TypeTable& typetable, const StructDecl* struct_decl)
    : KnownTypeNode(typetable, Kind_tuple, Array<Type>(struct_decl->fields().size()))
    , struct_decl_(struct_decl)
{}

void TypeVarNode::add_bound(Bound bound) const {
    assert(!is_closed() && "closed type variables must not be changed");
    bounds_.push_back(bound);
}

bool TraitNode::add_super_bound(Bound bound) const {
    auto p = super_bounds_.insert(bound.unify());
    return p.second;
}

Bound TraitNode::super_bound(Trait trait) const {
    for (auto super : super_bounds()) {
        if (super->trait() == trait)
            return super;
    }
    return Bound();
}

void TraitNode::add_impl(Impl impl) const {
    type2impls_[impl->type().unify()].push_back(impl);
}

//------------------------------------------------------------------------------

/*
 * recursive properties
 */

bool Unifiable::is_known() const {
    for (auto type_var : type_vars()) {
        for (auto bound : type_var->bounds()) {
            if (!bound->is_known())
                return false;
        }
    }

    for (auto elem : elems()) {
        if (!elem->is_known())
            return false;
    }

    return true;
}

bool Unifiable::is_closed() const {
    for (auto type_var : type_vars()) {
        for (auto bound : type_var->bounds()) {
            if (!bound->is_closed())
                return false;
        }
    }

    for (auto elem : elems()) {
        if (!elem->is_closed())
            return false;
    }

    return true;
}

bool KnownTypeNode::is_sane() const {
    for (auto elem : elems()) {
        if (!elem->is_sane())
            return false;
    }
    assert(is_closed());
    return true;
}

//------------------------------------------------------------------------------

/*
 * hash
 */

size_t Unifiable::hash() const {
    size_t seed = hash_combine(hash_combine(hash_begin((int) kind()), num_elems()), num_type_vars());

    for (auto type_var : type_vars())
        seed = hash_combine(seed, type_var->num_bounds());

    for (auto elem : elems())
        seed = hash_combine(seed, elem->hash());

    return seed;
}

size_t BoundNode::hash() const { return hash_combine(Unifiable::hash(), trait()->id()); }
size_t TraitNode::hash() const { return hash_value(trait_decl()); }
size_t ImplNode::hash() const { return hash_value(impl_item()); }

//------------------------------------------------------------------------------

/*
 * equal
 */

bool Unifiable::equal(const Unifiable* other) const {
    assert(this->is_unified());
    assert(!other->isa<UnknownTypeNode>());

    if (other->is_unified() && this == other->representative()) // includes this == other
        return true;

    if (this->kind() == other->kind()) {
        bool result = this->num_elems() == other->num_elems() && this->num_type_vars() == other->num_type_vars();

        // check arity of type vars (= the number of bounds)
        for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
            result &= this->type_var(i)->num_bounds() == other->type_var(i)->num_bounds();

        if (result) {
            // set equivalence constraints for type variables
            for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
                assert(this->type_var(i)->equiv_ == nullptr);
                this->type_var(i)->equiv_ = *other->type_var(i);
            }

            // check equality of the restrictions of the type variables
            for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
                result &= this->type_var(i)->bounds_equal(*other->type_var(i));

            // check recursively element types for equivalence
            for (size_t i = 0, e = this->num_elems(); i != e && result; ++i)
                result &= this->elem(i)->equal(*other->elem(i));
                //result &= this->elem(i) == other->elem(i);

            // unset equivalence constraints for type variables
            for (auto var : type_vars())
                var->equiv_ = nullptr;
        }

        return result;
    }

    return false;
}

bool BoundsLT::operator () (Bound b1, Bound b2) const {
    assert(b1->is_unified() && b2->is_unified());
    if (b1->id() == b2->id()) return false;
    if (b1->trait()->id() < b2->trait()->id()) return true;
    if (b1->trait()->id() > b2->trait()->id()) return false;
    if (b1->num_elems() < b2->num_elems()) return true;
    if (b1->num_elems() > b2->num_elems()) return false;

    for (size_t i = 0, e = b1->num_elems(); i != e; ++i) {
        assert(b1->elem(i)->is_unified() && b2->elem(i)->is_unified());
        if (b1->elem(i)->id() < b2->elem(i)->id()) return true;
        if (b1->elem(i)->id() > b2->elem(i)->id()) return false;
    }

    THORIN_UNREACHABLE;
}

bool TypeVarNode::bounds_equal(const TypeVarNode* other) const {
    assert(this->is_unified());

    if (this->num_bounds() != other->num_bounds())
        return false;

    for (auto this_bound : this->bounds()) { // TODO this loop is quadratic
        for (auto other_bound : other->bounds()) {
            if (this_bound->equal(*other_bound))
                goto found;
        }
        return false;
found:;
    }
    return true;
}

bool TypeVarNode::equal(const Unifiable* other) const {
    assert(this->is_unified());

    if (auto type_var = other->isa<TypeVarNode>())
        return this == other || (this->equiv_ != nullptr && this->equiv_ == type_var);
    return false;
}

bool TraitNode::equal(const Unifiable* other) const {
    assert(this->is_unified());

    if (auto trait = other->isa<TraitNode>())
        return this->trait_decl() == trait->trait_decl();
    return false;
}

bool BoundNode::equal(const Unifiable* other) const {
    return Unifiable::equal(other) && this->trait()->equal(*other->as<BoundNode>()->trait());
}

//------------------------------------------------------------------------------

/*
 * specialize and instantiate
 */

SpecializeMap specialize_map(const Unifiable* unifiable, ArrayRef<Type> type_args) {
    assert(unifiable->num_type_vars() == type_args.size());
    SpecializeMap map;
    size_t i = 0;
    for (TypeVar v : unifiable->type_vars())
        map[*v] = *type_args[i++];
    assert(map.size() == type_args.size());
    return map;
}

Type instantiate_unknown(Type type, std::vector<Type>& type_args) {
    for (size_t i = 0, e = type->num_type_vars(); i != e;  ++i) 
        type_args.push_back(type->typetable().unknown_type());
    auto map = specialize_map(type, type_args);
    return type->instantiate(map);
}

Type TypeNode::specialize(SpecializeMap& map) const {
    if (auto result = thorin::find(map, this))
        return result;

    for (auto type_var : type_vars()) {
        assert(!map.contains(*type_var));
        auto new_type_var = typetable().type_var();
        map[*type_var] = *new_type_var;
        for (auto bound : type_var->bounds())
            new_type_var->add_bound(bound->specialize(map));
    }

    auto t = instantiate(map);
    for (auto type_var : type_vars())
        t->bind(map[*type_var]->as<TypeVarNode>());

    return t;
}

Array<Type> Unifiable::specialize_elems(SpecializeMap& map) const {
    Array<Type> nelems(num_elems());
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        nelems[i] = elem(i)->specialize(map);
    return nelems;
}

Type TypeNode::instantiate(SpecializeMap& map) const {
#ifndef NDEBUG
    for (auto type_var : type_vars())
        assert(map.contains(*type_var));
#endif
    return vinstantiate(map);
}

Type TypeNode::instantiate(ArrayRef<Type> type_args) const {
    assert(num_type_vars() == type_args.size());
    SpecializeMap map;
    for (size_t i = 0, e = num_type_vars(); i != e; ++i)
        map[*type_var(i)] = *type_args[i];
    return instantiate(map);
}

Type BorrowedPtrTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().borrowd_ptr_type(referenced_type()->specialize(map)); 
}

Type DefiniteArrayTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().definite_array_type(elem_type()->specialize(map), dim()); 
}

Type FnTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().fn_type(specialize_elems(map)); 
}

Type IndefiniteArrayTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().indefinite_array_type(elem_type()->specialize(map)); 
}

Type OwnedPtrTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().owned_ptr_type(referenced_type()->specialize(map)); 
}

Type StructTypeNode::vinstantiate(SpecializeMap& map) const { 
    assert(false && "TODO"); return nullptr; 
}

Type TupleTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().tuple_type(specialize_elems(map)); 
}

Type UnknownTypeNode::vinstantiate(SpecializeMap& map) const { 
    return map[this] = *typetable().unknown_type(); 
}

Type NoRetTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type PrimTypeNode ::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type TypeErrorNode::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type TypeVarNode  ::vinstantiate(SpecializeMap& map) const { return map[this] = this; }

Bound TraitNode::instantiate(ArrayRef<Type> type_args) const {
    return typetable().bound(this, type_args);
}

Bound BoundNode::specialize(SpecializeMap& map) const {
    Array<Type> new_elems(num_elems());
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        new_elems[i] = elem(i)->specialize(map);

    return typetable().bound(trait(), new_elems);
}

Impl ImplNode::specialize(SpecializeMap& map) const { 
    return typetable().impl(impl_item(), bound()->specialize(map), type());
}

//------------------------------------------------------------------------------

/*
 * infer
 */

bool infer(const Unifiable* u1, const Unifiable* u2) {
    assert(u1->is_closed() && u2->is_closed());

    if (u2->isa<UnknownTypeNode>())                                 // normalize to have the UnknownType as u1
        std::swap(u1, u2);
    if (u2->isa<UnknownTypeNode>())                                 // second one also an UnknownType?
        return false;                                               // ... cannot infer
    if (u1->is_unified())                                           // normalize to have the unified type as u2
        std::swap(u1, u2);

    if (u2->unify()->is_unified()) {                                // if u2 is unified we try to infer u1
        if (u1->isa<UnknownTypeNode>()) {
            assert(u2->representative()->isa<KnownTypeNode>());
            u1->representative_ = u2->representative();             // set u1 to u2
            return true;
        } else if (u1->unify()->is_unified()) {
            return u1->representative() == u2->representative();    // both are unified - are types equal?
        } else if (bool result = u1->kind() == u2->kind()           // recursively infer sub elements
                && u1->num_type_vars() == u2->num_type_vars() 
                && u1->num_elems() == u2->num_elems()) {
            // TODO handle type vars
            for (size_t i = 0, e = u1->num_elems(); i != e && result; ++i)
                result &= infer(u1->elem(i), u2->elem(i));

            if (auto b1 = u1->isa<BoundNode>())
                result &= b1->trait() == u2->as<BoundNode>()->trait();

            return result;
        }
    }

    return false;
}

bool infer(Uni u1, Uni u2) { return infer(u1->unify(), u2->unify()); }

//------------------------------------------------------------------------------

/*
 * implements
 */

bool KnownTypeNode::implements(Bound bound, SpecializeMap& map) const {
    if (!is_unified())
        return unify()->as<KnownTypeNode>()->implements(bound, map);

    auto implements_bounds = [&] (Type type, TypeVar type_var) {
        for (auto b : type_var->bounds()) {
            if (!type->implements(b, map))
                return false;
        }
        return true;
    };

    for (auto impl : bound->trait()->type2impls(this)) {
        // find out which of impl's type_vars match to which of impl->bounds' type args
        for (auto type_var : impl->type_vars()) {
            for (size_t i = 0, e = impl->bound()->num_elems(); i != e; ++i) { // TODO this is currently quadratic
                if (type_var.as<Type>() == impl->bound()->elem(i) 
                        && !bound->elem(i)->isa<UnknownTypeNode>()
                        && implements_bounds(bound->elem(i), type_var))
                    map[*type_var] = *bound->elem(i);
            }
        }

        if (bound == impl->specialize(map)->bound())
            return true;
    }

    return false;
}

Impl KnownTypeNode::fimd_impl(Bound bound) const {
    SpecializeMap map;
    for (auto impl : bound->trait()->type2impls(this)) {
        return impl;
        // find out which of impl's type_vars match to which of impl->bounds' type args
        for (auto type_var : impl->type_vars()) {
            for (size_t i = 0, e = impl->bound()->num_elems(); i != e; ++i) { // TODO this is currently quadratic
                if (type_var.as<Type>() == impl->bound()->elem(i) 
                        && !bound->elem(i)->isa<UnknownTypeNode>())
                    map[*type_var] = *bound->elem(i);
            }
        }

        if (bound == impl->specialize(map)->bound())
            return impl;
    }

    return Impl();
}

bool TypeVarNode::implements(Bound bound, SpecializeMap& map) const {
    if (!is_unified())
        return unify()->as<TypeVarNode>()->implements(bound, map);

    std::queue<Bound> queue;
    IdSet<Bound> done;

    auto enqueue = [&] (Bound bound) { 
        assert(bound->is_unified());
        queue.push(bound); 
        done.insert(bound); 
    };

    for (auto b : bounds())
        enqueue(b);

    while (!queue.empty()) {
        auto cur_bound = thorin::pop(queue);

        if (cur_bound == bound)
            return true;

        for (auto super_bound : cur_bound->trait()->super_bounds()) {
            map[*super_bound->elem(0)] = *cur_bound->elem(0); // propagate self type param
            auto spec_super_bound = super_bound->specialize(map).unify();
            if (!done.contains(spec_super_bound))
                enqueue(spec_super_bound);
        }
    }

    return false;
}

//------------------------------------------------------------------------------

/*
 * find_method
 */

FnType KnownTypeNode::find_method(Symbol name) const {
    for (auto impl : impls_) {
        if (auto fn = impl->bound()->find_method(name))
            return fn;
    }
    return FnType();
}

FnType TypeVarNode::find_method(Symbol name) const {
    for (auto bound : bounds()) {
        if (auto fn = bound->find_method(name)) 
            return fn;
    }
    return FnType();
}

FnType TraitNode::find_method(Symbol name) const {
    auto i = trait_decl()->method_table().find(name);
    if (i != trait_decl()->method_table().end())
        return i->second->fn_type();

    for (auto super : super_bounds()) {
        if (auto type = super->find_method(name))
            return type;
    }

    return FnType();
}

FnType BoundNode::find_method(Symbol name) const {
    auto i = method_cache_.find(name);
    if (i != method_cache_.end())
        return i->second;

    if (auto type = trait()->find_method(name)) {
        auto map = specialize_map(trait(), elems());
        return method_cache_[name] = type->specialize(map).as<FnType>().unify();
    }

    return FnType();
}

//------------------------------------------------------------------------------

/*
 * to_string
 */

/*
 * TODO remove copy & paste code
 */

void Unifiable::dump() const { std::cout << to_string() << std::endl; }

std::string Unifiable::type_vars_to_string() const {
    std::string result;

    if (!is_polymorphic())
        return result;

    const char* separator = "[";
    for (auto v : type_vars()) {
        result += separator + v->to_string();
        auto& restr = v->bounds();

        if (!restr.empty()) {
            auto inner_sep = ":";
            for (auto t : restr) {
                result += inner_sep + t->to_string();
                inner_sep = "+";
            }
        }

        separator = ",";
    }
    return result + ']';
}

std::string UnknownTypeNode::to_string() const { 
    assert(!is_unified());
    return std::string("?") + std::to_string(id());
}

std::string PrimTypeNode::to_string() const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::string Unifiable::elems_to_string() const {
    std::string result;

    if (is_empty())
        return "()";

    const char* separator = "(";
    for (auto elem : elems()) {
        result += separator + elem->to_string();
        separator = ", ";
    }
    return result + ')';
}

std::string TypeVarNode::to_string() const {
    if (!name_.empty()) {
        return name_.str();
    } else {
        return std::string("_") + std::to_string(id()) + std::string("_");
    }
}

std::string TraitNode::to_string() const { return is_error() ? "<trait error>" : trait_decl()->symbol().str(); }

std::string BoundNode::to_string() const {
    if (is_error())
        return "<bound error>";

    std::string result = trait()->to_string();

    assert(!is_empty());
    if (num_elems() == 1)
        return result;

    assert(num_elems() == trait()->num_type_vars());
    const char* separator = "[";
    for (size_t i = 1; i < trait()->num_type_vars(); ++i) {
        result += separator + elem(i)->to_string();
        separator = ",";
    }

    return result + "]";
}

std::string OwnedPtrTypeNode::to_string() const {
    return "~" + referenced_type()->to_string();
}

std::string BorrowedPtrTypeNode::to_string() const {
    return "&" + referenced_type()->to_string();
}

std::string DefiniteArrayTypeNode::to_string() const {
    return '[' + elem_type()->to_string() + " * " + std::to_string(dim()) + ']';
}

std::string IndefiniteArrayTypeNode::to_string() const {
    return "[" + elem_type()->to_string() + "]";
}

//------------------------------------------------------------------------------

}
