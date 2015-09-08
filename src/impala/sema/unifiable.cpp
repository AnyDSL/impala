#include "impala/sema/unifiable.h"

#ifndef NDEBUG
#include <iostream>
#endif

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
        TraitAbs trait = impl->trait();
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
        if (auto fn = args().back().isa<FnType>()) {
            if (fn->num_args() == 1)
                return fn->args().front();
            return typetable().tuple_type(fn->args());
        }
    }
    return typetable().type_noret();
}

StructAbsTypeNode::StructAbsTypeNode(TypeTable& typetable, const StructDecl* struct_decl)
    : KnownTypeNode(typetable, Kind_struct_abs, Array<Type>(struct_decl->num_field_decls()))
    , struct_decl_(struct_decl)
{}

StructAppTypeNode::StructAppTypeNode(TypeTable& typetable, StructAbsType struct_abs_type, ArrayRef<Type> args)
    : KnownTypeNode(typetable, Kind_struct_app, args)
    , struct_abs_type_(struct_abs_type.unify())
    , elem_cache_(struct_abs_type->num_args())
{}

Type StructAppTypeNode::elem(size_t i) const {
    if (auto type = elem_cache_[i])
        return type;

    if (i < struct_abs_type()->num_args()) {
        auto type = struct_abs_type()->arg(i);
        auto map = specialize_map(struct_abs_type(), args());
        return elem_cache_[i] = type->specialize(map).unify();
    }
    return Type();
}

void TypeVarNode::add_bound(TraitApp bound) const {
    assert(!is_closed() && "closed type variables must not be changed");
    bounds_.push_back(bound);
}

bool TraitAbsNode::add_super_trait(TraitApp trait) const {
    auto p = super_traits_.insert(trait.unify());
    return p.second;
}

void TraitAbsNode::add_impl(Impl impl) const {
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

    for (auto arg : args()) {
        if (arg && !arg->is_known())
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

    for (auto arg : args()) {
        if (arg && !arg->is_closed())
            return false;
    }

    return true;
}

bool KnownTypeNode::is_sane() const {
    for (auto arg : args()) {
        if (!arg->is_sane())
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
    size_t seed = hash_combine(hash_combine(hash_begin((int) kind()), num_args()), num_type_vars());

    for (auto type_var : type_vars())
        seed = hash_combine(seed, type_var->num_bounds());

    for (auto arg : args())
        seed = hash_combine(seed, arg->hash());

    return seed;
}

size_t StructAbsTypeNode::hash() const { return hash_value(struct_decl()); }
size_t StructAppTypeNode::hash() const { return hash_combine(Unifiable::hash(), struct_abs_type()->hash()); }

size_t TraitAbsNode::hash() const { return hash_value(trait_decl()); }
size_t TraitAppNode::hash() const { return hash_combine(Unifiable::hash(), trait()->id()); }
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
        bool result = this->num_args() == other->num_args() && this->num_type_vars() == other->num_type_vars();

        if (result) {
            // set equivalence constraints for type variables
            for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
                assert(this->type_var(i)->equiv_ == nullptr);
                this->type_var(i)->equiv_ = *other->type_var(i);
            }

            // check equality of the restrictions of the type variables
            for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
                result &= this->type_var(i)->bounds_equal(*other->type_var(i));

            // check recursively argument types for equivalence
            for (size_t i = 0, e = this->num_args(); i != e && result; ++i)
                result &= this->arg(i)->equal(*other->arg(i));

            // unset equivalence constraints for type variables
            for (auto var : type_vars())
                var->equiv_ = nullptr;
        }

        return result;
    }

    return false;
}

bool TraitAppLT::operator () (TraitApp t1, TraitApp t2) const {
    assert(t1->is_unified() && t2->is_unified());
    if (t1->id() == t2->id()) return false;
    if (t1->trait()->id() < t2->trait()->id()) return true;
    if (t1->trait()->id() > t2->trait()->id()) return false;
    if (t1->num_args() < t2->num_args()) return true;
    if (t1->num_args() > t2->num_args()) return false;

    for (size_t i = 0, e = t1->num_args(); i != e; ++i) {
        assert(t1->arg(i)->is_unified() && t2->arg(i)->is_unified());
        if (t1->arg(i)->id() < t2->arg(i)->id()) return true;
        if (t1->arg(i)->id() > t2->arg(i)->id()) return false;
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

bool PtrTypeNode::equal(const Unifiable* other) const {
    assert(this->is_unified());
    return Unifiable::equal(other) && addr_space() == other->as<PtrTypeNode>()->addr_space();
}

bool DefiniteArrayTypeNode::equal(const Unifiable* other) const {
    assert(this->is_unified());
    return Unifiable::equal(other) && (this->dim() == other->as<DefiniteArrayTypeNode>()->dim());
}

bool StructAbsTypeNode::equal(const Unifiable* unifiable) const {
    assert(this->is_unified());
    if (auto other = unifiable->isa<StructAbsTypeNode>())
        return this->struct_decl() == other->struct_decl();
    return false;
}

bool StructAppTypeNode::equal(const Unifiable* other) const {
    assert(this->is_unified());
    return Unifiable::equal(other) && this->struct_abs_type()->equal(*other->as<StructAppTypeNode>()->struct_abs_type());
}

bool TraitAbsNode::equal(const Unifiable* other) const {
    assert(this->is_unified());

    if (auto trait = other->isa<TraitAbsNode>())
        return this->trait_decl() == trait->trait_decl();
    return false;
}

bool TraitAppNode::equal(const Unifiable* other) const {
    return Unifiable::equal(other) && this->trait()->equal(*other->as<TraitAppNode>()->trait());
}

bool SimdTypeNode::equal(const Unifiable* other) const {
    assert(this->is_unified());
    return Unifiable::equal(other) && (this->size() == other->as<SimdTypeNode>()->size());
}

//------------------------------------------------------------------------------

/*
 * is_subtype
 */

bool StructAppTypeNode::is_subtype(const TypeNode* other) const { return this == other; }
bool TypeVarNode::is_subtype(const TypeNode* other) const { return equal(other); }
bool PtrTypeNode::is_subtype(const TypeNode* other) const {
    return other->isa<PtrTypeNode>() &&
           addr_space() == other->as<PtrTypeNode>()->addr_space() &&
           referenced_type()->is_subtype(*other->as<PtrTypeNode>()->referenced_type());
}
bool BorrowedPtrTypeNode::is_subtype(const TypeNode* other) const {
    return PtrTypeNode::is_subtype(other) && !other->isa<OwnedPtrTypeNode>();
}
bool DefiniteArrayTypeNode::is_subtype(const TypeNode* other) const {
    bool dim_eq = true;
    if (auto da_other = other->isa<DefiniteArrayTypeNode>())
        dim_eq = dim() == da_other->dim();

    return dim_eq && other->isa<IndefiniteArrayTypeNode>() && elem_type()->is_subtype(*other->as<ArrayTypeNode>()->elem_type());
}

bool SimdTypeNode::is_subtype(const TypeNode* other) const {
    return this->equal(other); 
}

/*
 * TODO merge this code with equal
 */

bool is_subtype(Uni u1, Uni u2) {
    if (!u1->is_unified() || !u2->is_unified())
        return false;

    const Unifiable* up1 = *u1;
    const Unifiable* up2 = *u2;

    if (up1->isa<TypeNode>() && up2->isa<TypeNode>()) {
        return up1->as<TypeNode>()->is_subtype(up2->as<TypeNode>());
    } else {
        return false;
    }
}

bool TypeNode::is_subtype(const TypeNode* other) const {
    if (this == other)
        return true;

    if (this->kind() == other->kind()) {
        bool result = this->num_args() == other->num_args() && this->num_type_vars() == other->num_type_vars();

        if (result) {
            // set equality constraints for type variables
            for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
                assert(this->type_var(i)->equiv_ == nullptr);
                this->type_var(i)->equiv_ = *other->type_var(i);
            }

            // check equality of the restrictions of the type variables
            for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
                result &= this->type_var(i)->bounds_subtype(*other->type_var(i));

            // check recursively argument types for equivalence
            for (size_t i = 0, e = this->num_args(); i != e && result; ++i)
                result &= this->arg(i)->is_subtype(*other->arg(i));

            // unset equality constraints for type variables
            for (auto var : type_vars())
                var->equiv_ = nullptr;
        }

        return result;
    }

    return false;
}

bool TypeVarNode::bounds_subtype(const TypeVarNode* other) const {
    assert(this->is_unified());

    // removing bounds is okay
    if (this->num_bounds() > other->num_bounds())
        return false;

    // this->bounds() must be a subset of other->bounds()
    for (auto this_bound : this->bounds()) { // TODO this loop is quadratic
        for (auto other_bound : other->bounds()) {
            // FEATURE we could not only allow equal bounds, but use the sub-trait relation
            if (this_bound == other_bound)
                goto found;
        }
        return false;
found:;
    }
    return true;
}

//------------------------------------------------------------------------------

/*
 * specialize and instantiate
 */

SpecializeMap specialize_map(const Unifiable* unifiable, ArrayRef<Type> args) {
    assert(unifiable->num_type_vars() == args.size());
    SpecializeMap map;
    size_t i = 0;
    for (TypeVar v : unifiable->type_vars())
        map[*v] = *args[i++];
    assert(map.size() == args.size());
    return map;
}

Type instantiate_unknown(Type type, std::vector<Type>& args) {
    for (size_t i = 0, e = type->num_type_vars(); i != e; ++i)
        args.push_back(type->typetable().unknown_type());
    auto map = specialize_map(type, args);
    return type->instantiate(map);
}

Type TypeNode::instantiate(SpecializeMap& map) const {
#ifndef NDEBUG
    for (auto type_var : type_vars())
        assert(map.contains(*type_var));
#endif
    return vinstantiate(map);
}

std::unique_ptr<SpecializeMap> TypeNode::createSpecializationMap(ArrayRef<Type> args) const {
    assert(num_type_vars() == args.size());
    std::unique_ptr<SpecializeMap> map(new SpecializeMap());
    for (size_t i = 0, e = num_type_vars(); i != e; ++i)
        (*map)[*type_var(i)] = *args[i];
    return map;
}

Type TypeNode::instantiate(ArrayRef<Type> args) const {
    return instantiate(*createSpecializationMap(args));
}

Type StructAbsTypeNode::instantiate(ArrayRef<Type> args) const {
    return typetable().struct_app_type(this, args);
}

Type TypedefAbsNode::instantiate(ArrayRef<Type> args) const {
    return type()->specialize(*createSpecializationMap(args));
}

TraitApp TraitAbsNode::instantiate(ArrayRef<Type> args) const {
    return typetable().trait_app(this, args);
}

Type BorrowedPtrTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().borrowd_ptr_type(referenced_type()->specialize(map), addr_space());
}

Type DefiniteArrayTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().definite_array_type(elem_type()->specialize(map), dim());
}

Type SimdTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().simd_type(elem_type()->specialize(map), size());
}

Type FnTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().fn_type(specialize_args(map));
}

Type IndefiniteArrayTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().indefinite_array_type(elem_type()->specialize(map));
}

Type OwnedPtrTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().owned_ptr_type(referenced_type()->specialize(map), addr_space());
}

Type StructAppTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().struct_app_type(struct_abs_type(), specialize_args(map));
}

Type TupleTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().tuple_type(specialize_args(map));
}

Type UnknownTypeNode::vinstantiate(SpecializeMap& map) const {
    return map[this] = *typetable().unknown_type();
}

Type NoRetTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type PrimTypeNode ::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type TypeErrorNode::vinstantiate(SpecializeMap& map) const { return map[this] = this; }
Type TypeVarNode  ::vinstantiate(SpecializeMap& map) const { return map[this] = this; }

Array<Type> Unifiable::specialize_args(SpecializeMap& map) const {
    Array<Type> new_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        new_args[i] = arg(i)->specialize(map);
    return new_args;
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

TraitApp TraitAppNode::specialize(SpecializeMap& map) const {
    return typetable().trait_app(trait(), specialize_args(map));
}

Impl ImplNode::specialize(SpecializeMap& map) const {
    return typetable().impl(impl_item(), trait_app()->specialize(map), type());
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
                && u1->num_args() == u2->num_args()) {
            // TODO handle type vars
            for (size_t i = 0, e = u1->num_args(); i != e && result; ++i)
                result &= infer(u1->arg(i), u2->arg(i));

            if (auto b1 = u1->isa<TraitAppNode>())
                result &= b1->trait() == u2->as<TraitAppNode>()->trait();

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

bool KnownTypeNode::implements(TraitApp bound, SpecializeMap& map) const {
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
            for (size_t i = 0, e = impl->trait_app()->num_args(); i != e; ++i) { // TODO this is currently quadratic
                if (type_var.as<Type>() == impl->trait_app()->arg(i)
                        && !bound->arg(i)->isa<UnknownTypeNode>()
                        && implements_bounds(bound->arg(i), type_var))
                    map[*type_var] = *bound->arg(i);
            }
        }

        if (bound == impl->specialize(map)->trait_app())
            return true;
    }

    return false;
}

// TODO factor copy&paste code
Impl KnownTypeNode::find_impl(TraitApp bound) const {
    SpecializeMap map;
    for (auto impl : bound->trait()->type2impls(this)) {
        return impl;
        // find out which of impl's type_vars match to which of impl->bounds' type args
        for (auto type_var : impl->type_vars()) {
            for (size_t i = 0, e = impl->trait_app()->num_args(); i != e; ++i) { // TODO this is currently quadratic
                if (type_var.as<Type>() == impl->trait_app()->arg(i)
                        && !bound->arg(i)->isa<UnknownTypeNode>())
                    map[*type_var] = *bound->arg(i);
            }
        }

        if (bound == impl->specialize(map)->trait_app())
            return impl;
    }

    return Impl();
}

bool TypeVarNode::implements(TraitApp bound, SpecializeMap& map) const {
    if (!is_unified())
        return unify()->as<TypeVarNode>()->implements(bound, map);

    std::queue<TraitApp> queue;
    IdSet<TraitApp> done;

    auto enqueue = [&] (TraitApp bound) {
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

        for (auto super_trait : cur_bound->trait()->super_traits()) {
            map[*super_trait->arg(0)] = *cur_bound->arg(0); // propagate self type param
            auto spec_super_trait = super_trait->specialize(map).unify();
            if (!done.contains(spec_super_trait))
                enqueue(spec_super_trait);
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
        if (auto fn = impl->trait_app()->find_method(name))
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

FnType TraitAbsNode::find_method(Symbol name) const {
    if (trait_decl()) {
        auto i = trait_decl()->method_table().find(name);
        if (i != trait_decl()->method_table().end())
            return i->second->fn_type();

        for (auto super : super_traits()) {
            if (auto type = super->find_method(name))
                return type;
        }
    }

    return FnType();
}

FnType TraitAppNode::find_method(Symbol name) const {
    auto i = method_cache_.find(name);
    if (i != method_cache_.end())
        return i->second;

    if (auto type = trait()->find_method(name)) {
        auto map = specialize_map(trait(), args());
        return method_cache_[name] = type->specialize(map).as<FnType>().unify();
    }

    return FnType();
}

//------------------------------------------------------------------------------

}
