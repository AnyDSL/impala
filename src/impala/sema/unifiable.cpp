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

//------------------------------------------------------------------------------

bool TypeNode::is(PrimTypeKind kind) const { 
    return isa<PrimTypeNode>() && as<PrimTypeNode>()->primtype_kind() == kind; 
}

bool KnownTypeNode::is_sane() const {
    for (auto elem : elems_) {
        if (!elem->is_sane())
            return false;
    }
    assert(is_closed());
    return true;
}

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
            if (fn->size() == 1)
                return fn->elems().front();
            return typetable().tuple_type(fn->elems());
        }
    }
    return typetable().type_noret();
}

FnType FnTypeNode::peel_first() const {
    return typetable().fn_type(elems().slice_from_begin(1));
}

StructTypeNode::StructTypeNode(TypeTable& typetable, const StructDecl* struct_decl)
    : KnownTypeNode(typetable, Kind_tuple, struct_decl->fields().size())
    , struct_decl_(struct_decl)
{}

void TypeVarNode::add_bound(Bound bound) const {
    assert(!is_closed() && "closed type variables must not be changed!");
    bounds_.insert(bound);
}

bool TraitNode::add_super_bound(Bound bound) const {
    auto p = super_bounds_.insert(bound);
    bound->trait()->sub_traits_.insert(this);
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
    type2impls_[impl->type()].push_back(impl);
}

//------------------------------------------------------------------------------

/*
 * hash
 */

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

//------------------------------------------------------------------------------

/*
 * equal
 */

bool UnknownTypeNode::equal(const Unifiable* other) const {
    return is_instantiated() ? instance()->equal(other) : this == other;
}

bool KnownTypeNode::equal(const Unifiable* unifiable) const {
    if (this == unifiable) 
        return true;

    if (auto utn = unifiable->isa<const UnknownTypeNode>()) 
        return utn->equal(this);

    if (this->kind() == unifiable->kind()) {
        auto other = unifiable->as<KnownTypeNode>();
        bool result = this->size() == other->size() && this->num_type_vars() == other->num_type_vars();
        if (result) {
            // set equivalence constraints for type variables
            for (size_t i = 0, e = num_type_vars(); i != e; ++i) {
                assert(this->type_var(i)->equiv_ == nullptr);
                this->type_var(i)->equiv_ = *other->type_var(i);
            }

            // check equality of the restrictions of the type variables
            for (size_t i = 0, e = num_type_vars(); i != e && result; ++i)
                result &= this->type_var(i)->bounds_equal(other->type_var(i));

            // check recursively element types for equivalence
            for (size_t i = 0, e = size(); i != e && result; ++i)
                result &= this->elem(i) == other->elem(i);

            // unset equivalence constraints for type variables
            for (auto var : type_vars())
                var->equiv_ = nullptr;
        }

        return result;
    }

    return false;
}

bool TypeVarNode::bounds_equal(const TypeVar other) const {
    if (this->bounds().size() == other->bounds().size()) {
        // FEATURE this works but seems too much effort, at least use a set that uses representatives
        TypetableSet<const BoundNode> obounds;
        for (auto r : this->bounds()) {
            auto p = obounds.insert(*r); // TODO is deref here and below correct?
            assert(p.second && "hash/equal broken");
        }

        for (auto r : other->bounds()) {
            if (!obounds.contains(*r))
                return false;
        }
    }

    return true;
}

bool TypeVarNode::equal(const Unifiable* other) const {
    if (auto type_var = other->isa<TypeVarNode>())
        return this == other || (this->equiv_ != nullptr && this->equiv_ == type_var);
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
            assert(this->num_type_args() == bound->num_type_args());
            for (size_t i = 0, e = num_type_args(); i != e; ++i) {
                if (!(this->type_arg(i) == bound->type_arg(i)))
                    return false;
            }
            return true;
        }
    }
    return false;
}

//------------------------------------------------------------------------------

/*
 * is_known
 */

bool KnownTypeNode::is_known() const {
    for (auto v : type_vars()) {
        if (!v->is_known())
            return false;
    }

    for (auto elem : elems()) {
        if (!elem->is_known())
            return false;
    }
    return true;
}

bool BoundNode::is_known() const {
    for (auto type_arg : type_args()) {
        if (!type_arg->is_known())
            return false;
    }
    return true;
}

//------------------------------------------------------------------------------

/*
 * is_closed
 */

bool KnownTypeNode::is_closed() const {
    for (auto v : type_vars()) {
        for (auto r : v->bounds()) {
            if (!r->is_closed())
                return false;
        }
    }

    for (auto t : elems()) {
        if (!t->is_closed())
            return false;
    }
    return true;
}

bool TypeVarNode::is_closed() const { return bound_at_ != nullptr; }

bool BoundNode::is_closed() const {
    for (auto type_arg : type_args()) {
        if (!type_arg->is_closed())
            return false;
    }
    return true;
}

//------------------------------------------------------------------------------

/*
 * specialize and instantiate
 */

SpecializeMap specialize_map(const Unifiable* unifiable, thorin::ArrayRef<Type> type_args) {
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

thorin::Array<Type> KnownTypeNode::specialize_elems(SpecializeMap& map) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
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

Type UnknownTypeNode::vinstantiate(SpecializeMap& map) const { assert(false); return nullptr; }
Type TypeErrorNode::vinstantiate(SpecializeMap& map) const { return map[this] = *typetable().type_error(); }
Type NoRetTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = *typetable().type_noret(); }
Type PrimTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = *typetable().type(primtype_kind()); }
Type FnTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = *typetable().fn_type(specialize_elems(map)); }
Type TupleTypeNode::vinstantiate(SpecializeMap& map) const { return map[this] = *typetable().tuple_type(specialize_elems(map)); }
Type StructTypeNode::vinstantiate(SpecializeMap& map) const { assert(false); return nullptr; }
Type TypeVarNode::vinstantiate(SpecializeMap& map) const { return map[this] = this; }

Bound TraitNode::instantiate(thorin::ArrayRef<Type> type_args) const {
    return typetable().bound(this, type_args);
}

Bound BoundNode::specialize(SpecializeMap& map) const {
    thorin::Array<Type> new_type_args(num_type_args());
    for (size_t i = 0, e = num_type_args(); i != e; ++i)
        new_type_args[i] = type_arg(i)->specialize(map);

    return typetable().bound(trait(), new_type_args);
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

    if (u2->unify()->is_unified()) {                                // if u2 is unified we try to infer u1
        if (u1->isa<UnknownTypeNode>()) {
            assert(u2->representative()->isa<KnownTypeNode>());
            u1->representative_ = u2->representative();             // set u1 to u2
            return true;
        } else if (u1->unify()->is_unified())
            return u1->representative() == u2->representative();    // both are unified - are types equal?
        else if (u1->kind() == u2->kind()) {                        // recursively infer sub elements
            if (auto ktn1 = u1->isa<KnownTypeNode>()) {
                auto ktn2 = u2->as<KnownTypeNode>();
                bool result = ktn1->num_type_vars() == ktn2->num_type_vars() && ktn1->size() == ktn2->size();
                // TODO handle type vars
                for (size_t i = 0, e = ktn1->size(); i != e && result; ++i)
                    result &= infer(ktn1->elem(i), ktn2->elem(i));
                return result;
            } else if (auto b1 = u1->isa<BoundNode>()) {
                auto b2 = u2->as<BoundNode>();
                bool result = b1->trait() == b2->trait() && b1->num_type_args() == b2->num_type_args();
                for (size_t i = 0, e = b1->num_type_args(); result && i != e; ++i)
                    result &= infer(b1->type_arg(i), b2->type_arg(i));
                return result;
            } else
                assert(false);
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
    auto implements_bounds = [&] (Type type, TypeVar type_var) {
        for (auto b : type_var->bounds()) {
            if (!type->implements(b, map))
                return false;
        }
        return true;
    };

    std::queue<Bound> queue;
    UniSet<Bound> done;

    auto enqueue = [&] (Bound bound) { 
        queue.push(bound); 
        done.insert(bound); 
    };

    enqueue(bound);

    while (!queue.empty()) {
        auto bound = thorin::pop(queue);
        for (auto impl : bound->trait()->type2impls(this)) {
            // find out which of impl's type_vars match to which of impl->bounds' type_args
            for (auto type_var : impl->type_vars()) {
                for (size_t i = 0, e = impl->bound()->num_type_args(); i != e; ++i) { // TODO this is currently quadratic
                    if (type_var.as<Type>() == impl->bound()->type_arg(i) 
                            && !bound->type_arg(i)->isa<UnknownTypeNode>()
                            && implements_bounds(bound->type_arg(i), type_var))
                        map[*type_var] = *bound->type_arg(i);
                }
            }

            if (bound == impl->specialize(map)->bound())
                return true;
        }

        // may be one of bound->trait's subtraits implements 'this'
        for (auto sub_trait : bound->trait()->sub_traits()) {
            auto super_bound = sub_trait->super_bound(bound->trait());
            thorin::Array<Type> new_type_args(sub_trait->num_type_vars());
            for (size_t i = 0, e = sub_trait->num_type_vars(); i != e; ++i) {
                for (size_t j = 0, e = super_bound->num_type_args(); j != e; ++j) { // TODO this is currently quadratic
                    if (sub_trait->type_var(i).as<Type>() == super_bound->type_arg(j)
                            && implements_bounds(bound->type_arg(j), sub_trait->type_var(i)))
                        new_type_args[i] = bound->type_arg(j);
                }

                if (new_type_args[i].empty())
                    new_type_args[i] = typetable().unknown_type();
            }

            auto sub_bound = sub_trait->instantiate(new_type_args);
            if (!done.contains(sub_bound)) {
                assert(sub_bound->is_closed());
                enqueue(sub_bound);
            }
        }
    }

    return false;
}

bool TypeVarNode::implements(Bound bound, SpecializeMap& map) const {
    std::queue<Bound> queue;
    UniSet<Bound> done;

    for (auto b : bounds()) {
        queue.push(b);
        done.insert(b);
    }

    while (!queue.empty()) {
        auto cur_bound = queue.front();
        queue.pop();

        if (cur_bound == bound)
            return true;

        for (auto super_bound : cur_bound->trait()->super_bounds()) {
            map[*super_bound->type_arg(0)] = *cur_bound->type_arg(0); // propagate self type param
            auto spec_super_bound = super_bound->specialize(map);
            spec_super_bound->unify();
            if (!done.contains(spec_super_bound)) {
                queue.push(spec_super_bound);
                done.insert(spec_super_bound);
            }
        }
    }

    return false;
}

//------------------------------------------------------------------------------

/*
 * find_method
 */

Type KnownTypeNode::find_method(Symbol name) const {
    for (auto impl : impls_) {
        if (auto fn = impl->bound()->find_method(name))
            return fn;
    }
    return Type();
}

Type TypeVarNode::find_method(Symbol name) const {
    for (auto bound : bounds()) {
        if (auto fn = bound->find_method(name)) 
            return fn;
    }
    return Type();
}

Type TraitNode::find_method(Symbol name) const {
    auto i = trait_decl()->method_table().find(name);
    if (i != trait_decl()->method_table().end())
        return i->second->type();

    for (auto super : super_bounds()) {
        if (auto type = super->find_method(name))
            return type;
    }

    return Type();
}

Type BoundNode::find_method(Symbol name) const {
    auto i = method_cache_.find(name);
    if (i != method_cache_.end())
        return i->second;

    if (auto type = trait()->find_method(name)) {
        auto map = specialize_map(trait(), type_args());
        return method_cache_[name] = type->specialize(map);
    }

    return Type();
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

    if (!is_generic())
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
    return is_instantiated() ? instance()->to_string() : (std::string("?") + std::to_string(id())); 
}

std::string PrimTypeNode::to_string() const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::string KnownTypeNode::elems_to_string() const {
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
    if (name_) {
        return name_.str();
    } else {
        return std::string("_") + std::to_string(id()) + std::string("_");
    }
}

std::string TraitNode::to_string() const { return is_error_trait() ? "<trait error>" : trait_decl()->symbol().str(); }

std::string BoundNode::to_string() const {
    std::string result = trait()->to_string();

    assert(!type_args_.empty());
    if (type_args_.size() == 1)
        return result;

    assert(type_args_.size() == trait()->num_type_vars());
    const char* separator = "[";
    for (size_t i = 1; i < trait()->num_type_vars(); ++i) {
        result += separator + type_arg(i)->to_string();
        separator = ",";
    }

    return result + "]";
}

//------------------------------------------------------------------------------

}
