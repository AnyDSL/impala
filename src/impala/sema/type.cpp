#include "impala/sema/type.h"

#include <queue>
#include <iostream>

#include "impala/ast.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

bool KnownTypeNode::unify_with(const Unifiable* other) const {
    if (auto ktn = other->isa<KnownTypeNode>()) {
        if (this->kind() == ktn->kind()) { // TODO make this kind handling better
            if (unify_type_vars(other->type_vars())) {
                // go through all sub elements
                if (size() == ktn->size()) {
                    bool result = true;
                    for (size_t i = 0, e = size(); i != e && result; ++i)
                        result &= elem(i)->unify_with(ktn->elem(i));
                    return result;
                }
            }
        }
    }
    return false;
}

bool UnknownTypeNode::unify_with(const Unifiable* other) const {
    if (!is_instantiated()) {
        instantiate(Type(other->as<TypeNode>()));
        typetable().unify(Type(this));
        return true;
    } else
        return instance()->unify_with(other);
}

//------------------------------------------------------------------------------

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

bool KnownTypeNode::is_sane() const {
    for (auto t : elems_) {
        if (!t->is_sane()) {
            return false;
        }
    }
    assert(is_closed());
    return true;
}

//------------------------------------------------------------------------------

void TypeVarNode::add_bound(Bound bound) const {
    assert(!is_closed() && "closed type variables must not be changed!");
    bounds_.insert(bound);
}

bool TypeVarNode::is_closed() const {
    return bound_at_ != nullptr;
}

//------------------------------------------------------------------------------

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

static bool search_up(std::queue<Bound>& queue, UniSet<Bound>& done, Bound bound, SpecializeMap& map) {
    while (!queue.empty()) {
        auto impl_bound = queue.front();
        queue.pop();

        if (impl_bound == bound)
            return true;

        for (auto super_bound : impl_bound->trait()->super_bounds()) {
            // propagate self type param
            map[*super_bound->type_arg(0)] = *impl_bound->type_arg(0);
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

bool KnownTypeNode::implements(Bound bound, SpecializeMap& map) const {
    std::queue<Trait> queue;
    UniSet<Trait> done;

    for (auto impl : bound->trait()->type2impls().find(Type(this))->second) {
        auto tmp_map = map;
        // find out which of impl's type_vars match to which of impl->bounds' type_args
        for (size_t i = 0, e = impl->num_type_vars(); i != e; ++i) {
            for (size_t j = 0, e = impl->bound()->num_type_args(); j != e; ++j) {
                if (impl->type_var(i).as<Type>() == impl->bound()->type_arg(j))
                    tmp_map[*impl->type_var(i)] = *bound->type_arg(j); // map this to bound's corresponding type_arg
            }
        }

        if (impl->specialize(tmp_map)->bound() == bound)
            return true;
    }

    // may be one of bound->trait's subtraits implements 'this'
    for (auto sub_trait : bound->trait()->sub_traits()) {
        // TODO
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
    return search_up(queue, done, bound, map);
}

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

FnType FnTypeNode::specialize_method(Type t) const {
    assert(elem(0) == t);
    FnType f = typetable().fn_type(elems().slice_from_begin(1));
    typetable().unify(f.as<Type>());
    return f;
}

Type FnTypeNode::return_type() const {
    if (!is_empty()) {
        if (auto fn = elems().back().isa<FnType>()) {
            if (fn->size() == 1)
                return fn->elems().front();
            return typetable().tuple_type(fn->elems());
        }
    }
    return typetable().type_noreturn();
}

//------------------------------------------------------------------------------

Type TypeNode::specialize(SpecializeMap& map) const {
    if (auto result = thorin::find(map, this))
        return Type(result);

    for (auto v : type_vars()) {
        assert(!map.contains(*v));
        v->specialize_bounds(map);
    }

    auto t = vspecialize(map);

    for (auto v : type_vars())
        t->bind(TypeVar(map[*v]->as<TypeVarNode>()));

    return Type(t);
}

thorin::Array<Type> CompoundTypeNode::specialize_elems(SpecializeMap& map) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = elem(i)->specialize(map);
    return nelems;
}

const TypeNode* UnknownTypeNode::vspecialize(SpecializeMap& map) const { assert(false); return nullptr; }
const TypeNode* TypeErrorNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().type_error().node(); }
const TypeNode* NoReturnTypeNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().type_noreturn().node(); }
const TypeNode* PrimTypeNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().type(primtype_kind()).node(); }
const TypeNode* FnTypeNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().fn_type(specialize_elems(map)).node(); }
const TypeNode* TupleTypeNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().tuple_type(specialize_elems(map)).node(); }
const TypeNode* StructTypeNode::vspecialize(SpecializeMap& map) const { assert(false); return nullptr; }
const TypeNode* TypeVarNode::vspecialize(SpecializeMap& map) const { return map[this] = this; }

TypeVar TypeVarNode::specialize_bounds(SpecializeMap& map) const {
    TypeVar v = typetable().type_var();
    map[this] = v.node();
    for (auto b : bounds())
        v->add_bound(b->specialize(map));

    return v;
}

//------------------------------------------------------------------------------

void KnownTypeNode::refine() const {
    refine_type_vars();
    for (size_t i = 0; i < size(); ++i) {
        Type e = elem(i);
        if (auto utn = e.node()->isa<UnknownTypeNode>()) {
            assert(utn->is_instantiated());
            utn->instance()->refine();
            const_cast<KnownTypeNode*>(this)->set(i, utn->instance());
        } else {
            e->refine();
        }
    }

}

bool KnownTypeNode::is_known() const {
    bool result = type_vars_known();
    for (auto e : elems())
        result = result && e->is_known();
    return result;
}

//------------------------------------------------------------------------------

StructTypeNode::StructTypeNode(TypeTable& typetable, const StructDecl* struct_decl)
    : KnownTypeNode(typetable, Type_tuple, struct_decl->fields().size())
    , struct_decl_(struct_decl)
{}

//------------------------------------------------------------------------------

}
