#include "impala/sema/type.h"

#include "thorin/util/queue.h"

#include "impala/ast.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

bool KnownTypeNode::infer(const Unifiable* other) const {
    if (auto ktn = other->isa<KnownTypeNode>()) {
        bool result = this->kind() == ktn->kind() && this->num_type_vars() == other->num_type_vars() 
            && size() == ktn->size();
        if (result) {
            // TODO handle type vars
            for (size_t i = 0, e = size(); i != e && result; ++i)
                result &= elem(i)->infer(ktn->elem(i));
            return result;
        }
    }
    return false;
}

bool UnknownTypeNode::infer(const Unifiable* other) const {
    if (!is_instantiated()) {
        instantiate(other->as<TypeNode>());
        typetable().unify(this);
        return true;
    } else
        return instance()->infer(other);
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
                        map[*type_var] = *bound->type_arg(i); // map this to bound's corresponding type_arg
                }
            }

            if (bound->infer(*impl->specialize(map)->bound()))
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
    return typetable().type_noret();
}

//------------------------------------------------------------------------------

Type TypeNode::specialize(SpecializeMap& map) const {
    if (auto result = thorin::find(map, this))
        return result;

    for (auto v : type_vars()) {
        assert(!map.contains(*v));
        v->specialize_bounds(map);
    }

    auto t = vspecialize(map);

    for (auto v : type_vars())
        t->bind(TypeVar(map[*v]->as<TypeVarNode>()));

    return t;
}

thorin::Array<Type> CompoundTypeNode::specialize_elems(SpecializeMap& map) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = elem(i)->specialize(map);
    return nelems;
}

const TypeNode* UnknownTypeNode::vspecialize(SpecializeMap& map) const { assert(false); return nullptr; }
const TypeNode* TypeErrorNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().type_error().node(); }
const TypeNode* NoRetTypeNode::vspecialize(SpecializeMap& map) const { return map[this] = typetable().type_noret().node(); }
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
