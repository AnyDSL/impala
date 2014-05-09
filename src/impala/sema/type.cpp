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

// TODO test this
bool KnownTypeNode::is_subtype(const Type super_type) const {
    if (this == *super_type)
        return true;

    for (auto t : super_type->elems()) {
        if (this->is_subtype(t))
            return true;
    }
    return false;
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

void KnownTypeNode::add_implementation(Impl impl) const {
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
    std::queue<Bound> queue;
    UniSet<Bound> done;

    for (auto impl : impls()) {
        queue.push(impl->bound());
        done.insert(impl->bound());
    }

    while (!queue.empty()) {
        auto impl_bound = queue.front();
        queue.pop();

        if (impl_bound == bound)
            return true;

        for (auto super_bound : impl_bound->trait()->super_bounds()) {
            // propagate self type param
            map[*super_bound->arg(0)] = *impl_bound->arg(0);
            auto spec_super_bound = super_bound->specialize(map);
            spec_super_bound->unify();
            if (!done.contains(spec_super_bound)) {
                queue.push(spec_super_bound);
                done.insert(spec_super_bound);
            }
        }
    }

    return false;
#if 0
    // try to instantiate the generic implementations
    for (auto ti : gen_trait_impls_) {
        std::vector<Type> inst_types;
        Impl inst = typetable().instantiate_unknown(ti, inst_types);
        if (inst->trait()->unify_with(*trait)) { // TODO why do we have to deref here explicitly? It *should* work without deref
            if (typetable().check_bounds(nullptr, ti, inst_types))
                return true;
        }
    }
#endif
}

bool TypeVarNode::implements(Bound bound, SpecializeMap&) const {
    // CHECK is this enough?
    return bounds().contains(bound);
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
    // FEATURE this could be faster if we copy only types where something changed inside
    if (auto result = thorin::find(map, this))
        return Type(result);

    for (auto v : type_vars()) {
        // CHECK is representative really correct or do we need node()? -- see also below!
        assert(!map.contains(v.representative()));
        v->clone(map); // CHECK is node() correct here?
    }

    auto t = vspecialize(map);

    for (auto v : type_vars()) {
        assert(map.contains(v.representative()));
        t->bind(TypeVar(map[v.representative()]->as<TypeVarNode>()));
    }

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
const TypeNode* TypeVarNode::vspecialize(SpecializeMap& map) const {
    // was not bound in the specialized type -> return orginal type var
    // CHECK do we need to create a new copy here? unification lead to segmentation faults in the past...
    return map[this] = this;
}

TypeVar TypeVarNode::clone(SpecializeMap& map) const {
    TypeVar v = typetable().type_var();
    map[this] = v.node();

    // copy bounds
    for (auto b : bounds())
        //v->add_bound(b->specialize(map));
        v->add_bound(b);

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
