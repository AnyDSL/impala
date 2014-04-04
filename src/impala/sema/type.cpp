#include "impala/ast.h"

#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

int TypeVarNode::counter_ = 0;
int UnknownTypeNode::counter_ = 0;

//------------------------------------------------------------------------------

bool KnownTypeNode::unify_with(Unifiable* other) {
    if (kind() == other->kind()) {
        KnownTypeNode* ktn = other->as<KnownTypeNode>();

        if (unify_bound_vars(other->bound_vars())) {
            // go through all sub elements
            if (size() == ktn->size()) {
                bool result = true;
                for (size_t i = 0; i < size(); ++i)
                    result = result && elem(i)->unify_with(ktn->elem(i));

                return result;
            }
        }
    }
    return false;
}

bool UnknownTypeNode::unify_with(Unifiable* other) {
    if (!is_instantiated()) {
        instantiate(Type(other));
        typetable().unify(Type(this));
        return true;
    } else
        return instance()->unify_with(other);
}

//------------------------------------------------------------------------------

bool KnownTypeNode::is_closed() const {
    for (auto v : bound_vars()) {
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

void TypeVarNode::bind(const Unifiable* const e) {
    assert(bound_at_ == nullptr && "type variables can only be bound once!");
    bound_at_ = e;
}

void TypeVarNode::add_bound(Trait bound) {
    assert(!is_closed() && "Closed type variables must not be changed!");
    bounds_.insert(bound);
    bounds_.insert(bound->super_traits().begin(), bound->super_traits().end());
}

bool TypeVarNode::is_closed() const {
    return bound_at_ != nullptr;
}

//------------------------------------------------------------------------------

void KnownTypeNode::add_implementation(TraitImpl impl) {
    // TODO fail if a method was implemented multiple times!
    if (impl->is_generic()) {
        gen_trait_impls_.push_back(impl);
    } else {
        Trait trait = impl->trait();
        if (!trait_impls_.insert(trait).second)
            typetable().error(impl->impl_decl()) << "duplicated implementation of trait '" << trait << "'\n";
        for (auto super : trait->super_traits()) {
            if (!trait_impls_.insert(super).second)
                typetable().error(impl->impl_decl()) << "duplicated implementation of trait '" << super << "'\n";
        }
    }
}

bool KnownTypeNode::implements(Trait trait) const {
    if (trait_impls_.find(trait) == trait_impls_.end()) {
        // try to instantiate the generic implementations
        for (auto ti : gen_trait_impls_) {
            std::vector<Type> inst_types;
            TraitImpl inst = typetable().instantiate_unknown(ti, inst_types);
            if (inst->trait()->unify_with(*trait)) { // TODO why do we have to deref here explicitly? It *should* work without deref
                if (typetable().check_bounds(nullptr, ti, inst_types))
                    return true;
            }
        }
        return false;
    }
    return true;
}

bool TypeVarNode::implements(Trait trait) const {
    // CHECK is this enough?
    return bounds().find(trait) != bounds().end();
}

Type KnownTypeNode::find_method(Symbol s) const {
    // TODO what about generic implementations?
    for (auto t : trait_impls_) {
        if (auto fn = t->find_method(s)) 
            return fn;
    }
    return Type();
}

Type TypeVarNode::find_method(Symbol s) const {
    for (auto t : bounds()) {
        if (auto fn = t->find_method(s)) 
            return fn;
    }
    return Type();
}

FnType FnTypeNode::specialize_method(Type t) const {
    assert(elem(0) == t);
    FnType f = typetable().fntype(elems().slice_from_begin(1));
    typetable().unify(f.as<Type>());
    return f;
}

Type FnTypeNode::return_type() const {
    if (!is_empty()) {
        if (auto fn = elems().back().isa<FnType>()) {
            if (fn->size() == 1)
                return fn->elems().front();
            return typetable().tupletype(fn->elems());
        }
    }
    return typetable().type_noreturn();
}

//------------------------------------------------------------------------------

thorin::Array<Type> CompoundTypeNode::specialize_elems(SpecializeMap& map) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = elem(i)->specialize(map);
    return nelems;
}

Unifiable* UnknownTypeNode::vspecialize(SpecializeMap& map) { assert(false); return nullptr; }
Unifiable* TypeErrorNode::vspecialize(SpecializeMap& map) { return map[this] = typetable().type_error().node(); }
Unifiable* NoReturnTypeNode::vspecialize(SpecializeMap& map) { return map[this] = typetable().type_noreturn().node(); }
Unifiable* PrimTypeNode::vspecialize(SpecializeMap& map) { return map[this] = typetable().primtype(primtype_kind()).node(); }
Unifiable* FnTypeNode::vspecialize(SpecializeMap& map) { return map[this] = typetable().fntype(specialize_elems(map)).node(); }
Unifiable* TupleTypeNode::vspecialize(SpecializeMap& map) { return map[this] = typetable().tupletype(specialize_elems(map)).node(); }

TypeVarNode* TypeVarNode::vspecialize(SpecializeMap& map) {
    // was not bound in the specialized type -> return orginal type var
    // CHECK do we need to create a new copy here? unification lead to segmentation faults in the past...
    return map[this] = this;
}

TypeVar TypeVarNode::clone(SpecializeMap& map) const {
    TypeVar v = typetable().typevar();
    map[this] = v.node();

    // copy bounds!
    for (auto b : bounds())
        v->add_bound(b->specialize(map));

    return v;
}

void TypeVarNode::refresh_bounds() {
    std::vector<Trait> tmp;
    for (auto i : bounds())
        tmp.push_back(i);
    bounds_.clear();
    for (auto i : tmp) {
        auto p = bounds_.insert(i);
        assert(p.second && "hash/equal broken");
    }
}

//------------------------------------------------------------------------------

void KnownTypeNode::refine() {
    refine_bound_vars();
    for (size_t i = 0; i < size(); ++i) {
        Type e = elem(i);
        if (UnknownTypeNode* utn = e.node()->isa<UnknownTypeNode>()) {
            assert(utn->is_instantiated());
            utn->instance()->refine();
            set(i, utn->instance());
        } else {
            e->refine();
        }
    }

}

bool KnownTypeNode::is_known() const {
    bool result = bound_vars_known();
    for (auto e : elems())
        result = result && e->is_known();
    return result;
}

//------------------------------------------------------------------------------

}
