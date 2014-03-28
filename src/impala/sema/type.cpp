#include "impala/ast.h"

#include "impala/sema/type.h"
#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"

namespace impala {

int TypeVarNode::counter_ = 0;
int UninstantiatedTypeNode::counter_ = 0;

//------------------------------------------------------------------------------

bool RealTypeNode::unify_with(TypeNode* other) {
    if (kind() == other->kind()) {
        RealTypeNode* rtn = other->as<RealTypeNode>();

        if (unify_bound_vars(other->bound_vars())) {
            // go through all sub elements
            if (size() == rtn->size()) {
                bool result = true;
                for (size_t i = 0; i < size(); ++i)
                    result = result && elem(i)->unify_with(rtn->elem(i));

                return result;
            }
        }
    }
    return false;
}

bool UninstantiatedTypeNode::unify_with(TypeNode* other) {
    if (!is_instantiated()) {
        instantiate(Type(other));
        return true;
    } else
        return instance()->unify_with(other);
}

//------------------------------------------------------------------------------

bool RealTypeNode::is_closed() const {
    for (auto v : bound_vars()) {
        for (auto r : v->bounds()) {
            if (! r->is_closed())
                return false;
        }
    }

    for (auto t : elems()) {
        if (! t->is_closed())
            return false;
    }
    return true;
}

// TODO test this
bool RealTypeNode::is_subtype(const Type super_type) const {
    if (this == *super_type)
        return true;

    for (Type t : super_type->elems()) {
        if (this->is_subtype(t))
            return true;
    }
    return false;
}

bool RealTypeNode::is_sane() const {
    for (auto t : elems_) {
        if (!t->is_sane()) {
            return false;
        }
    }
    assert(is_closed());
    return true;
}

//------------------------------------------------------------------------------

void TypeVarNode::bind(const Generic* const e) {
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

void RealTypeNode::add_implementation(TraitImpl impl) {
    // TODO fail if a method was implemented multiple times!
    if (impl->is_generic()) {
        gen_trait_impls_.push_back(impl);
    } else {
        Trait trait = impl->trait();
        if (!trait_impls_.insert(trait).second)
            typetable().error(impl->impl_decl()) << "Duplicated implementation of trait '" << trait << "'\n";
        for (Trait super : trait->super_traits()) {
            if (!trait_impls_.insert(super).second)
                typetable().error(impl->impl_decl()) << "Duplicated implementation of trait '" << super << "'\n";
        }
    }
}

bool RealTypeNode::implements(Trait trait) const {
    if (trait_impls_.find(trait) == trait_impls_.end()) {
        // try to instantiate the generic implementations
        for (TraitImpl ti : gen_trait_impls_) {
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

Type RealTypeNode::find_method(Symbol s) const {
    // TODO what about generic implementations?
    for (Trait t : trait_impls_) {
        if (auto fn = t->find_method(s)) return fn;
    }
    return Type();
}

Type TypeVarNode::find_method(Symbol s) const {
    for (Trait t : bounds()) {
        if (auto fn = t->find_method(s)) return fn;
    }
    return Type();
}

FnType FnTypeNode::specialize_method(Type t) const {
    assert(is_final_representative());
    assert(elem(0) == t);

    std::vector<Type> new_args(++elems_.begin(), elems_.end());
    FnType f = typetable().fntype(new_args);
    typetable().unify(f.as<Type>());
    return f;
}

//------------------------------------------------------------------------------

Generic* UninstantiatedTypeNode::vspecialize(SpecializeMapping& mapping) { assert(false); return nullptr; }

thorin::Array<Type> CompoundTypeNode::specialize_elems(SpecializeMapping& mapping) const {
    thorin::Array<Type> nelems(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        nelems[i] = Type(elem(i)->specialize(mapping)->as<TypeNode>());
    return nelems;
}

Generic* TypeErrorNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().type_error().node(); }
Generic* NoReturnTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().type_noreturn().node(); }
Generic* PrimTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().primtype(primtype_kind()).node(); }
Generic* FnTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().fntype(specialize_elems(mapping)).node(); }
Generic* TupleTypeNode::vspecialize(SpecializeMapping& mapping) { return mapping[this] = typetable().tupletype(specialize_elems(mapping)).node(); }

Generic* TypeVarNode::vspecialize(SpecializeMapping& mapping) {
    // was not bound in the specialized type -> return orginal type var
    // CHECK do we need to create a new copy here? unification lead to segmentation faults in the past...
    return mapping[this] = this;
}

TypeVar TypeVarNode::clone(SpecializeMapping& mapping) const {
    TypeVar v = typetable().typevar();
    mapping[this] = v.node();

    // copy bounds!
    for (Trait b : bounds())
        v->add_bound(b->specialize(mapping));

    return v;
}

void TypeVarNode::refresh_bounds() {
    std::vector<Trait> tmp;
    for (Trait i : bounds())
        tmp.push_back(i);
    bounds_.clear();
    for (Trait i : tmp) {
        auto p = bounds_.insert(i);
        assert(p.second && "hash/equal broken");
    }
}

//------------------------------------------------------------------------------

void RealTypeNode::make_real() {
    make_bound_vars_real();
    for (size_t i = 0; i < size(); ++i) {
        Type e = elem(i);
        if (UninstantiatedType utn = e.isa<UninstantiatedType>()) {
            assert(utn->is_instantiated());
            utn->instance()->make_real();
            set(i, utn->instance());
        } else {
            e->make_real();
        }
    }

}

bool RealTypeNode::is_real() const {
    bool result = bound_vars_real();
    for (auto e : elems())
        result = result && e->is_real();
    return result;
}

//------------------------------------------------------------------------------

void verify(thorin::ArrayRef<const Type> types) {
    for (auto t : types)
        assert(t->is_sane());

    for (Type ty1 : types) {
        TypeNode* t1 = ty1.node();
        for (Type ty2 : types) {
            TypeNode* t2 = ty2.node();
            if (t1->is_unified() && t2->is_unified()) {
                if (!((!t1->equal(t2)) || (t1->representative() == t2->representative()))) {
                    t1->dump();
                    t2->dump();
                    assert(false);
                }
            }
        }
    }
}

}
