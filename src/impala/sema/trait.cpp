#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"
#include "impala/ast.h"

namespace impala {

//------------------------------------------------------------------------------

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : Unifiable(tt)
    , trait_decl_(trait_decl)
{}

bool TraitNode::add_super_bound(Bound bound) const {
    typetable().unify(bound);
    auto p = super_bounds_.insert(bound);
    bound->trait()->sub_traits_.insert(this);
    return p.second;
}

Bound TraitNode::super_bound(Trait trait) const {
    for (auto super : super_bounds())
        if (super->trait() == trait)
            return super;
    return Bound();
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

Bound TraitNode::instantiate(thorin::ArrayRef<Type> type_args) const {
    return typetable().bound(Trait(this), type_args);
}

void TraitNode::add_impl(Impl impl) const {
    type2impls_[impl->type()].push_back(impl);
}

//------------------------------------------------------------------------------

BoundNode::BoundNode(const Trait trait, thorin::ArrayRef<Type> type_args)
    : Unifiable(trait->typetable())
    , trait_(trait)
    , type_args_(type_args)
{
    trait->instances_.push_back(Bound(this));
    assert(trait_->num_type_vars() == num_type_args());
}

bool BoundNode::infer(const Unifiable* unifiable) const {
    if (auto other = unifiable->isa<BoundNode>()) {
        bool result = this->trait() == other->trait() && this->num_type_args() == other->num_type_args();
        for (size_t i = 0, e = num_type_args(); result && i != e; ++i)
            result &= this->type_arg(i)->infer(other->type_arg(i));
        return result;
    }
    return false;
}

bool BoundNode::is_known() const {
    for (auto type_arg : type_args()) {
        if (!type_arg->is_known())
            return false;
    }
    return true;
}

bool BoundNode::is_closed() const {
    for (auto type_arg : type_args()) {
        if (!type_arg->is_closed())
            return false;
    }
    return true;
}

Type BoundNode::find_method(Symbol name) const {
    // TODO cache found methods
    if (auto type = trait()->find_method(name)) {
        SpecializeMap map;
        for (size_t i = 0, e = num_type_args(); i != e; ++i)
            map[*trait()->type_var(i)] = *type_arg(i);
        Type t = type->specialize(map);
        typetable().unify(t);
        return t;
    }

    return Type();
}

Bound BoundNode::specialize(SpecializeMap& map) const {
    thorin::Array<Type> new_type_args(num_type_args());
    for (size_t i = 0, e = num_type_args(); i != e; ++i)
        new_type_args[i] = type_arg(i)->specialize(map);

    return typetable().bound(trait(), new_type_args);
}

//------------------------------------------------------------------------------

Impl ImplNode::specialize(SpecializeMap& map) const { 
    return typetable().impl(impl_item(), bound()->specialize(map), type());
}

//------------------------------------------------------------------------------

}
