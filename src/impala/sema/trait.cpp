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
    return p.second;
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

Bound TraitNode::instantiate(thorin::ArrayRef<Type> args) const {
    return typetable().bound(Trait(this), args);
}

//------------------------------------------------------------------------------

BoundNode::BoundNode(const Trait trait, thorin::ArrayRef<Type> args)
    : Unifiable(trait->typetable())
    , trait_(trait)
    , args_(args)
{
    assert(trait_->num_type_vars() == num_args());
}

bool BoundNode::unify_with(const Unifiable* other) const {
    // TODO
#if 0
    if (auto tinst = other->isa<BoundNode>()) {
        if (trait() == tinst->trait()) {
            auto other_vinsts = tinst->var_instances_;
            if (var_instances_.size() == other_vinsts.size()) {
                bool result = true;
                for (auto v : trait()->type_vars()) {
                    TypeVarNode* vnode = v.node();
                    assert(var_instances_.find(vnode) != var_instances_.end());
                    assert(other_vinsts.find(vnode) != other_vinsts.end());

                    result = result && var_instances_[vnode]->unify_with(other_vinsts[vnode]);
                }
                return result;
            }
        }
    }
    return false;
#endif
    return false;
}

void BoundNode::refine() const {
#if 0
    // TODO review this code
    for (size_t i = 1, e = num_args(); i != e; ++i) {
        auto v = trait()->type_var(i);
        auto t = arg(i);

        if (auto utn = t->isa<UnknownTypeNode>()) {
            assert(utn->is_instantiated());
            utn->instance()->refine();
            //var_instances_[v.node()] = utn;
            args_[i] = Type(utn);
        } else
            t->refine();
    }
#endif
}

bool BoundNode::is_known() const {
    for (auto arg : args()) {
        if (!arg->is_known())
            return false;
    }
    return true;
}

bool BoundNode::is_closed() const {
    for (auto arg : args()) {
        if (!arg->is_closed())
            return false;
    }
    return true;
}

Type BoundNode::find_method(Symbol name) const {
    // TODO cache found methods
    if (auto type = trait()->find_method(name)) {
        SpecializeMap map;
        for (size_t i = 0, e = num_args(); i != e; ++i)
            map[*trait()->type_var(i)] = *arg(i);
        Type t = type->specialize(map);
        typetable().unify(t);
        return t;
    }

    return Type();
}

//------------------------------------------------------------------------------

}
