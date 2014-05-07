#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"
#include "impala/ast.h"

namespace impala {

//------------------------------------------------------------------------------

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : TUnifiable(tt)
    , trait_decl_(trait_decl)
{}

bool TraitNode::add_super_trait(Trait t) const {
    typetable().unify(t);
    auto p = super_traits_.insert(t);
    return p.second;
}

Type TraitNode::find_method(Symbol name) const {
    auto i = trait_decl()->method_table().find(name);
    if (i != trait_decl()->method_table().end())
        return i->second->type();

    for (auto super : super_traits()) {
        if (auto type = super->find_method(name))
            return type;
    }

    return Type();
}

//------------------------------------------------------------------------------

TraitInstanceNode::TraitInstanceNode(const Trait trait, thorin::ArrayRef<Type> args)
    : TUnifiable(trait->typetable())
    , trait_(trait)
    , args_(args)
{
    assert(trait_->num_type_vars() == num_args());
}

bool TraitInstanceNode::unify_with(const Unifiable* other) const {
    // TODO
#if 0
    if (auto tinst = other->isa<TraitInstanceNode>()) {
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

void TraitInstanceNode::refine() const {
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

bool TraitInstanceNode::is_known() const {
    for (auto arg : args()) {
        if (!arg->is_known())
            return false;
    }
    return true;
}

bool TraitInstanceNode::is_closed() const {
    for (auto arg : args()) {
        if (!arg->is_closed())
            return false;
    }
    return true;
}

Type TraitInstanceNode::find_method(Symbol name) {
    // TODO cache found methods
    if (auto type = trait()->find_method(name)) {
#if 0
        auto m = var_instances();
        Type t = type->specialize(m);
        typetable().unify(t);
        return t;
#endif
    }

    return Type();
}

//------------------------------------------------------------------------------

}
