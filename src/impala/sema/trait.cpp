#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"
#include "impala/ast.h"

namespace impala {

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : TUnifiable(tt)
    , trait_decl_(trait_decl)
{}

TraitInstanceNode::TraitInstanceNode(const Trait trait, const SpecializeMap& var_instances)
    : TraitNode(trait->typetable(), trait->trait_decl())
    , trait_(trait)
    , var_instances_(var_instances)
{
    assert(trait_->num_type_vars() == var_instances_.size());
}

bool TraitInstanceNode::unify_with(Unifiable* other) {
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
}

void TraitInstanceNode::refine() {
    for (size_t i = 1; i < trait()->num_type_vars(); ++i) {
        TypeVar tv = trait()->type_var(i);
        assert(var_instances_.find(tv.node()) != var_instances_.end()); // CHECK is node() correct here?
        auto u = var_instances_.find(tv.node())->second;

        if (UnknownTypeNode* utn = u->isa<UnknownTypeNode>()) {
            assert(utn->is_instantiated());
            utn->instance()->refine();
            var_instances_[tv.node()] = utn;
        } else
            u->refine();
    }
}

bool TraitInstanceNode::is_known() const {
    bool result = true;
    for (auto tv : trait()->type_vars()) {
        assert(var_instances_.find(tv.node()) != var_instances_.end()); // CHECK is node() correct here?
        result = result && var_instances_.find(tv.node())->second->is_known();
    }
    return result;
}

bool TraitInstanceNode::is_closed() const {
    // TODO review this
    for (auto i : var_instances_) {
        if (!i.second->is_closed())
            return false;
    }
    return true;
}

Type TraitNode::find_method(Symbol name) {
#if 0
    auto it = all_methods_.find(name);
    return (it == all_methods_.end()) ? Type() : it->second;
#endif
    return Type();
}

Type TraitInstanceNode::find_method(Symbol name) {
#if 0
    auto it = all_methods_.find(name);
    if (it != all_methods_.end()) {
        return it->second;
    } else {
        Type fn = trait()->find_method(name);
        if (fn.empty()) {
            return fn;
        } else {
            SpecializeMap m = var_instances();
            Type t = fn->specialize(m);
            typetable().unify(t);
            return all_methods_[name] = t;
        }
    }
#endif
    return Type();
}

Unifiable* TraitNode::vspecialize(SpecializeMap& map) {
    return is_generic() ? typetable().instantiate_trait(this, map) : this;
}

Unifiable* TraitInstanceNode::vspecialize(SpecializeMap& map) {
    SpecializeMap m;
    for (auto i : var_instances_)
        m[i.first] = i.second->specialize(map);

    return typetable().instantiate_trait(trait(), m);
}

Unifiable* TraitImplNode::vspecialize(SpecializeMap& map) { 
    return map[this] = typetable().implement_trait(impl_decl(), trait()->specialize(map)).node(); 
}

}
