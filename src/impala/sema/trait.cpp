#include "impala/sema/trait.h"
#include "impala/sema/typetable.h"
#include "impala/ast.h"

namespace impala {

TraitNode::TraitNode(TypeTable& tt, const TraitDecl* trait_decl)
    : TUnifiable(tt)
    , trait_decl_(trait_decl)
    , super_traits_()
{}

TraitInstanceNode::TraitInstanceNode(const Trait trait, const SpecializeMap& var_instances)
    : TraitNode(trait->typetable(), trait->trait_decl())
    , trait_(trait)
    , var_instances_(var_instances)
{
    assert(trait_->num_bound_vars() == var_instances_.size());
}

void TraitNode::add_super_trait(Trait t) {
    typetable().unify(t);
    super_traits_.insert(t);
    super_traits_.insert(t->super_traits().begin(), t->super_traits().end());

    for (auto mname : t->declared_methods()) {
        assert(t->has_method(mname));
        if (!add_method(mname, t->find_method(mname), true))
            typetable().error(this->trait_decl()) << "conflicting method name in super traits: '" << mname << "'\n";
    }
}

bool TraitInstanceNode::unify_with(Unifiable* other) {
    if (auto tinst = other->isa<TraitInstanceNode>()) {
        if (trait() == tinst->trait()) {
            auto other_vinsts = tinst->var_instances_;
            if (var_instances_.size() == other_vinsts.size()) {
                bool result = true;
                for (auto v : trait()->bound_vars()) {
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
    for (size_t i = 1; i < trait()->num_bound_vars(); ++i) {
        TypeVar tv = trait()->bound_var(i);
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
    for (auto tv : trait()->bound_vars()) {
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

bool TraitNode::add_method(Symbol name, Type method_type, bool inherited) {
    assert(!is_unified() && "Unified traits must not be changed anymore!");
    if (has_method(name)) {
        return false;
    } else {
        all_methods_[name] = method_type;
        if (!inherited)
            declared_methods_.push_back(name);
        return true;
    }
}

Type TraitNode::find_method(Symbol name) {
    auto it = all_methods_.find(name);
    return (it == all_methods_.end()) ? Type() : it->second;
}

Type TraitInstanceNode::find_method(Symbol name) {
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
}

const MethodTable& TraitInstanceNode::all_methods() {
    if (all_methods_.size() < trait()->num_methods()) {
        for (auto p : trait()->all_methods())
            find_method(p.first); // this will insert the specialized method
    }
    assert(all_methods_.size() == trait()->num_methods());
    return all_methods_;
}

const UniSet<Trait>& TraitInstanceNode::super_traits() {
    if (super_traits_.size() < trait()->super_traits().size()) {
        // specialize super traits
        for (auto super : trait()->super_traits()) {
            SpecializeMap m = this->var_instances();
            Trait super_inst = super->specialize(m);
            //typetable().unify(super_inst);
            auto p = super_traits_.insert(super_inst);
            assert(p.second && "Hash/Equal broken");
        }
    }
    assert(super_traits_.size() == trait()->super_traits().size());
    return super_traits_;
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
