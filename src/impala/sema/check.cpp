#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/scopetable.h"
#include "impala/sema/typetable.h"

namespace impala {

class Sema : public ScopeTable, public TypeTable {
public:
    Sema(bool nossa)
        : cur_fn_(nullptr)
        , nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    const Fn* cur_fn() const { return cur_fn_; }

private:
    const Fn* cur_fn_;
    bool nossa_;
};

//------------------------------------------------------------------------------

Type ErrorASTType::to_type(Sema& sema) const {
    return sema.type_error();
}

Type PrimASTType::to_type(Sema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.primtype(PrimType_##itype);
#include "impala/tokenlist.h"
    default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::to_type(Sema& sema) const {
    return Type(); // TODO
}

Type IndefiniteArrayASTType::to_type(Sema& sema) const {
    return Type(); // TODO
}

Type DefiniteArrayASTType::to_type(Sema& sema) const {
    return Type(); // TODO
}

Type TupleASTType::to_type(Sema& sema) const {
    return Type(); // TODO
}

Type ASTTypeApp::to_type(Sema& sema) const {
    const Decl* d = sema.lookup(symbol());

    if (d == nullptr) {
        // TODO better error handling
        assert(false);
    }

    if (auto tp = d->isa<TypeParam>()) {
        assert(elems().empty());
        assert(!tp->type_var().empty());
        return tp->type_var();
    } else {
        // TODO better error handling!
        assert(false && "Cannot convert a trait instance into a type");
        return sema.type_error();
    }
}

Type FnASTType::to_type(Sema& sema) const {
    // TODO type vars
    std::vector<Type> params;
    for (auto p : elems()) {
        params.push_back(p->to_type(sema));
    }
    return sema.fntype(params);
}

TypeTraitInstance ASTTypeApp::to_trait_instance(Sema& sema) const {
    const Decl* d = sema.lookup(symbol());
    if (auto t = d->isa<TraitDecl>()) {
        assert(t->typetrait() != nullptr);

        std::vector<Type> type_args;
        for (const ASTType* e : elems()) {
            type_args.push_back(e->to_type(sema));
        }

        return sema.instantiate_trait(t->typetrait(), type_args);
    } else {
        // TODO better error handling!
        assert(false && "Cannot convert a type variable into a trait instance");
        //return sema.type_error();
    }
}

//------------------------------------------------------------------------------

/*
 * items - check_head
 */

void ModDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void ModContents::check(Sema& sema) const {
    for (auto item : items()) item->check_head(sema);
    for (auto item : items()) item->check(sema);
}

void ForeignMod::check_head(Sema& sema) const {
    sema.insert(this);
}

void EnumDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void FnDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void StaticItem::check_head(Sema& sema) const {
    sema.insert(this);
}

void StructDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void TraitDecl::check_head(Sema& sema) const {
    sema.insert(this);
}

void Typedef::check_head(Sema& sema) const {
    sema.insert(this);
}

void Impl::check_head(Sema& sema) const {
    sema.insert(this);
}

/*
 * items - check
 */

void ModDecl::check(Sema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
}

void ForeignMod::check(Sema& sema) const {
}

void Typedef::check(Sema& sema) const {
}

void EnumDecl::check(Sema& sema) const {
}

void StaticItem::check(Sema& sema) const {
}

void ParametricASTType::check_type_params(Sema& sema) const {
    // we need two runs for types like fn[A:T[B], B:T[A]](A, B)

    for (const TypeParam* tp : type_params()) {
        tp->type_var_ = sema.typevar();
        sema.insert(tp);
    }

    // check bounds
    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>()) {
                tp->type_var()->add_restriction(trait_inst->to_trait_instance(sema));
            } else {
                // TODO better error handling
                assert(false && "Bounds must be trait instances, not types");
            }
        }
    }
}

void FnDecl::check(Sema& sema) const {
    // TODO introduce new scope?
    check_type_params(sema);
    // check parameters
    std::vector<Type> par_types;
    for (const Param* p : fn().params()) {
        sema.insert(p);
        Type pt = p->asttype()->to_type(sema);
        p->type_ = pt;
        par_types.push_back(pt);
    }
    // create FnType
    Type fn_type = sema.fntype(par_types);
    for (auto tp : type_params()) {
        assert(!tp->type_var().empty());
        fn_type->add_bound_var(tp->type_var());
    }
    sema.unify(fn_type);

    type_ = fn_type;

    // TODO set sema.cur_fn_?
    fn().body()->check(sema);

    // TODO check for correct return type
}

void StructDecl::check(Sema& sema) const {
}

void TraitDecl::check(Sema& sema) const {
    // TODO consider super traits and check methods
    type_trait_ = sema.typetrait(this, TypeTraitSet());

    check_type_params(sema);
    for (auto tp : type_params()) {
        assert(!tp->type_var().empty());
        type_trait_->add_bound_var(tp->type_var());
    }
}

void Impl::check(Sema& sema) const {
}

/*
 * expressions
 */

void EmptyExpr::check(Sema& sema) const {
}

void BlockExpr::check(Sema& sema) const {
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            item_stmt->item()->check_head(sema);
    }

    for (auto stmt : stmts())
        stmt->check(sema);

    expr()->check(sema);
    //assert(!expr()->type().empty()); TODO fails for EmptyExpr -> should we create an EmptyType=void?
    type_ = expr()->type();
}

void LiteralExpr::check(Sema& sema) const {
}

void FnExpr::check(Sema& sema) const {
}

#include <iostream>

void PathExpr::check(Sema& sema) const {
    // TODO consider longer paths
    decl_ = sema.lookup(path()->path_items()[0]->symbol());
    if (auto vdec = decl_->isa<ValueDecl>()) {
        type_ = vdec->type();
    } // TODO else
}

void PrefixExpr::check(Sema& sema) const {
}

void InfixExpr::check(Sema& sema) const {
}

void PostfixExpr::check(Sema& sema) const {
}

void FieldExpr::check(Sema& sema) const {
}

void CastExpr::check(Sema& sema) const {
}

void DefiniteArrayExpr::check(Sema& sema) const {
}

void RepeatedDefiniteArrayExpr::check(Sema& sema) const {
}

void IndefiniteArrayExpr::check(Sema& sema) const {
}

void TupleExpr::check(Sema& sema) const {
}

void StructExpr::check(Sema& sema) const {
}

void MapExpr::check(Sema& sema) const {
}

void IfExpr::check(Sema& sema) const {
}

void ForExpr::check(Sema& sema) const {
}

/*
 * statements
 */

void ExprStmt::check(Sema& sema) const {
    expr()->check(sema);
}

void ItemStmt::check(Sema& sema) const {
    item()->check(sema);
}

void LetStmt::check(Sema& sema) const {
    if (init())
        init()->check(sema);
}

//------------------------------------------------------------------------------

bool check(const ModContents* mod, bool nossa) {
    Sema sema(nossa);
    mod->check(sema);
    return sema.result();
}

}
