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

#include <iostream>

Type PrimASTType::to_type(Sema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.primtype(PrimType_##itype);
#include "impala/tokenlist.h"
    default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::to_type(Sema& sema) const {
    // TODO
}

Type IndefiniteArrayASTType::to_type(Sema& sema) const {
    // TODO
}

Type DefiniteArrayASTType::to_type(Sema& sema) const {
    // TODO
}

Type TupleASTType::to_type(Sema& sema) const {
    // TODO
}

Type ASTTypeApp::to_type(Sema& sema) const {
    // TODO
}

Type FnASTType::to_type(Sema& sema) const {
    // TODO type vars
    std::vector<Type> params;
    for (auto p : elems()) {
        params.push_back(p->to_type(sema));
    }
    return sema.fntype(params);
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

void FnDecl::check(Sema& sema) const {
    // TODO set type
    // TODO check type variables
    // check parameters
    std::vector<Type> par_types;
    for (const Param* p : fn().params()) {
        sema.insert(p);
        Type pt = p->asttype()->to_type(sema);
        p->type_ = pt;
        par_types.push_back(pt);
    }
    // create FnType
    // TODO consider type variables
    Type fn_type = sema.fntype(par_types);
    type_ = fn_type;

    // TODO remove output
    type_->dump();

    fn_.body()->check(sema);
}

void StructDecl::check(Sema& sema) const {
}

void TraitDecl::check(Sema& sema) const {
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
}

void LiteralExpr::check(Sema& sema) const {
}

void FnExpr::check(Sema& sema) const {
}

void PathExpr::check(Sema& sema) const {
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
