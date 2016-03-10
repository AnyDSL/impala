#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/lvmap.h"

namespace impala {

//------------------------------------------------------------------------------

enum Liveness: char { DEAD, LIVE, ERR };

inline Liveness payload2ls(payload_t pl) {
    assert(pl == DEAD || pl == LIVE || pl == ERR);
    return (Liveness) pl;
}

// TODO: maybe this should be a member function of Type
bool type_copyable(const Type& type) {
    // TODO: this check is not good
    return true;
}

//class MoveSema {
//public:
//    
//private:
//};

//------------------------------------------------------------------------------

/*
 * misc
 */

void TypeParam::check(MoveSema& sema) const {
    for (auto bound : bounds())
        bound->check(sema);
}

void LocalDecl::check(MoveSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void TypeParamList::check_type_params(MoveSema& sema) const {
    for (auto type_param : type_params())
        type_param->check(sema);
}

void ErrorASTType::check(MoveSema&) const {}
void PrimASTType::check(MoveSema&) const {}
void PtrASTType::check(MoveSema& sema) const { referenced_type()->check(sema); }
void IndefiniteArrayASTType::check(MoveSema& sema) const { elem_type()->check(sema); }
void DefiniteArrayASTType::check(MoveSema& sema) const { elem_type()->check(sema); }

void TupleASTType::check(MoveSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void ASTTypeApp::check(MoveSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void FnASTType::check(MoveSema& sema) const {
    check_type_params(sema);
    for (auto arg : args())
        arg->check(sema);
}

void Typeof::check(MoveSema& sema) const {
    expr()->check(sema, false);
}

void SimdASTType::check(MoveSema& sema) const {
    elem_type()->check(sema);
}

//------------------------------------------------------------------------------


/*
 * items
 */

void ModDecl::check(MoveSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
}

void ModContents::check(MoveSema& sema) const {
    for (auto item : items())
        item->check(sema);
}

void ExternBlock::check(MoveSema& sema) const {
    for (auto fn : fns())
        fn->check(sema);
}

void Typedef::check(MoveSema& sema) const {
    check_type_params(sema);
    ast_type()->check(sema);
}

void EnumDecl::check(MoveSema&) const {}

void StaticItem::check(MoveSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    if (init())
        init()->check(sema, false);
}

void Fn::fn_check(MoveSema& sema) const {
    check_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema, false);
}

void FnDecl::check(MoveSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(MoveSema& sema) const {
    check_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::check(MoveSema& sema) const {
    ast_type()->check(sema);
}

void TraitDecl::check(MoveSema& sema) const {
    check_type_params(sema);
    for (auto t : super_traits())
        t->check(sema);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(MoveSema& sema) const {
    check_type_params(sema);
    if (trait())
        trait()->check(sema);
    ast_type()->check(sema);
    for (auto fn : methods())
        fn->check(sema);
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

Liveness validate(const ASTNode* loc, Liveness live) {
    if (live == Liveness::DEAD) {
        error(loc);
        return Liveness::ERR;
    }
    return live;
}

Liveness check_lv(const Expr* lv, MoveSema& sema, bool assign_from) {
    Liveness live = payload2ls(lv->lookup_payload(sema));
    if (assign_from && !type_copyable(lv->type()))
        // TODO: produce error if live == DEAD here or in the assignment rule?
        // TODO: check owns value
        lv->insert_payload(sema, Liveness::DEAD);
    return live;
}


Liveness EmptyExpr::check(MoveSema&, bool assign_from) const {
    return Liveness::LIVE;
}

Liveness BlockExprBase::check(MoveSema& sema, bool assign_from) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness LiteralExpr::check(MoveSema&, bool) const { return Liveness::LIVE; }
Liveness CharExpr::check(MoveSema&, bool) const { return Liveness::LIVE; };
Liveness StrExpr::check(MoveSema&, bool) const { return Liveness::LIVE; }
Liveness FnExpr::check(MoveSema& sema, bool assign_from) const {
    fn_check(sema);
    return Liveness::DEAD;
}

Liveness PathExpr::check(MoveSema& sema, bool assign_from) const {
    return check_lv(this, sema, assign_from);
}

Liveness PrefixExpr::check(MoveSema& sema, bool assign_from) const  {
    switch (kind()) {
        case Token::MUL: // *
            return check_lv(this, sema, assign_from);
        case Token::AND: // &
            assert(rhs()->is_lvalue());
            return validate(rhs(), payload2ls(rhs()->lookup_payload(sema)));
            break;
        case Token::TILDE: // ~
            return validate(rhs(), rhs()->check(sema, true));
            break;
        default:
            // TODO: false makes the assumption that the other cases are copyable which is
            // currently ok, but maybe we should let this depend on the copyablility
            return validate(rhs(), rhs()->check(sema, false));
    }
}

Liveness InfixExpr::check(MoveSema& sema, bool assign_from) const {
    lhs()->check(sema, assign_from);
    rhs()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness PostfixExpr::check(MoveSema& sema, bool assign_from) const {
    lhs()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness FieldExpr::check(MoveSema& sema, bool assign_from) const {
    lhs()->check(sema, assign_from);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
    return Liveness::DEAD;
}

Liveness CastExpr::check(MoveSema& sema, bool assign_from) const {
    lhs()->check(sema, assign_from);
    ast_type()->check(sema);
    return Liveness::DEAD;
}

Liveness DefiniteArrayExpr::check(MoveSema& sema, bool assign_from) const {
    for (auto arg : args())
        arg->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness RepeatedDefiniteArrayExpr::check(MoveSema& sema, bool assign_from) const {
    value()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness IndefiniteArrayExpr::check(MoveSema& sema, bool assign_from) const {
    dim()->check(sema, assign_from);
    elem_type()->check(sema);
    return Liveness::DEAD;
}

Liveness TupleExpr::check(MoveSema& sema, bool assign_from) const {
    for (auto arg : args())
        arg->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness SimdExpr::check(MoveSema& sema, bool assign_from) const {
    for (auto arg : args())
        arg->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness StructExpr::check(MoveSema& sema, bool assign_from) const {
    path()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness MapExpr::check(MoveSema& sema, bool assign_from) const {
    lhs()->check(sema, assign_from);
    for (auto type_arg : type_args())
        type_arg->check(sema);
    for (auto arg : args())
        arg->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness IfExpr::check(MoveSema& sema, bool assign_from) const {
    cond()->check(sema, assign_from);
    then_expr()->check(sema, assign_from);
    else_expr()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness WhileExpr::check(MoveSema& sema, bool assign_from) const {
    cond()->check(sema, assign_from);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema, assign_from);
    return Liveness::DEAD;
}

Liveness ForExpr::check(MoveSema& sema, bool assign_from) const {
    expr()->check(sema, assign_from);
    break_decl()->check(sema);
    fn_expr()->check(sema, assign_from);
    return Liveness::DEAD;
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(MoveSema& sema) const { expr()->check(sema, false); }
void ItemStmt::check(MoveSema& sema) const { item()->check(sema); }
void LetStmt::check(MoveSema& sema) const {
    if (init())
        init()->check(sema, true);
    local()->check(sema);
}

//------------------------------------------------------------------------------

void move_analysis(const ModContents* mod) {
    // TODO: why do we need to say new here?
    auto sema = new MoveSema();
    mod->check(*sema);
#ifndef NDEBUG
    //TODO: verify something? like in type sema?
#endif
}

//------------------------------------------------------------------------------

}
