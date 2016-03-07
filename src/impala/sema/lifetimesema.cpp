#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

class LifetimeSema {
private:
    bool todo_ = true;

    friend void type_inference(const ModContents* mod) {
        LifetimeSema sema;

        while (sema.todo_) {
            sema.todo_ = false;
            mod->check(sema);
        }
    }
};

//------------------------------------------------------------------------------

/*
 * misc
 */

void TypeParam::check(LifetimeSema& sema) const {
    for (auto bound : bounds())
        bound->check(sema);
}

void LocalDecl::check(LifetimeSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void TypeParamList::check_type_params(LifetimeSema& sema) const {
    for (auto type_param : type_params())
        type_param->check(sema);
}

void ErrorASTType::check(LifetimeSema&) const {}
void PrimASTType::check(LifetimeSema&) const {}
void PtrASTType::check(LifetimeSema& sema) const { referenced_type()->check(sema); }
void IndefiniteArrayASTType::check(LifetimeSema& sema) const { elem_type()->check(sema); }
void DefiniteArrayASTType::check(LifetimeSema& sema) const { elem_type()->check(sema); }

void TupleASTType::check(LifetimeSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void ASTTypeApp::check(LifetimeSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void FnASTType::check(LifetimeSema& sema) const {
    check_type_params(sema);
    for (auto arg : args())
        arg->check(sema);
}

void Typeof::check(LifetimeSema& sema) const {
    expr()->check(sema);
}

void SimdASTType::check(LifetimeSema& sema) const {
    elem_type()->check(sema);
}

//------------------------------------------------------------------------------


/*
 * items
 */

void ModDecl::check(LifetimeSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
}

void ModContents::check(LifetimeSema& sema) const {
    for (auto item : items())
        item->check(sema);
}

void ExternBlock::check(LifetimeSema& sema) const {
    for (auto fn : fns())
        fn->check(sema);
}

void Typedef::check(LifetimeSema& sema) const {
    check_type_params(sema);
    ast_type()->check(sema);
}

void EnumDecl::check(LifetimeSema&) const {}

void StaticItem::check(LifetimeSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    if (init())
        init()->check(sema);
}

void Fn::fn_check(LifetimeSema& sema) const {
    check_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema);
}

void FnDecl::check(LifetimeSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(LifetimeSema& sema) const {
    check_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::check(LifetimeSema& sema) const {
    ast_type()->check(sema);
}

void TraitDecl::check(LifetimeSema& sema) const {
    check_type_params(sema);
    for (auto t : super_traits())
        t->check(sema);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(LifetimeSema& sema) const {
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

void EmptyExpr::check(LifetimeSema&) const {}

void BlockExprBase::check(LifetimeSema& sema) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
}

void LiteralExpr::check(LifetimeSema&) const {}
void CharExpr::check(LifetimeSema&) const {}
void StrExpr::check(LifetimeSema&) const {}
void FnExpr::check(LifetimeSema& sema) const { fn_check(sema); }

void PathElem::check(LifetimeSema&) const {}

void Path::check(LifetimeSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(LifetimeSema& sema) const {
    path()->check(sema);
}

void PrefixExpr::check(LifetimeSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(LifetimeSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(LifetimeSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(LifetimeSema& sema) const {
    lhs()->check(sema);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::check(LifetimeSema& sema) const {
    lhs()->check(sema);
    ast_type()->check(sema);
}

void DefiniteArrayExpr::check(LifetimeSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void RepeatedDefiniteArrayExpr::check(LifetimeSema& sema) const {
    value()->check(sema);
}

void IndefiniteArrayExpr::check(LifetimeSema& sema) const {
    dim()->check(sema);
    elem_type()->check(sema);
}

void TupleExpr::check(LifetimeSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void SimdExpr::check(LifetimeSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void StructExpr::check(LifetimeSema& sema) const {
    path()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema);
}

void MapExpr::check(LifetimeSema& sema) const {
    lhs()->check(sema);
    for (auto type_arg : type_args())
        type_arg->check(sema);
    for (auto arg : args())
        arg->check(sema);
}

void IfExpr::check(LifetimeSema& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void WhileExpr::check(LifetimeSema& sema) const {
    cond()->check(sema);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema);
}

void ForExpr::check(LifetimeSema& sema) const {
    expr()->check(sema);
    break_decl()->check(sema);
    fn_expr()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(LifetimeSema& sema) const { expr()->check(sema); }
void ItemStmt::check(LifetimeSema& sema) const { item()->check(sema); }
void LetStmt::check(LifetimeSema& sema) const {
    if (init())
        init()->check(sema);
    local()->check(sema);
}

//------------------------------------------------------------------------------

void lifetime_analysis(const ModContents* mod) {
    // TODO
    return;
}

//------------------------------------------------------------------------------

}
