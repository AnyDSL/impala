#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

class BorrowSema {
private:
    bool todo_ = true;

    friend void type_inference(const ModContents* mod) {
        BorrowSema sema;

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

void ASTTypeParam::check(BorrowSema& sema) const {
    for (auto bound : bounds())
        bound->check(sema);
}

void LocalDecl::check(BorrowSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void ASTTypeParamList::check_ast_type_params(BorrowSema& sema) const {
    for (auto ast_type_param : ast_type_params())
        ast_type_param->check(sema);
}

void ErrorASTType::check(BorrowSema&) const {}
void PrimASTType::check(BorrowSema&) const {}
void PtrASTType::check(BorrowSema& sema) const { referenced_ast_type()->check(sema); }
void IndefiniteArrayASTType::check(BorrowSema& sema) const { elem_ast_type()->check(sema); }
void DefiniteArrayASTType::check(BorrowSema& sema) const { elem_ast_type()->check(sema); }
void SimdASTType::check(BorrowSema& sema) const { elem_ast_type()->check(sema); }
void Typeof::check(BorrowSema& sema) const { expr()->check(sema); }

void TupleASTType::check(BorrowSema& sema) const {
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void ASTTypeApp::check(BorrowSema& sema) const {
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void FnASTType::check(BorrowSema& sema) const {
    check_ast_type_params(sema);
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

//------------------------------------------------------------------------------


/*
 * items
 */

void ModDecl::check(BorrowSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
}

void ModContents::check(BorrowSema& sema) const {
    for (auto item : items())
        item->check(sema);
}

void ExternBlock::check(BorrowSema& sema) const {
    for (auto fn : fns())
        fn->check(sema);
}

void Typedef::check(BorrowSema& sema) const {
    check_ast_type_params(sema);
    ast_type()->check(sema);
}

void EnumDecl::check(BorrowSema&) const {}

void StaticItem::check(BorrowSema& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    if (init())
        init()->check(sema);
}

void Fn::fn_check(BorrowSema& sema) const {
    check_ast_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema);
}

void FnDecl::check(BorrowSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(BorrowSema& sema) const {
    check_ast_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::check(BorrowSema& sema) const {
    ast_type()->check(sema);
}

void TraitDecl::check(BorrowSema& sema) const {
    check_ast_type_params(sema);
    for (auto t : super_traits())
        t->check(sema);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(BorrowSema& sema) const {
    check_ast_type_params(sema);
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

void EmptyExpr::check(BorrowSema&) const {}

void BlockExprBase::check(BorrowSema& sema) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
}

void LiteralExpr::check(BorrowSema&) const {}
void CharExpr::check(BorrowSema&) const {}
void StrExpr::check(BorrowSema&) const {}
void FnExpr::check(BorrowSema& sema) const { fn_check(sema); }

void Path::Elem::check(BorrowSema&) const {}

void Path::check(BorrowSema& sema) const {
    for (auto elem : elems())
        elem->check(sema);
}

void PathExpr::check(BorrowSema& sema) const {
    path()->check(sema);
}

void PrefixExpr::check(BorrowSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(BorrowSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(BorrowSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(BorrowSema& sema) const {
    lhs()->check(sema);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::check(BorrowSema& sema) const {
    lhs()->check(sema);
    //ast_type()->check(sema);
    // TODO
}

void DefiniteArrayExpr::check(BorrowSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void RepeatedDefiniteArrayExpr::check(BorrowSema& sema) const {
    value()->check(sema);
}

void IndefiniteArrayExpr::check(BorrowSema& sema) const {
    dim()->check(sema);
    elem_ast_type()->check(sema);
}

void TupleExpr::check(BorrowSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void SimdExpr::check(BorrowSema& sema) const {
    for (const auto& arg : args())
        arg->check(sema);
}

void StructExpr::check(BorrowSema& sema) const {
    ast_type_app()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema);
}

void TypeAppExpr::check(BorrowSema& sema) const {
    lhs()->check(sema);
    for (auto ast_type_arg : ast_type_args())
        ast_type_arg->check(sema);
}

void MapExpr::check(BorrowSema& sema) const {
    lhs()->check(sema);
    for (const auto& arg : args())
        arg->check(sema);
}

void IfExpr::check(BorrowSema& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void WhileExpr::check(BorrowSema& sema) const {
    cond()->check(sema);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema);
}

void ForExpr::check(BorrowSema& sema) const {
    expr()->check(sema);
    break_decl()->check(sema);
    fn_expr()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * patterns
 */

void TuplePtrn::check(BorrowSema& sema) const {
    for (const auto& elem : elems()) {
        elem->check(sema);
    }
}

void IdPtrn::check(BorrowSema& sema) const {
    local()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(BorrowSema& sema) const { expr()->check(sema); }
void ItemStmt::check(BorrowSema& sema) const { item()->check(sema); }
void LetStmt::check(BorrowSema& sema) const {
    if (init())
        init()->check(sema);
    ptrn()->check(sema);
}

//------------------------------------------------------------------------------

}
