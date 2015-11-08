#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

class BorrowChecker {
private:
    bool todo_ = true;

    friend void type_inference(const ModContents* mod) {
        BorrowChecker sema;

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

void TypeParam::check(BorrowChecker& sema) const {
    for (auto bound : bounds())
        bound->check(sema);
}

void LocalDecl::check(BorrowChecker& sema) const {
    if (ast_type())
        ast_type()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void TypeParamList::check_type_params(BorrowChecker& sema) const {
    for (auto type_param : type_params())
        type_param->check(sema);
}

void ErrorASTType::check(BorrowChecker&) const {}
void PrimASTType::check(BorrowChecker&) const {}
void PtrASTType::check(BorrowChecker& sema) const { referenced_type()->check(sema); }
void IndefiniteArrayASTType::check(BorrowChecker& sema) const { elem_type()->check(sema); }
void DefiniteArrayASTType::check(BorrowChecker& sema) const { elem_type()->check(sema); }

void TupleASTType::check(BorrowChecker& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void ASTTypeApp::check(BorrowChecker& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void FnASTType::check(BorrowChecker& sema) const {
    check_type_params(sema);
    for (auto arg : args())
        arg->check(sema);
}

void Typeof::check(BorrowChecker& sema) const {
    expr()->check(sema);
}

void SimdASTType::check(BorrowChecker& sema) const {
    elem_type()->check(sema);
}

//------------------------------------------------------------------------------


/*
 * items
 */

void ModDecl::check(BorrowChecker& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
}

void ModContents::check(BorrowChecker& sema) const {
    for (auto item : items())
        item->check(sema);
}

void ExternBlock::check(BorrowChecker& sema) const {
    for (auto fn : fns())
        fn->check(sema);
}

void Typedef::check(BorrowChecker& sema) const {
    check_type_params(sema);
    ast_type()->check(sema);
}

void EnumDecl::check(BorrowChecker&) const {}

void StaticItem::check(BorrowChecker& sema) const {
    if (ast_type())
        ast_type()->check(sema);
    if (init())
        init()->check(sema);
}

void Fn::fn_check(BorrowChecker& sema) const {
    check_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema);
}

void FnDecl::check(BorrowChecker& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(BorrowChecker& sema) const {
    check_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::check(BorrowChecker& sema) const {
    ast_type()->check(sema);
}

void TraitDecl::check(BorrowChecker& sema) const {
    check_type_params(sema);
    for (auto t : super_traits())
        t->check(sema);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(BorrowChecker& sema) const {
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

void EmptyExpr::check(BorrowChecker&) const {}
void SizeofExpr::check(BorrowChecker& sema) const { ast_type()->check(sema); }

void BlockExprBase::check(BorrowChecker& sema) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
}

void LiteralExpr::check(BorrowChecker&) const {}
void CharExpr::check(BorrowChecker&) const {}
void StrExpr::check(BorrowChecker&) const {}
void FnExpr::check(BorrowChecker& sema) const { fn_check(sema); }

void PathElem::check(BorrowChecker&) const {}

void Path::check(BorrowChecker& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(BorrowChecker& sema) const {
    path()->check(sema);
}

void PrefixExpr::check(BorrowChecker& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(BorrowChecker& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(BorrowChecker& sema) const { lhs()->check(sema); }

void FieldExpr::check(BorrowChecker& sema) const {
    lhs()->check(sema);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::check(BorrowChecker& sema) const {
    lhs()->check(sema);
    ast_type()->check(sema);
}

void DefiniteArrayExpr::check(BorrowChecker& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void RepeatedDefiniteArrayExpr::check(BorrowChecker& sema) const {
    value()->check(sema);
}

void IndefiniteArrayExpr::check(BorrowChecker& sema) const {
    dim()->check(sema);
    elem_type()->check(sema);
}

void TupleExpr::check(BorrowChecker& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void SimdExpr::check(BorrowChecker& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void StructExpr::check(BorrowChecker& sema) const {
    path()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema);
}

void MapExpr::check(BorrowChecker& sema) const {
    lhs()->check(sema);
    for (auto type_arg : type_args())
        type_arg->check(sema);
    for (auto arg : args())
        arg->check(sema);
}

void IfExpr::check(BorrowChecker& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void WhileExpr::check(BorrowChecker& sema) const {
    cond()->check(sema);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema);
}

void ForExpr::check(BorrowChecker& sema) const {
    expr()->check(sema);
    break_decl()->check(sema);
    fn_expr()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(BorrowChecker& sema) const { expr()->check(sema); }
void ItemStmt::check(BorrowChecker& sema) const { item()->check(sema); }
void LetStmt::check(BorrowChecker& sema) const {
    if (init())
        init()->check(sema);
    local()->check(sema);
}

//------------------------------------------------------------------------------

}
