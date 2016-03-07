#include "impala/ast.h"
#include "impala/impala.h"

#include <iostream>

namespace impala {

//------------------------------------------------------------------------------

class MoveSema {
private:
    bool todo_ = true;

    friend void type_inference(const ModContents* mod) {
        MoveSema sema;

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
    expr()->check(sema);
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
        init()->check(sema);
}

void Fn::fn_check(MoveSema& sema) const {
    check_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->check(sema);
    }
    if (body() != nullptr)
        body()->check(sema);
}

void FnDecl::check(MoveSema& sema) const {
    std::cout << "blabla\n";
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

void EmptyExpr::check(MoveSema&) const {}

void BlockExprBase::check(MoveSema& sema) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
}

void LiteralExpr::check(MoveSema&) const {}
void CharExpr::check(MoveSema&) const {}
void StrExpr::check(MoveSema&) const {}
void FnExpr::check(MoveSema& sema) const { fn_check(sema); }

void PathElem::check(MoveSema&) const {}

void Path::check(MoveSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(MoveSema& sema) const {
    path()->check(sema);
}

void PrefixExpr::check(MoveSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(MoveSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(MoveSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(MoveSema& sema) const {
    lhs()->check(sema);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::check(MoveSema& sema) const {
    lhs()->check(sema);
    ast_type()->check(sema);
}

void DefiniteArrayExpr::check(MoveSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void RepeatedDefiniteArrayExpr::check(MoveSema& sema) const {
    value()->check(sema);
}

void IndefiniteArrayExpr::check(MoveSema& sema) const {
    dim()->check(sema);
    elem_type()->check(sema);
}

void TupleExpr::check(MoveSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void SimdExpr::check(MoveSema& sema) const {
    for (auto arg : args())
        arg->check(sema);
}

void StructExpr::check(MoveSema& sema) const {
    path()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema);
}

void MapExpr::check(MoveSema& sema) const {
    lhs()->check(sema);
    for (auto type_arg : type_args())
        type_arg->check(sema);
    for (auto arg : args())
        arg->check(sema);
}

void IfExpr::check(MoveSema& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void WhileExpr::check(MoveSema& sema) const {
    cond()->check(sema);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema);
}

void ForExpr::check(MoveSema& sema) const {
    expr()->check(sema);
    break_decl()->check(sema);
    fn_expr()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(MoveSema& sema) const { expr()->check(sema); }
void ItemStmt::check(MoveSema& sema) const { item()->check(sema); }
void LetStmt::check(MoveSema& sema) const {
    if (init())
        init()->check(sema);
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
