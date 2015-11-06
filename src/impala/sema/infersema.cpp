#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

class InferSema {
private:
    bool todo_ = true;

    friend void type_inference(const ModContents* mod) {
        InferSema sema;

        while (sema.todo_) {
            sema.todo_ = false;
            mod->infer(sema);
        }
    }
};

//------------------------------------------------------------------------------

/*
 * misc
 */

void TypeParam::infer(InferSema& sema) const {
    for (auto bound : bounds())
        bound->infer(sema);
}

void LocalDecl::infer(InferSema& sema) const {
    if (ast_type())
        ast_type()->infer(sema);
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void TypeParamList::infer_type_params(InferSema& sema) const {
    for (auto type_param : type_params())
        type_param->infer(sema);
}

void ErrorASTType::infer(InferSema&) const {}
void PrimASTType::infer(InferSema&) const {}
void PtrASTType::infer(InferSema& sema) const { referenced_type()->infer(sema); }
void IndefiniteArrayASTType::infer(InferSema& sema) const { elem_type()->infer(sema); }
void DefiniteArrayASTType::infer(InferSema& sema) const { elem_type()->infer(sema); }

void TupleASTType::infer(InferSema& sema) const {
    for (auto arg : args())
        arg->infer(sema);
}

void ASTTypeApp::infer(InferSema& sema) const {
    for (auto arg : args())
        arg->infer(sema);
}

void FnASTType::infer(InferSema& sema) const {
    infer_type_params(sema);
    for (auto arg : args())
        arg->infer(sema);
}

void Typeof::infer(InferSema& sema) const {
    expr()->infer(sema);
}

void SimdASTType::infer(InferSema& sema) const {
    elem_type()->infer(sema);
}

//------------------------------------------------------------------------------


/*
 * items
 */

void ModDecl::infer(InferSema& sema) const {
    if (mod_contents())
        mod_contents()->infer(sema);
}

void ModContents::infer(InferSema& sema) const {
    for (auto item : items())
        item->infer(sema);
}

void ExternBlock::infer(InferSema& sema) const {
    for (auto fn : fns())
        fn->infer(sema);
}

void Typedef::infer(InferSema& sema) const {
    infer_type_params(sema);
    ast_type()->infer(sema);
}

void EnumDecl::infer(InferSema&) const {}

void StaticItem::infer(InferSema& sema) const {
    if (ast_type())
        ast_type()->infer(sema);
    if (init())
        init()->infer(sema);
}

void Fn::fn_infer(InferSema& sema) const {
    infer_type_params(sema);
    for (auto param : params()) {
        if (param->ast_type())
            param->ast_type()->infer(sema);
    }
    if (body() != nullptr)
        body()->infer(sema);
}

void FnDecl::infer(InferSema& sema) const {
    fn_infer(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::infer(InferSema& sema) const {
    infer_type_params(sema);
    for (auto field_decl : field_decls()) {
        field_decl->infer(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::infer(InferSema& sema) const {
    ast_type()->infer(sema);
}

void TraitDecl::infer(InferSema& sema) const {
    infer_type_params(sema);
    for (auto t : super_traits())
        t->infer(sema);
    for (auto method : methods()) {
        method->infer(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::infer(InferSema& sema) const {
    infer_type_params(sema);
    if (trait())
        trait()->infer(sema);
    ast_type()->infer(sema);
    for (auto fn : methods())
        fn->infer(sema);
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::infer(InferSema&) const {}
void SizeofExpr::infer(InferSema& sema) const { ast_type()->infer(sema); }

void BlockExprBase::infer(InferSema& sema) const {
    for (auto stmt : stmts())
        stmt->infer(sema);
    expr()->infer(sema);
}

void LiteralExpr::infer(InferSema&) const {}
void CharExpr::infer(InferSema&) const {}
void StrExpr::infer(InferSema&) const {}
void FnExpr::infer(InferSema& sema) const { fn_infer(sema); }

void PathElem::infer(InferSema&) const {}

void Path::infer(InferSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->infer(sema);
}

void PathExpr::infer(InferSema& sema) const {
    path()->infer(sema);
}

void PrefixExpr::infer(InferSema& sema) const  {                     rhs()->infer(sema); }
void InfixExpr::infer(InferSema& sema) const   { lhs()->infer(sema); rhs()->infer(sema); }
void PostfixExpr::infer(InferSema& sema) const { lhs()->infer(sema); }

void FieldExpr::infer(InferSema& sema) const {
    lhs()->infer(sema);
    // don't infer symbol here as it depends on lhs' type - must be done in TypeSema
}

void CastExpr::infer(InferSema& sema) const {
    lhs()->infer(sema);
    ast_type()->infer(sema);
}

void DefiniteArrayExpr::infer(InferSema& sema) const {
    for (auto arg : args())
        arg->infer(sema);
}

void RepeatedDefiniteArrayExpr::infer(InferSema& sema) const {
    value()->infer(sema);
}

void IndefiniteArrayExpr::infer(InferSema& sema) const {
    dim()->infer(sema);
    elem_type()->infer(sema);
}

void TupleExpr::infer(InferSema& sema) const {
    for (auto arg : args())
        arg->infer(sema);
}

void SimdExpr::infer(InferSema& sema) const {
    for (auto arg : args())
        arg->infer(sema);
}

void StructExpr::infer(InferSema& sema) const {
    path()->infer(sema);
    for (const auto& elem : elems())
        elem.expr()->infer(sema);
}

void MapExpr::infer(InferSema& sema) const {
    lhs()->infer(sema);
    for (auto type_arg : type_args())
        type_arg->infer(sema);
    for (auto arg : args())
        arg->infer(sema);
}

void IfExpr::infer(InferSema& sema) const {
    cond()->infer(sema);
    then_expr()->infer(sema);
    else_expr()->infer(sema);
}

void WhileExpr::infer(InferSema& sema) const {
    cond()->infer(sema);
    break_decl()->infer(sema);
    continue_decl()->infer(sema);
    body()->infer(sema);
}

void ForExpr::infer(InferSema& sema) const {
    expr()->infer(sema);
    break_decl()->infer(sema);
    fn_expr()->infer(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::infer(InferSema& sema) const { expr()->infer(sema); }
void ItemStmt::infer(InferSema& sema) const { item()->infer(sema); }
void LetStmt::infer(InferSema& sema) const {
    if (init())
        init()->infer(sema);
    local()->infer(sema);
}

//------------------------------------------------------------------------------

}
