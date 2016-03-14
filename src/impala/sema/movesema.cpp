#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/lvmap.h"

#include <iostream>

namespace impala {

//------------------------------------------------------------------------------

enum Liveness: char { DEAD, LIVE, ERROR };

inline Liveness payload2ls(payload_t pl) {
    assert(pl == DEAD || pl == LIVE || pl == ERROR);
    return (Liveness) pl;
}

// TODO: maybe this should be a member function of Type
bool type_copyable(const Type& type) {
    // TODO: this check is not good
    return false;
}

// TODO: this is the same code as in one place in lvmap.cpp, deduplicate
bool is_reference_type(const Type& type) {
    return type->kind() == Kind_borrowed_ptr || type->kind() == Kind_mut_ptr;
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
    sema.insert(this, (payload_t) Liveness::DEAD);
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
    if (init())
        init()->check(sema, false);
}

void Fn::fn_check(MoveSema& sema) const {
    check_type_params(sema);
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

inline void validate(const ASTNode* loc, Liveness live) {
    if (live == Liveness::DEAD)
        error(loc) << "cannot use " << loc << ", it is not live\n";
}

void check_lv(const Expr* lv, MoveSema& sema) {
    assert(lv->is_lvalue());
    Liveness live = payload2ls(lv->lookup_payload(sema));
    validate(lv, live);
    if (!type_copyable(lv->type())) {
        // TODO: except for creating borrows which is not handled here, can there ever be a case
        // where a non-copyable lvalue is used without beeing assigned?
        if (!lv->owns_value())
            error(lv) << "cannot move out of lvalue " << lv << " because it does not own its value\n";
        else
            lv->insert_payload(sema, Liveness::DEAD);
    }
}


void EmptyExpr::check(MoveSema&, bool assign_to) const {}

void BlockExprBase::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, false);
}

void LiteralExpr::check(MoveSema&, bool) const {}
void CharExpr::check(MoveSema&, bool) const {};
void StrExpr::check(MoveSema&, bool) const {}
void FnExpr::check(MoveSema& sema, bool) const {
    fn_check(sema);
}

void PathElem::check(MoveSema&) const {}

void Path::check(MoveSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(MoveSema& sema, bool assign_to) const {
    if (!assign_to)
        check_lv(this, sema);
    // if assign_to is true the variable is the target of an assignment and does not need to be live
}

void PrefixExpr::check(MoveSema& sema, bool assign_to) const  {
    switch (kind()) {
        case Token::MUL: // *
            if (assign_to) {
                // for an assignment target it suffices that the first ancestor of the first dereference
                // is live
                assert(rhs()->is_lvalue());
                check_lv(this, sema);
            } else {
                if (is_reference_type(rhs()->type()))
                    // since the liveness of a reference depends on the liveness of the owner of
                    // the reference, it suffices to check that
                    // This allows us to handle something like *&x
                    rhs()->check(sema, false);
                else {
                    assert(rhs()->type()->kind() == Kind_owned_ptr);
                    // values behind owned pointers may be moved
                    check_lv(this, sema);
                }
            }
            break;
        case Token::AND: // &
            assert(rhs()->is_lvalue());
            validate(rhs(), payload2ls(rhs()->lookup_payload(sema)));
            break;
        case Token::TILDE: // ~
            rhs()->check(sema, false);
            break;
        default:
            rhs()->check(sema, false);
    }
}

void InfixExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    switch (kind()) {
        // TODO: can this be simplified?
        case ASGN:      // all assignments
        case ADD_ASGN:
        case SUB_ASGN:
        case MUL_ASGN:
        case DIV_ASGN:
        case REM_ASGN:
        case AND_ASGN:
        case OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN:
            assert(lhs()->is_lvalue());
            rhs()->check(sema, false);
            lhs()->check(sema, true);
            lhs()->insert_payload(sema, Liveness::LIVE);
            break;
        default:
            lhs()->check(sema, false);
            rhs()->check(sema, false);
    }
}

void PostfixExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    lhs()->check(sema, false);
}

void FieldExpr::check(MoveSema& sema, bool assign_to) const {
    if (assign_to)
        lhs()->check(sema, true);
    else
        check_lv(this, sema);
}

void CastExpr::check(MoveSema& sema, bool assign_to) const {
    assert(false); //TODO: implement stuff
    lhs()->check(sema, assign_to);
}

void DefiniteArrayExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    for (auto arg : args())
        arg->check(sema, false);
}

void RepeatedDefiniteArrayExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    value()->check(sema, false);
}

void IndefiniteArrayExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    dim()->check(sema, false);
}

void TupleExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    for (auto arg : args())
        arg->check(sema, false);
}

void SimdExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    for (auto arg : args())
        arg->check(sema, false);
}

void StructExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    path()->check(sema);
    for (const auto& elem : elems())
        elem.expr()->check(sema, false);
}

void MapExpr::check(MoveSema& sema, bool assign_to) const {
    std::cout << "map expr: " << this << "\n"; // TODO: when called?
    if (is_lvalue())
        assert(false);
    //lhs()->check(sema, assign_to);
    for (auto arg : args())
        arg->check(sema, assign_to);
}

void IfExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    cond()->check(sema, false);
    then_expr()->check(sema, false);
    else_expr()->check(sema, false);
}

void WhileExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    cond()->check(sema, false);
    break_decl()->check(sema);
    continue_decl()->check(sema);
    body()->check(sema, false);
}

void ForExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    expr()->check(sema, false);
    break_decl()->check(sema);
    fn_expr()->check(sema, false);
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
    auto sema = MoveSema();
    mod->check(sema);
#ifndef NDEBUG
    // TODO: verify something? like in type sema?
#endif
}

//------------------------------------------------------------------------------

}
