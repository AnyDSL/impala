#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/lvmap.h"

#include <iostream>

namespace impala {

//------------------------------------------------------------------------------

enum Liveness: char { LIVE = 0, DEAD = 1, ERROR = 2};

inline Liveness payload2ls(payload_t pl) {
    assert(pl == DEAD || pl == LIVE || pl == ERROR);
    return (Liveness) pl;
}

//------------------------------------------------------------------------------

/*
 * misc
 */

void Param::check(MoveSema& sema) const {
    sema.insert(this, (payload_t) Liveness::LIVE, loc());
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
}

void EnumDecl::check(MoveSema&) const {}

void StaticItem::check(MoveSema& sema) const {
    if (init())
        init()->check(sema, false);
}

void Fn::fn_check(MoveSema& sema) const {
    sema.enter_scope();
    for (auto param : params())
        param->check(sema);
    if (body() != nullptr)
        body()->check(sema, false);
    sema.leave_scope();
}

void FnDecl::check(MoveSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(MoveSema& sema) const {
    for (auto field_decl : field_decls()) {
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void TraitDecl::check(MoveSema& sema) const {
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(MoveSema& sema) const {
    for (auto fn : methods())
        fn->check(sema);
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

inline void validate(const ASTNode* node, const Payload& pl) {
    Liveness live = payload2ls(pl.get_value());
    if (live == Liveness::DEAD) {
        std::ostream& es = error(node);
        es << "cannot use '" << node << "', it is not live\n";
        if (pl.loc().is_set())
            es << "note: the value moved here: " << pl.loc() << "\n";
        else
            es << "note: the value was not initialized\n";
        // TODO: use something like note() instead of warn()
    }
}

inline bool check_owns_value(const Expr* expr) {
    if (expr->owns_value())
        return true;
    error(expr) << "cannot move out of expression '" << expr << "' because it does not own its value\n";
    return false;
}

void check_lv(const Expr* lv, MoveSema& sema, bool assign_to) {
    const Payload& pl = lookup_payload(*lv, sema);
    Liveness live = payload2ls(pl.get_value());

    if (assign_to) {
        if (live == Liveness::DEAD)
            // TODO: display move location
            error(lv) << "cannot assign to pointer or reference owned by '" << lv << "' because '" << lv << "' is not live\n";
        return;
    }

    validate(lv, pl);
    if (!is_copyable(lv->type()) && check_owns_value(lv))
        insert(*lv, sema, Liveness::DEAD, lv->loc());
}


void EmptyExpr::check(MoveSema&, bool assign_to) const { assert(!assign_to); }

void BlockExprBase::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    sema.enter_scope();
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, false);
    sema.leave_scope();
}

void LiteralExpr::check(MoveSema&, bool assign_to) const { assert(!assign_to); }
void CharExpr::check(MoveSema&, bool assign_to) const { assert(!assign_to); };
void StrExpr::check(MoveSema&, bool assign_to) const { assert(!assign_to); }
void FnExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to); 
    fn_check(sema);
}

void PathElem::check(MoveSema&) const {}

void Path::check(MoveSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(MoveSema& sema, bool assign_to) const {
    if (!assign_to)
        check_lv(this, sema, false);
    // if assign_to is true the variable is the target of an assignment and does not need to be live
}

void PrefixExpr::check(MoveSema& sema, bool assign_to) const  {
    switch (kind()) {
        case Token::MUL: // *
            if (assign_to)
                // for an assignment target it suffices that the first ancestor of the first dereference
                // is live
                check_lv(rhs(), sema, true);
            else
                check_lv(this, sema, false);
            break;
        case Token::AND: // &
            assert(rhs()->is_lvalue());
            validate(rhs(), lookup_payload(*rhs(), sema));
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
            insert(*lhs(), sema, Liveness::LIVE, loc());
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
        check_lv(this, sema, false);
}

void CastExpr::check(MoveSema& sema, bool assign_to) const {
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
    if (is_lvalue()) {
        // handled like the * case of Prefix expr
        if (assign_to)
            check_lv(lhs(), sema, true);
        else
            check_lv(this, sema, false);
    }
    //lhs()->check(sema, assign_to);
    for (auto arg : args())
        arg->check(sema, false);
}

void IfExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    cond()->check(sema, false);

    //std::cout << "sema 1:\n" << &sema << "\n";

    sema.enter_scope();
    MoveSema else_sema(sema);

    //std::cout << "sema:\n" << &sema << "\n";
    //std::cout << "else_sema:\n" << &sema << "\n";

    then_expr()->check(sema, false);
    else_expr()->check(else_sema, false);

    //std::cout << "sema:\n" << &sema << "\n";
    //std::cout << "else_sema:\n" << &sema << "\n";

    sema.leave_scope();
    else_sema.leave_scope();
    sema.merge(else_sema);
}

void WhileExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    // TODO: do we need break and continue decl?
    //break_decl()->check(sema);
    //continue_decl()->check(sema);

    MoveSema body_sema(sema);
    body_sema.enter_scope();
    body()->check(body_sema, false);
    body_sema.leave_scope();

    sema.merge(body_sema);
    // check the condition after the body because the body might move things the condition needs
    cond()->check(sema, false);
}

void ForExpr::check(MoveSema& sema, bool assign_to) const {
    assert(!assign_to);
    //break_decl()->check(sema);

    MoveSema body_sema(sema);
    //body_sema.enter_scope();
    fn_expr()->check(body_sema, false);
    //body_sema.leave_scope();

    sema.merge(body_sema);
    expr()->check(sema, false);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(MoveSema& sema) const { expr()->check(sema, false); }
void ItemStmt::check(MoveSema& sema) const { item()->check(sema); }
void LetStmt::check(MoveSema& sema) const {
    if (init()) {
        init()->check(sema, false);
        sema.insert(local(), Liveness::LIVE, loc()); 
    } else
        sema.insert(local(), Liveness::DEAD, UNSET_LOCATION); 
}

//------------------------------------------------------------------------------

void move_analysis(const ModContents* mod) {
    LvMapComparator comparator = LvMapComparator();
    MoveSema sema(comparator);
    mod->check(sema);
#ifndef NDEBUG
    // TODO: verify something? like in type sema?
#endif
}

//------------------------------------------------------------------------------

}
