#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

//------------------------------------------------------------------------------

enum BorrowState { MUT = 0, FREEZED = 1, CLAIMED = 2 };
enum InitState { UNINIT = 0, INIT = 1 };

inline payload_t bs2pl(BorrowState bs) { return (payload_t) bs; }
inline payload_t is2pl(InitState is) { return (payload_t) is; }
inline BorrowState pl2bs(payload_t pl) { return (BorrowState) pl; }
inline InitState pl2is(payload_t pl) { return (InitState) pl; }

typedef LvMap BorrowMap;
typedef LvMap InitMap;

const LvMapComparator DEFAULT_COMPARATOR = LvMapComparator();

//------------------------------------------------------------------------------

/*
 * BorrowSema
 */

class BorrowSema {
private:
    BorrowSema& operator= (const BorrowSema&);

public:
    BorrowSema();
    ~BorrowSema();

    // TODO: make those functions const
    Payload lookup_borrow(const Expr*);
    InitState lookup_init(const Expr*);
    size_t current_scope() const { return borrow_maps_.size() - 1; }
    size_t get_target_scope(const Expr*);

    void add_decl(const ValueDecl*, InitState);
    void add_borrow(const Expr*, BorrowState, size_t);

    void enter_scope();
    void leave_scope();

    void merge(BorrowSema& other);

    //std::ostream& stream(std::ostream&) const;

private:
    std::vector<std::shared_ptr<BorrowMap>> borrow_maps_;
    InitMap init_map_;
    thorin::HashMap<const ValueDecl*, size_t> decl_scope_map_;
    std::stack<const ValueDecl*> scope_stack_;
};



BorrowSema::BorrowSema()
    : borrow_maps_(std::vector<std::shared_ptr<BorrowMap>>())
    , init_map_(InitMap(DEFAULT_COMPARATOR))
    , decl_scope_map_(thorin::HashMap<const ValueDecl*, size_t>())
    , scope_stack_(std::stack<const ValueDecl*>())
    {
        BorrowMap* inital_map = new BorrowMap(DEFAULT_COMPARATOR);
        borrow_maps_.push_back(std::shared_ptr<BorrowMap>(inital_map));
    }

BorrowSema::~BorrowSema() {}

Payload BorrowSema::lookup_borrow(const Expr* expr) {
    const ValueDecl* decl = expr->get_decl();
    for (auto i = borrow_maps_.rbegin(); i != borrow_maps_.rend(); i++) {
        if ((*i)->contains(decl))
            return lookup_payload(*expr, **i);
    }
    assert(false);
}

InitState BorrowSema::lookup_init(const Expr* expr) {
    Payload pl = lookup_payload(*expr, init_map_);
    return pl2is(pl.get_value());
}

size_t BorrowSema::get_target_scope(const Expr* expr) {
    const ValueDecl* decl = expr->get_decl();
    assert(decl_scope_map_.contains(decl));

    return decl_scope_map_[decl];
}

BorrowMap& get_borrow_map_for_insert(std::vector<std::shared_ptr<BorrowMap>>& borrow_maps,
    size_t scope_level) {
    
    std::shared_ptr<BorrowMap> bmap = borrow_maps[scope_level];
    if (bmap.use_count() > 2) {
        // this map is shared over multiple scopes, we need to make a scope exclusive copy
        BorrowMap* new_map = new BorrowMap(*bmap);
        bmap = std::shared_ptr<BorrowMap>(new_map);
        borrow_maps[scope_level] = bmap;
    }
    return *bmap;
}

void BorrowSema::add_decl(const ValueDecl* decl, InitState is) {
    assert(!decl_scope_map_.contains(decl));

    BorrowMap& bmap = get_borrow_map_for_insert(borrow_maps_, current_scope());
    bmap.insert(decl, bs2pl(BorrowState::MUT), decl->loc());
    init_map_.insert(decl, is2pl(is), decl->loc());
    decl_scope_map_[decl] = current_scope();
    scope_stack_.push(decl);
}

void BorrowSema::add_borrow(const Expr* expr, BorrowState bs, size_t target_scope) {
    assert(target_scope < borrow_maps_.size());

    BorrowMap& bmap = get_borrow_map_for_insert(borrow_maps_, target_scope);
    insert(*expr, bmap, bs2pl(bs), expr->loc());
}

void BorrowSema::enter_scope() {
    borrow_maps_.push_back(borrow_maps_.back());
    init_map_.enter_scope();
    scope_stack_.push(nullptr);
}

void BorrowSema::leave_scope() {
    assert(!scope_stack_.empty());
    assert(!borrow_maps_.empty());

    borrow_maps_.pop_back();
    init_map_.leave_scope();

    // TODO: same code as in LvMap, can this be deduplicated?
    const ValueDecl* decl;
    do {
        decl = scope_stack_.top();
        scope_stack_.pop();
        if (decl != nullptr) {
            assert(decl_scope_map_.contains(decl));
            decl_scope_map_.erase(decl);
        }
    } while (decl != nullptr);
}

void BorrowSema::merge(BorrowSema& other) {
    assert(borrow_maps_.size() == other.borrow_maps_.size());

    init_map_.merge(other.init_map_);
    for (size_t i = 0; i < borrow_maps_.size(); i++) {
        std::shared_ptr<BorrowMap> this_map = borrow_maps_[i];
        std::shared_ptr<BorrowMap> other_map = other.borrow_maps_[i];
        if (this_map != other_map)
            this_map->merge(*other_map);
    }
    // make other unusable
    other.borrow_maps_.clear();

    // TODO: should not be necessary to merge decl_scope_map_
}

//std::ostream& BorrowSema::stream(std::ostream&) const {
//    assert(false);
//}


//------------------------------------------------------------------------------

/*
 * misc
 */

void LocalDecl::check(BorrowSema& sema) const {
    // TODO:
    sema.add_decl(this, InitState::UNINIT);
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
}

void EnumDecl::check(BorrowSema&) const {}

void StaticItem::check(BorrowSema& sema) const {
    if (init())
        init()->check(sema, BorrowExpectation::ASSIGN_FROM, sema.current_scope());
}

void Fn::fn_check(BorrowSema& sema) const {
    // TODO: check params
    //for (auto param : params())
    //    param->check(sema);
    if (body() != nullptr)
        body()->check(sema, BorrowExpectation::ASSIGN_FROM, sema.current_scope());
}

void FnDecl::check(BorrowSema& sema) const {
    fn_check(sema);
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(BorrowSema& sema) const {
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
        field_table_[field_decl->symbol()] = field_decl;
    }
}

void FieldDecl::check(BorrowSema& sema) const {
}

void TraitDecl::check(BorrowSema& sema) const {
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(BorrowSema& sema) const {
    for (auto fn : methods())
        fn->check(sema);
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

inline BorrowState type_expectation(Type t) {
    return is_copyable(t) ? BorrowState::FREEZED : BorrowState::MUT;
}

bool initial_lv_check(const Expr* expr, BorrowSema& sema, BorrowExpectation expectation) {
    assert(expr->is_lvalue());
    
    Payload pl = sema.lookup_borrow(expr);
    BorrowState borrowed = pl2bs(pl.get_value());
    switch (expectation) {
        case BorrowExpectation::ASSIGN_FROM: {
            Type t = expr->type();
            BorrowState type_state = type_expectation(t);
            if (DEFAULT_COMPARATOR.compare(borrowed, type_state) == Relation::LESS) {
                error(expr) << "cannot use " << expr << " because it is borrowed\n";
                return true;
            }
            BorrowExpectation check_expectation = type_state == BorrowState::MUT ?
                BorrowExpectation::CHECK_MUT : BorrowExpectation::CHECK_FREEZED;
            // TODO: 0's as target_scope should not matter here
            expr->check(sema, check_expectation, 0);
            break;
        }
        case BorrowExpectation::ASSIGN_TO: {
            if (DEFAULT_COMPARATOR.compare(borrowed, BorrowState::MUT) == Relation::LESS) {
                error(expr) << "cannot assign to " << expr << " because it is borrowed\n";
                return true;
            }
            if (sema.lookup_init(expr) == InitState::UNINIT)
                return true;
            expr->check(sema, BorrowExpectation::CHECK_MUT, 0);
            break;
        }
        case BorrowExpectation::BORROW_MUT:
        case BorrowExpectation::BORROW_FREEZED: {
            BorrowState exp_state = expectation == BorrowExpectation::BORROW_MUT ?
                BorrowState::MUT : BorrowState::FREEZED;
            if (DEFAULT_COMPARATOR.compare(borrowed, exp_state) == Relation::LESS) {
                error(expr) << "cannot borrow " << expr << " because it is already borrowed\n";
                return true;
            }
            BorrowExpectation check_expectation = expectation == BorrowExpectation::BORROW_MUT ?
                BorrowExpectation::CHECK_MUT : BorrowExpectation::CHECK_FREEZED;
            expr->check(sema, check_expectation, 0);
            break;
        }
        default:
            return false;
    }
    return true;
}

BorrowState expected_state(BorrowExpectation expectation) {
    switch (expectation) {
        case BorrowExpectation::CHECK_MUT:
            return BorrowState::MUT;
        case BorrowExpectation::CHECK_FREEZED:
            return BorrowState::FREEZED;
        default:
            assert(false);
    }
}

void EmptyExpr::check(BorrowSema&, BorrowExpectation, size_t) const {}

void BlockExprBase::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    sema.enter_scope();
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    sema.leave_scope();
}

void LiteralExpr::check(BorrowSema&, BorrowExpectation, size_t) const {}
void CharExpr::check(BorrowSema&, BorrowExpectation, size_t) const {}
void StrExpr::check(BorrowSema&, BorrowExpectation, size_t) const {}

void FnExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    fn_check(sema);
}

void PathExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    if (initial_lv_check(this, sema, expectation))
        return;

    BorrowState structural_state = value_decl()->is_mut() ?
        BorrowState::MUT : BorrowState::FREEZED;
    BorrowState exp_state = expected_state(expectation);
    if (DEFAULT_COMPARATOR.compare(structural_state, exp_state) == Relation::LESS)
        error(this) << "state not sufficient\n";
}

void PrefixExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const  {
    switch (kind()) {
        case Token::MUL: // *
            if (initial_lv_check(this, sema, expectation))
                return;
            break;
        case Token::AND: // &
        // TODO: & mut missing
            assert(expectation == BorrowExpectation::ASSIGN_FROM);
            rhs()->check(sema, BorrowExpectation::BORROW_FREEZED, target_scope);
            // TODO: only do this if there were no errors?
            sema.add_borrow(rhs(), BorrowState::FREEZED, target_scope);
            break;
        default:
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    }
}

void InfixExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const   {
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
            target_scope = sema.get_target_scope(lhs());
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
            lhs()->check(sema, BorrowExpectation::ASSIGN_TO, target_scope);
            break;
        default:
            lhs()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    }
}

void PostfixExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    lhs()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void FieldExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(false);
}

void CastExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(false);

    //lhs()->check(sema);
}

void DefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    for (auto arg : args())
        arg->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void RepeatedDefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    value()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void IndefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    dim()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void TupleExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(false);

    //for (auto arg : args())
    //    arg->check(sema);
}

void SimdExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(false);

    //for (auto arg : args())
    //    arg->check(sema);
}

void StructExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    //path()->check(sema); // TODO: check this?
    for (const auto& elem : elems())
        elem.expr()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void MapExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(false); // TODO

    //lhs()->check(sema, assign_to, true, expected_state);
    //for (auto arg : args())
    //    arg->check(sema);
}

void IfExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    cond()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);

    sema.enter_scope();
    BorrowSema else_sema(sema);

    then_expr()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    else_expr()->check(else_sema, BorrowExpectation::ASSIGN_FROM, target_scope);

    sema.leave_scope();
    else_sema.leave_scope();
    sema.merge(else_sema);
}

void WhileExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    //break_decl()->check(sema);
    //continue_decl()->check(sema);

    BorrowSema body_sema(sema);
    body_sema.enter_scope();
    body()->check(body_sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    body_sema.leave_scope();

    sema.merge(body_sema);
    // TODO: can the condition introduce borrows? If so we might need to check it twice
    cond()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
}

void ForExpr::check(BorrowSema& sema, BorrowExpectation expectation, size_t target_scope) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    BorrowSema body_sema(sema);
    fn_expr()->check(body_sema, BorrowExpectation::ASSIGN_FROM, target_scope);

    sema.merge(body_sema);
    expr()->check(sema, BorrowExpectation::ASSIGN_FROM, target_scope);
    //break_decl()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(BorrowSema& sema) const { expr()->check(sema, BorrowExpectation::ASSIGN_FROM, sema.current_scope()); }

void ItemStmt::check(BorrowSema& sema) const { item()->check(sema); }

void LetStmt::check(BorrowSema& sema) const {
    if (init())
        init()->check(sema, BorrowExpectation::ASSIGN_FROM, sema.current_scope());
    local()->check(sema);
}

//------------------------------------------------------------------------------

void borrow_analysis(const ModContents* mod) {
    BorrowSema sema = BorrowSema();
    mod->check(sema);
}

//------------------------------------------------------------------------------

}
