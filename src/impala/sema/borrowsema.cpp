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

typedef LvBaseMap BorrowMap;
typedef LvBaseMap InitMap;

const LvMapComparator DEFAULT_COMPARATOR = LvMapComparator();

//------------------------------------------------------------------------------

/*
 * BorrowSema
 */

class BorrowSema : public LvMap {
private:
    BorrowSema& operator= (const BorrowSema&);

public:
    BorrowSema();
    ~BorrowSema();

    virtual LvTreeLookupTree lookup(const ValueDecl*) const override;
    virtual void insert(const ValueDecl*, payload_t, const thorin::Location&) override;
    virtual void update(const ValueDecl*, LvTree*) override;

    virtual void add_decl(const ValueDecl*, InitState, const thorin::Location&);
    InitState lookup_init(const Expr*) const;

    virtual void enter_scope() override;
    virtual void leave_scope() override;

    virtual void merge(LvMap& other) override;

    virtual std::ostream& stream(std::ostream&) const override;

private:
    std::vector<BorrowMap> borrow_maps_;
    InitMap init_map_;
    unsigned scope_level_ = 0;
    thorin::HashMap<const ValueDecl*, unsigned> decl_scope_map_;
    std::stack<const ValueDecl*> scope_stack_;
};


BorrowSema::BorrowSema()
    : LvMap(DEFAULT_COMPARATOR)
    , borrow_maps_(std::vector<BorrowMap>())
    , init_map_(InitMap(DEFAULT_COMPARATOR))
    , scope_level_(0)
    , decl_scope_map_(thorin::HashMap<const ValueDecl*, unsigned>())
    , scope_stack_(std::stack<const ValueDecl*>())
    {
        //TODO: add first scope level?
    }

BorrowSema::~BorrowSema() {}

LvTreeLookupTree BorrowSema::lookup(const ValueDecl*) const {
    assert(false);
}

void BorrowSema::insert(const ValueDecl* decl, payload_t pl, const thorin::Location& loc) {
    assert(false);
    // do not use this function
}

void BorrowSeam::add_decl(const ValueDecl* decl, InitState is, const thorin::Location& loc) {
    assert(scope_level_ + 1 == borrow_maps_.size());
    assert(!decl_scope_map_.contains(decl));

    borrow_maps_.back().insert(decl, bs2pl(BorrowState::MUT), loc);
    init_map_.insert(decl, is2pl(is));
    decl_scope_map_[decl] = scope_level_;
    scope_stack_.push(decl);
}

void BorrowSema::update(const ValueDecl* decl, LvTree* tree) {
    assert(scope_level_ + 1 == borrow_maps_.size());
    assert(decl_scope_map_.contains(decl));

    unsigned scope_level = decl_scope_map_[decl];
    assert(scope_level < borrow_maps_.size());
    borrow_maps[scope_level].update(decl, tree);
}

InitState BorrowSema::lookup_init(const Expr* expr) const {
    return pl2is(lookup(expr, init_map_));
}

void BorrowSema::enter_scope() {
    scope_level_++;
    borrow_maps_.push_back(BorrowMap(DEFAULT_COMPARATOR));
    init_map_.enter_scope();
    scope_stack_.push(nullptr);
}

void BorrowSema::leave_scope() {
    assert(scope_level_ > 0);
    assert(!scope_stack_.empty());
    assert(!borrow_maps_.empty());

    scope_level_--;
    borrow_maps_.pop_back();
    init_map_.leave_scope();

    // TODO: same code as in LvBaseMap, can this be deduplicated?
    const ValueDecl* decl;
    do {
        decl = scope_stack_.top();
        scope_stack_.pop();
        if (decl != nullptr) {
            assert(decl_scope_map_.contains(decl);
            decl_scope_map_.erase(decl);
        }
    } while (decl != nullptr);
}

void BorrowSema::merge(LvMap& other) {
    assert(false);
}

std::ostream& BorrowSema::stream(std::ostream&) const {
    assert(false);
}


//------------------------------------------------------------------------------

/*
 * misc
 */

void LocalDecl::check(BorrowSema& sema) const {
    sema.insert(this, bs2pl(BorrowState::MUT), loc());
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
        init()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void Fn::fn_check(BorrowSema& sema) const {
    // TODO: check params
    //for (auto param : params())
    //    param->check(sema);
    if (body() != nullptr)
        body()->check(sema, BorrowExpectation::ASSIGN_FROM);
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

void initial_lv_check(const Expr* expr, BorrowSema& sema, BorrowExpectation expectation) {
    assert(expr->is_lvalue());
   
    Payload pl = lookup_payload(*expr, sema);
    BorrowState borrowed = pl2bs(pl.get_value());
    switch (expectation) {
        case BorrowExpectation::ASSIGN_FROM: {
            Type t = expr->type();
            BorrowState type_state = type_expectation(t);
            if (sema.get_comparator().compare(borrowed, type_state) == Relation::LESS) {
                error(expr) << "cannot use " << expr << " because it is borrowed\n";
                return;
            }
            BorrowExpectation check_expectation = type_state == BorrowState::MUT ?
                BorrowExpectation::CHECK_MUT : BorrowExpectation::CHECK_FREEZED;
            expr->check(sema, check_expectation);
            break;
        }
        case BorrowExpectation::ASSIGN_TO: {
            if (sema.get_comparator().compare(borrowed, BorrowState::MUT) == Relation::LESS) {
                error(expr) << "cannot assign to " << expr << " because it is borrowed\n";
                return;
            }
            if (sema.lookup_init(expr) == InitState::UNINIT)
                return;
            expr->check(sema, BorrowExpectation::CHECK_MUT);
            break;
        }
        case BorrowExpectation::BORROW_MUT:
        case BorrowExpectation::BORROW_FREEZED: {
            BorrowState exp_state = expectation == BorrowExpectation::BORROW_MUT ?
                BorrowState::MUT : BorrowState::FREEZED;
            if (sema.get_comparator().compare(borrowed, exp_state) == Relation::LESS) {
                error(expr) << "cannot borrow " << expr << " because it is already borrowed\n";
                return;
            }
            BorrowExpectation check_expectation = expectation == BorrowExpectation::BORROW_MUT ?
                BorrowExpectation::CHECK_MUT : BorrowExpectation::CHECK_FREEZED;
            expr->check(sema, check_expectation);
            break;
        }
        default:
            assert(false);
    }
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

void EmptyExpr::check(BorrowSema&, BorrowExpectation) const {}

void BlockExprBase::check(BorrowSema& sema, BorrowExpectation expectation) const {
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void LiteralExpr::check(BorrowSema&, BorrowExpectation) const {}
void CharExpr::check(BorrowSema&, BorrowExpectation) const {}
void StrExpr::check(BorrowSema&, BorrowExpectation) const {}

void FnExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    fn_check(sema);
}

void PathExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    switch (expectation) {
        case BorrowExpectation::ASSIGN_FROM:
        case BorrowExpectation::ASSIGN_TO:
        case BorrowExpectation::BORROW_MUT:
        case BorrowExpectation::BORROW_FREEZED:
            initial_lv_check(this, sema, expectation);
            break;
        default:
            BorrowState structural_state = value_decl()->is_mut() ?
                BorrowState::MUT : BorrowState::FREEZED;
            BorrowState exp_state = expected_state(expectation);
            if (sema.get_comparator().compare(structural_state, exp_state) == Relation::LESS)
                error(this) << "state not sufficient\n";
    }
}

void PrefixExpr::check(BorrowSema& sema, BorrowExpectation expectation) const  {
    switch (kind()) {
        case Token::MUL: // *
            break;
        case Token::AND: // &
            assert(rhs()->is_lvalue());
            break;
        // TODO: & mut missing
        default:
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM);
    }
}

void InfixExpr::check(BorrowSema& sema, BorrowExpectation expectation) const   {
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
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM);
            lhs()->check(sema, BorrowExpectation::ASSIGN_TO);
            break;
        default:
            lhs()->check(sema, BorrowExpectation::ASSIGN_FROM);
            rhs()->check(sema, BorrowExpectation::ASSIGN_FROM);
    }
}

void PostfixExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    lhs()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void FieldExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(false);
}

void CastExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(false);

    //lhs()->check(sema);
}

void DefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    for (auto arg : args())
        arg->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void RepeatedDefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    value()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void IndefiniteArrayExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    dim()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void TupleExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(false);

    //for (auto arg : args())
    //    arg->check(sema);
}

void SimdExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(false);

    //for (auto arg : args())
    //    arg->check(sema);
}

void StructExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    //path()->check(sema); // TODO: check this?
    for (const auto& elem : elems())
        elem.expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void MapExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(false); // TODO

    //lhs()->check(sema, assign_to, true, expected_state);
    //for (auto arg : args())
    //    arg->check(sema);
}

void IfExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    cond()->check(sema, BorrowExpectation::ASSIGN_FROM);
    then_expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
    else_expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void WhileExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    cond()->check(sema, BorrowExpectation::ASSIGN_FROM);
    //break_decl()->check(sema);
    //continue_decl()->check(sema);
    body()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

void ForExpr::check(BorrowSema& sema, BorrowExpectation expectation) const {
    assert(expectation == BorrowExpectation::ASSIGN_FROM);

    expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
    //break_decl()->check(sema);
    fn_expr()->check(sema, BorrowExpectation::ASSIGN_FROM);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(BorrowSema& sema) const { expr()->check(sema, BorrowExpectation::ASSIGN_FROM); }

void ItemStmt::check(BorrowSema& sema) const { item()->check(sema); }

void LetStmt::check(BorrowSema& sema) const {
    if (init())
        init()->check(sema, BorrowExpectation::ASSIGN_FROM);
    local()->check(sema);
}

//------------------------------------------------------------------------------

void borrow_analysis(const ModContents* mod) {
    BorrowSema sema = BorrowSema();
    mod->check(sema);
}

//------------------------------------------------------------------------------

}
