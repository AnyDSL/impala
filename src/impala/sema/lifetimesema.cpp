#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/lvtree.h"

namespace impala {

//------------------------------------------------------------------------------

typedef int lt_t;

class LifetimeSema: public LvMap {
public:
    LifetimeSema(const LvMapComparator& comp): LvMap(comp), cur_lifetime_(1) {}

    virtual void enter_scope() override {
        cur_lifetime_++;
        LvMap::enter_scope();
    }

    virtual void leave_scope() override {
        cur_lifetime_--;
        LvMap::leave_scope();
    }

    lt_t cur_lifetime() { return cur_lifetime_; }

private:
    lt_t cur_lifetime_ = 1;
};

struct AssignmentInfo {
    AssignmentInfo(const Expr* left_expr, lt_t left_lt, const thorin::Location& loc)
        : left_expr_(left_expr)
        , left_lt_(left_lt)
        , asgn_loc_(loc)
        , tree_res_(nullptr)
        {}

    const Expr* left_expr_; // TODO: const ptr?
    const lt_t left_lt_;
    const thorin::Location& asgn_loc_;
    std::shared_ptr<LvTree> tree_res_; // TODO: leave this here?
};

bool build_left_tree(payload_t, const thorin::Location&, const Expr*, LvMap&);

void integrate_lifetime_tree(LvTree*, const Expr*, LvMap&, const thorin::Location&);

//------------------------------------------------------------------------------

/*
 * misc
 */

void TypeParam::check(LifetimeSema& sema) const {
    assert(false);
}

void LocalDecl::check(LifetimeSema& sema) const {
    sema.insert(this, sema.cur_lifetime(), loc());
}

// TODO: we might need to reinclude the ASTTypes once lifetime parameters are supported

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
    // TODO: delete
}

void EnumDecl::check(LifetimeSema&) const {}

void StaticItem::check(LifetimeSema& sema) const {
    if (init())
        // TODO
        init()->check(sema, nullptr);
}

void Fn::fn_check(LifetimeSema& sema) const {
    if (body() != nullptr)
        // TODO
        body()->check(sema, nullptr);
}

void FnDecl::check(LifetimeSema& sema) const {
    fn_check(sema);
}

void StructDecl::check(LifetimeSema& sema) const {
    for (auto field_decl : field_decls()) {
        field_decl->check(sema);
    }
}

void FieldDecl::check(LifetimeSema& sema) const {
    // TODO
}

void TraitDecl::check(LifetimeSema& sema) const {
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
}

void ImplItem::check(LifetimeSema& sema) const {
    for (auto fn : methods())
        fn->check(sema);
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::check(LifetimeSema&, AssignmentInfo*) const {}

void BlockExprBase::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    sema.enter_scope();
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema, asgn_info);
    sema.leave_scope();
}

void LiteralExpr::check(LifetimeSema&, AssignmentInfo*) const {}
void CharExpr::check(LifetimeSema&, AssignmentInfo*) const {}
void StrExpr::check(LifetimeSema&, AssignmentInfo*) const {}

void FnExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    fn_check(sema);
}

void handle_lvs(const Expr* right_expr, LifetimeSema& sema, AssignmentInfo* asgn_info) {
    if (asgn_info == nullptr)
        return;
    assert(asgn_info->tree_res_ == nullptr);

    // TODO: rename build_left_tree
    if (!build_left_tree(asgn_info->left_lt_, asgn_info->asgn_loc_, right_expr, sema))
        return;
    if (asgn_info->left_expr_ == nullptr)
        //asgn_info->tree_res_ = tree_res;
        ;
    else {
        auto tree_res = asgn_info->left_expr_->lookup_lv_tree(sema, true);
        integrate_lifetime_tree(tree_res.tree_.get(), right_expr, sema, asgn_info->asgn_loc_);
    }
}

void PathExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    handle_lvs(this, sema, asgn_info);
}

void PrefixExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const  {
    switch (kind()) {
        case Token::MUL:
            if (asgn_info == nullptr)
                rhs()->check(sema, nullptr);
            else
                handle_lvs(this, sema, asgn_info);
            break;
        case Token::AND: {
            assert(asgn_info != nullptr); // TODO: is this possible?
            assert(asgn_info->tree_res_ == nullptr);
            const Expr* left_expr = asgn_info->left_expr_;
            asgn_info->left_expr_ = nullptr;
            rhs()->check(sema, asgn_info);
            if (asgn_info->tree_res_ != nullptr) {
                if (left_expr == nullptr)
                    assert(false); // TODO
                else {
                    auto tree_res = left_expr->lookup_lv_tree(sema, true);
                    integrate_lifetime_tree(tree_res.tree_.get(), nullptr, sema, asgn_info->asgn_loc_);
                }
            }
            break;
        }
        default:
            rhs()->check(sema, nullptr);
    }
}

void InfixExpr::check(LifetimeSema& sema, AssignmentInfo*) const   {
    switch (kind()) {
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
        case SHR_ASGN: {
            assert(lhs()->is_lvalue());
            lt_t left_lt = lookup(lhs(), sema).get_value();
            auto ai = AssignmentInfo(lhs(), left_lt, loc());
            rhs()->check(sema, &ai);
            // TODO: might need to handle parent assignment
            break;
        }
        default:
            lhs()->check(sema, nullptr);
            rhs()->check(sema, nullptr);
    }
}

void PostfixExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
}

void FieldExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
}

void CastExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
}

void DefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
}

void RepeatedDefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    value()->check(sema, nullptr);
}

void IndefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    dim()->check(sema, nullptr);
}

void TupleExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
}

void SimdExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
}

void StructExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (const auto& elem : elems())
        elem.expr()->check(sema, nullptr);
}

void MapExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
    for (auto arg : args())
        arg->check(sema, nullptr);
}

void IfExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    cond()->check(sema, nullptr);
    then_expr()->check(sema, asgn_info);
    else_expr()->check(sema, asgn_info);
    // TODO: merge semas
}

void WhileExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    cond()->check(sema, nullptr);
    body()->check(sema, asgn_info);
    // TODO: merge semas
}

void ForExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    expr()->check(sema, nullptr);
    fn_expr()->check(sema, nullptr);
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(LifetimeSema& sema) const {
    expr()->check(sema, nullptr);
}

void ItemStmt::check(LifetimeSema& sema) const {
    item()->check(sema);
}

void LetStmt::check(LifetimeSema& sema) const {
    local()->check(sema);
    if (init()) {
        // TODO:
        //auto ai = AssignmentInfo(init(), init()->loc());
        init()->check(sema, nullptr);
    }
}

//------------------------------------------------------------------------------

void lifetime_analysis(const ModContents* mod) {
    LvMapComparator comparator = LvMapComparator();
    LifetimeSema sema = LifetimeSema(comparator);
    mod->check(sema);
}

//------------------------------------------------------------------------------

/*
 * build_left_tree
 */

bool build_left_tree(payload_t base_lt, const thorin::Location& asgn_loc, const Expr* right_expr, LvMap& map) {
    if (!contains_ref_types(right_expr->type()))
        return false;

    LvTreeLookupRes right_res = right_expr->lookup_lv_tree(map, false);
    lt_t right_lt = right_res.payload_.get_value();
    switch (map.get_comparator().compare(base_lt, right_lt)) {
        // either the left side tree has children with larger lifetimes already or it does not
        // in which case it needs to be built up because the right side's reference children
        // need to be appended which have a lifetime as least as long as the right side and
        // thus longer than the left side base
        case Relation::LESS: return true;
        case Relation::EQUAL:
            return right_res.tree_ != nullptr && right_res.tree_->has_children();
        case Relation::GREATER:
            // TODO: use other loc
            error(asgn_loc) << "cannot assign from " << right_expr << " because it does not live long enough\n";
            break;
        case Relation::INCOMPARABLE:
            error(asgn_loc) << "lifetimes are incomparable (TODO)\n";
            assert(false);
    }
    return false;
}

//------------------------------------------------------------------------------

/*
 * integrate_lifetime_tree
 */

void integrate_lifetime_tree(LvTree* left_tree, const Expr* right_expr, LvMap& map, const thorin::Location& loc) {
    if (!contains_ref_types(right_expr->type()))
        return;

    //// TODO: only build trees with true here if the right side has children or the payloads are equal
    //LvTreeLookupRes left_res = left_expr->lookup_lv_tree(map, true);
    //assert(left_res.tree_ != nullptr);
    LvTreeLookupRes right_res = right_expr->lookup_lv_tree(map, false);
    payload_t left_pl = left_tree->get_payload().get_value(); // TODO: might go wrong

    switch (map.get_comparator().compare(left_pl, right_res.payload_.get_value())) {
        case Relation::EQUAL:
            // TODO: handle multi_ref
            if (right_res.tree_ != nullptr && right_res.tree_->has_children())
                left_tree->copy_subtrees(right_res.tree_.get());
            else
                //insert(left_expr, map, left_res.payload_.get_value(), left_res.payload_.loc());
                assert(false);
                // TODO
            break;
        case Relation::GREATER:
            // TODO: handle multi_ref
            right_expr->type()->integrate_lifetime_tree(left_tree,
                left_pl, right_res.tree_.get(), right_res.payload_.get_value(),
                loc, map.get_comparator());
            break;
        case Relation::LESS:
        default:
            assert(false);
    }
}

void StructAppTypeNode::integrate_lifetime_tree(LvTree* target, payload_t target_pl, LvTree* source, payload_t source_pl, const thorin::Location& loc, const LvMapComparator& comp) const {
    // TODO: assert refernced type contains ref || pl relation
    
    StructAbsType t = struct_abs_type();
    const StructDecl *decl = t->struct_decl();
    for (auto fd : decl->field_decls()) {
        Type ft = fd->type();
        if (!contains_ref_types(ft))
            continue;
        Symbol fs = fd->symbol();
        // TODO: multi ref
        // TODO: assert that pl does not change
        ft->integrate_lifetime_tree(target->get_child(fs, true).get(), target_pl,
            source == nullptr ? nullptr : source->get_child(fs, false).get(), source_pl,
            loc, comp);
    }
}

payload_t get_child_payload(LvTree* tree, payload_t default_pl) {
    payload_t child_pl = default_pl;
    auto child = tree->get_child(get_deref_symbol(), false);
    if (child != nullptr && !child->get_payload().is_inherited())
        child_pl = child->get_payload().get_value();
    return child_pl;
}

void PtrTypeNode::integrate_lifetime_tree(LvTree* target, payload_t target_pl, LvTree* source,
    payload_t source_pl, const thorin::Location& loc, const LvMapComparator& comp) const {
    // TODO: assert refernced type contains ref || pl relation
    switch (kind()) {
        case Kind_borrowed_ptr:
        case Kind_mut_ptr: {
            if (source != nullptr)
                // TODO: multi ref
                target->copy_subtrees(source);

            payload_t source_child_pl = get_child_payload(source, source_pl);
            Relation compare_res = comp.compare(target_pl, source_child_pl);
            switch (compare_res) {
                case Relation::EQUAL:
                    if (target->has_children())
                        target->get_child(get_deref_symbol(), false)->set_payload_inherited(loc);
                    break;
                case Relation::LESS:
                    if (target->has_children())
                        target->get_child(get_deref_symbol(), false)->set_payload(source_child_pl, loc);
                case Relation::GREATER:
                    error(loc) << "Cannot assign because the source of the assignment does not live long enough\n";
                    // TODO: this should be in the lifetimesema
                    return;
                default:    
                    assert(false);
            }
            break;
        }
        case Kind_owned_ptr:
            // TODO: assert inherited
            referenced_type()->integrate_lifetime_tree(target->get_child(get_deref_symbol(), true).get(),
                target_pl, source == nullptr ? nullptr :
                source->get_child(get_deref_symbol(), false).get(), source_pl, loc, comp);
            break;
        default:
            THORIN_UNREACHABLE;
    }
}

//--------------------------------------------------------------------

}
