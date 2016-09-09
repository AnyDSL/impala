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
    AssignmentInfo(lt_t left_lt, const thorin::Location& loc)
        : left_lt_(left_lt)
        , asgn_loc_(loc)
        {}

    const lt_t left_lt_;
    const thorin::Location& asgn_loc_;
};

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

std::shared_ptr<LvTree> EmptyExpr::check(LifetimeSema&, AssignmentInfo*) const {return nullptr;}

std::shared_ptr<LvTree> BlockExprBase::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    sema.enter_scope();
    for (auto stmt : stmts())
        stmt->check(sema);
    auto res = expr()->check(sema, asgn_info);
    sema.leave_scope();
    return res;
}

std::shared_ptr<LvTree> LiteralExpr::check(LifetimeSema&, AssignmentInfo*) const {return nullptr;}
std::shared_ptr<LvTree> CharExpr::check(LifetimeSema&, AssignmentInfo*) const {return nullptr;}
std::shared_ptr<LvTree> StrExpr::check(LifetimeSema&, AssignmentInfo*) const {return nullptr;}

std::shared_ptr<LvTree> FnExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    fn_check(sema);
    return nullptr;
}

std::shared_ptr<LvTree> handle_lv(const Expr* right_expr, LifetimeSema& sema, AssignmentInfo* asgn_info) {
    if (asgn_info == nullptr)
        // TODO: should this be possible, would it be better to make asgn_info a reference?
        return nullptr;
    assert(contains_ref_types(right_expr->type()));

    auto right_tree = right_expr->lookup_lv_tree(sema, false);
    // TODO: what if no tree is returned?
    lt_t right_lt = right_tree.payload_.get_value();
    // TODO: have to make sure that get_value() works

    // TODO: explain this behavior
    switch (sema.get_comparator().compare(asgn_info->left_lt_, right_lt)) {
        case Relation::INCOMPARABLE:
            // TODO
            assert(false);
        case Relation::GREATER:
            error(asgn_info->asgn_loc_) << "cannot make assignment because the right side does not "
                << "live long enough\n";
            // TODO: better error message
            break;
        case Relation::EQUAL:
            if (right_tree.tree_ != nullptr && right_tree.tree_->has_children())
                // TODO: right thing to check here? want to check that there are children for this lv
                // that will have larger payloads
                return right_tree.tree_;
            break;
        case Relation::LESS: {
            return right_tree.tree_ == nullptr ?
                right_expr->lookup_lv_tree(sema, true).tree_ : right_tree.tree_;
        }
    }
    return nullptr;
}

std::shared_ptr<LvTree> PathExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    return handle_lv(this, sema, asgn_info);
}

std::shared_ptr<LvTree> PrefixExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const  {
    switch (kind()) {
        case Token::MUL:
            // TODO: ok like that, always check rhs?
            if (asgn_info == nullptr)
                rhs()->check(sema, nullptr);
            else
                return handle_lv(this, sema, asgn_info);
            break;
        case Token::AND: {
            assert(asgn_info != nullptr); // TODO: is this possible?
            auto res = rhs()->check(sema, asgn_info);
            if (false && res != nullptr) {
                // TODO: reuse functionality from lvmap.cpp?
                auto new_tree = std::shared_ptr<LvTree>(new LvTree(nullptr));
                new_tree->set_payload_inherited(asgn_info->asgn_loc_);
                new_tree->add_child(get_deref_symbol(), res);
                return new_tree;
            }
            break;
        }
        default:
            rhs()->check(sema, nullptr);
    }
    return nullptr;
}

std::shared_ptr<LvTree> InfixExpr::check(LifetimeSema& sema, AssignmentInfo*) const   {
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
            if (!contains_ref_types(rhs()->type()))
                // TODO: should we really stop here, couldn't the arguments do something with references
                // even if they are not assigned? do we need to check that?
                // TODO: the idea is that we only need to check assignments in this phase,
                // is that correct? the correctness of the rest should follow
                // nothing to do
                return nullptr;

            lt_t left_lt = lookup(lhs(), sema).get_value();
            auto ai = AssignmentInfo(left_lt, loc());
            auto res = rhs()->check(sema, &ai);
            if (res != nullptr)
                // TODO: should we really pass a pointer to integrate... or better a shared pointer?
                ;//integrate_lifetime_tree(ai.tree_res_.get(), rhs(), sema, loc());
                // TODO: change integrate
            // TODO: might need to handle parent assignment
            break;
        }
        default:
            // TODO: can we stop here?
            lhs()->check(sema, nullptr);
            rhs()->check(sema, nullptr);
    }
    return nullptr;
}

std::shared_ptr<LvTree> PostfixExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> FieldExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> CastExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> DefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> RepeatedDefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    value()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> IndefiniteArrayExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    dim()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> TupleExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> SimdExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (auto arg : args())
        arg->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> StructExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    for (const auto& elem : elems())
        elem.expr()->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> MapExpr::check(LifetimeSema& sema, AssignmentInfo*) const {
    lhs()->check(sema, nullptr);
    for (auto arg : args())
        arg->check(sema, nullptr);
    return nullptr;
}

std::shared_ptr<LvTree> IfExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    cond()->check(sema, nullptr);
    then_expr()->check(sema, asgn_info);
    else_expr()->check(sema, asgn_info);
    // TODO: merge semas
    return nullptr;
}

std::shared_ptr<LvTree> WhileExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    cond()->check(sema, nullptr);
    body()->check(sema, asgn_info);
    // TODO: merge semas
    return nullptr;
}

std::shared_ptr<LvTree> ForExpr::check(LifetimeSema& sema, AssignmentInfo* asgn_info) const {
    expr()->check(sema, nullptr);
    fn_expr()->check(sema, nullptr);
    return nullptr;
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
        // TODO: handle the same way as the assignment
        // TODO: need a different location, the one of the assignment operator
        auto ai = AssignmentInfo(sema.cur_lifetime(), init()->loc());
        init()->check(sema, &ai);
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
