#include "impala/ast.h"

using namespace thorin;

namespace impala {

size_t ASTNode::gid_counter_ = 1;

//------------------------------------------------------------------------------

ASTNode::ASTNode(Location location)
    : gid_(gid_counter_++)
    , location_(location)
{}

const char* Visibility::str() {
    if (visibility_ == Pub)  return "pub ";
    if (visibility_ == Priv) return "priv ";
    return "";
}

std::string PtrASTType::prefix() const {
    switch (tag()) {
        case Borrowed: return "&";
        case Mut:      return "&mut";
        case Owned:    return "~";
    }
    THORIN_UNREACHABLE;
}

const FnASTType* FnASTType::ret_fn_ast_type() const {
    if (num_ast_type_args() != 0) {
        if (auto fn_type = ast_type_args().back()->isa<FnASTType>())
            return fn_type;
    }
    return nullptr;
}

PrimTypeTag LiteralExpr::literal2type() const {
    switch (tag()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return PrimType_##itype;
#include "impala/tokenlist.h"
        case LIT_bool:    return PrimType_bool;
        default: THORIN_UNREACHABLE;
    }
}

const Expr* Expr::skip_rvalue() const {
    if (auto rvalue = isa<RValueExpr>()) return rvalue->src();
    return this;
}

uint64_t LiteralExpr::get_u64() const { return thorin::bcast<uint64_t, thorin::Box>(box()); }

bool IfExpr::has_else() const {
    if (auto block = else_expr_->isa<BlockExpr>())
        return !block->empty();
    return true;
}

//------------------------------------------------------------------------------

/*
 * write
 */

void PathExpr::write() const {
    if (value_decl())
        value_decl()->write();
}

void MapExpr::write() const {
    auto type = unpack_ref_type(lhs()->type());
    if (type->isa<ArrayType>() || type->isa<TupleType>() || type->isa<PtrType>())
        lhs()->write();
}

void PrefixExpr::write() const {
    if (tag() == MUT)
        rhs()->write();
}
void FieldExpr::write() const { lhs()->write(); }
void CastExpr::write() const { src()->write(); }

//------------------------------------------------------------------------------

/*
 * has_side_effect
 */

bool RValueExpr::has_side_effect() const {
    return src()->has_side_effect();
}

bool PrefixExpr::has_side_effect() const {
    return tag() == INC || tag() == DEC || tag() == TILDE || tag() == HLT;
}

bool InfixExpr::has_side_effect() const {
    return Token::is_assign((TokenTag) tag());
}

bool PostfixExpr::has_side_effect() const { return true; }
bool MapExpr::has_side_effect() const { return bool(lhs()->type()->isa<FnType>()); }
bool BlockExpr::has_side_effect() const { return !stmts().empty() || expr()->has_side_effect(); }

bool IfExpr::has_side_effect() const {
    return cond()->has_side_effect() || then_expr()->has_side_effect() || else_expr()->has_side_effect();
}

bool MatchExpr::has_side_effect() const {
    return expr()->has_side_effect() ||
        std::any_of(arms().begin(), arms().end(),
            [] (const std::unique_ptr<const Arm>& arm) { return arm->expr()->has_side_effect(); });
}

bool WhileExpr::has_side_effect() const { return true; }
bool ForExpr::has_side_effect() const { return true; }

//------------------------------------------------------------------------------

/*
 * take_address
 */

void PathExpr::take_address() const {
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>())
            local->take_address();
    }
}

void MapExpr::take_address() const { lhs()->take_address(); }
void FieldExpr::take_address() const { lhs()->take_address(); }

//------------------------------------------------------------------------------

/*
 * is_refutable
 */

bool TuplePtrn::is_refutable() const {
    return std::any_of(elems_.begin(), elems_.end(),
        [] (const std::unique_ptr<const Ptrn>& p) { return p->is_refutable(); });
}

bool IdPtrn::is_refutable() const {
    return false;
}

bool EnumPtrn::is_refutable() const {
    return true;
}

bool LiteralPtrn::is_refutable() const {
    return true;
}

//------------------------------------------------------------------------------

const PrefixExpr* replace_rvalue_by_addrof(const RValueExpr* rvalue) {
    auto parent = rvalue->back_ref_;
    parent->release();
    auto src = rvalue->src()->back_ref_->release();
    src->back_ref_ = nullptr;
    auto new_expr = new PrefixExpr(rvalue->location(), PrefixExpr::AND, src);
    delete rvalue;
    parent->reset(new_expr);
    new_expr->back_ref_ = parent;
    return new_expr;
}

const PrefixExpr* PrefixExpr::create_addrof(const Expr* rhs) {
    if (auto rvalue = rhs->isa<RValueExpr>()) {
        return replace_rvalue_by_addrof(rvalue);
    }
    return create(rhs, AND);
}

}
