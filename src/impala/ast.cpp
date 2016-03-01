#include "impala/ast.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

const Param* Param::create(size_t var_handle, const Identifier* identifier, const Location& loc, const ASTType* fn_type) {
    auto param = new Param(var_handle);
    param->is_mut_ = false;
    param->identifier_ = identifier;
    param->ast_type_ = fn_type;
    param->set_loc(loc);
    return param;
}

const PrefixExpr* PrefixExpr::create_deref(const AutoPtr<const Expr>& dock) {
    auto deref = new PrefixExpr();
    deref->set_loc(dock->loc());
    deref->kind_ = PrefixExpr::MUL;
    deref->rhs_ = deref;
    swap(deref->rhs_, const_cast<AutoPtr<const Expr>&>(dock));
    return deref;
}

const char* Visibility::str() {
    if (visibility_ == Pub)  return "pub ";
    if (visibility_ == Priv) return "priv ";
    return "";
}

std::string PtrASTType::prefix() const {
    switch (kind()) {
        case Borrowed: return "&";
        case Mut:      return "&mut";
        case Owned:    return "~";
    }
    THORIN_UNREACHABLE;
}

const FnASTType* FnASTType::ret_fn_type() const {
    if (num_args() != 0) {
        if (auto fn_type = args().back()->isa<FnASTType>())
            return fn_type;
    }
    return nullptr;
}

PrimTypeKind LiteralExpr::literal2type() const {
    switch (kind()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return PrimType_##itype;
#include "impala/tokenlist.h"
        case LIT_bool:    return PrimType_bool;
        default: THORIN_UNREACHABLE;
    }
}

uint64_t LiteralExpr::get_u64() const { return thorin::bcast<uint64_t, thorin::Box>(box()); }

bool IfExpr::has_else() const {
    if (auto block = else_expr_->isa<BlockExprBase>())
        return !block->empty();
    return true;
}

//------------------------------------------------------------------------------

/*
 * is_lvalue
 */

bool PathExpr::is_lvalue() const {
    if (value_decl()) {
        value_decl()->write();
        return value_decl()->is_mut();
    }
    return false;
}

bool MapExpr::is_lvalue() const {
    if (!lhs()->type())
        return true; // prevent further errors
    return (lhs()->type().isa<ArrayType>() || lhs()->type().isa<TupleType>() || lhs()->type().isa<PtrType>()) && lhs()->is_lvalue();
}

bool PrefixExpr::is_lvalue() const { return (kind() == MUL || kind() == AND) && rhs()->is_lvalue(); }
bool FieldExpr::is_lvalue() const { return lhs()->is_lvalue(); }
bool CastExpr::is_lvalue() const { return lhs()->is_lvalue(); }

//------------------------------------------------------------------------------

/*
 * has_side_effect
 */

bool PrefixExpr::has_side_effect() const {
    return kind() == INC || kind() == DEC || kind() == TILDE || kind() == RUN || kind() == HLT;
}

bool InfixExpr::has_side_effect() const {
    return Token::is_assign((TokenKind) kind());
}

bool PostfixExpr::has_side_effect() const { return true; }
bool MapExpr::has_side_effect() const { return lhs()->type().isa<FnType>(); }
bool BlockExprBase::has_side_effect() const { return !stmts().empty() || expr()->has_side_effect(); }

bool IfExpr::has_side_effect() const {
    return cond()->has_side_effect() || then_expr()->has_side_effect() || else_expr()->has_side_effect();
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

}
