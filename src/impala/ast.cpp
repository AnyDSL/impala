#include "impala/ast.h"

using namespace thorin;

namespace impala {

const char* Visibility::str() {
    if (visibility_ == Pub)  return "pub ";
    if (visibility_ == Priv) return "priv ";
    return "";
}

const FnType* FnType::ret_fn_type() const { 
    if (!elems().empty()) {
        if (auto fn_type = elems().back()->isa<FnType>())
            return fn_type;
    }
    return nullptr;
}

TokenKind LiteralExpr::literal2type() const {
    switch (kind()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return Token::TYPE_##itype;
#include "impala/tokenlist.h"
        case LIT_bool:    return Token::TYPE_bool;
        default: THORIN_UNREACHABLE;
    }
}

bool IdExpr::is_lvalue() const { 
    assert(decl());
    //if (auto vardecl = decl()->isa<VarDecl>())
        //return vardecl->is_mut();
    return false;
}

uint64_t LiteralExpr::get_u64() const { return bcast<uint64_t, Box>(box()); }

bool MapExpr::is_lvalue() const {
    assert(false && "TODO");
    return true;
}

bool IfExpr::has_else() const {
    if (auto block = else_expr_->isa<BlockExpr>())
        return !block->empty();
    return true;
}

}
