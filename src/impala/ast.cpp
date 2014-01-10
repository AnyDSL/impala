#include "impala/ast.h"

#include "impala/type.h"

using namespace thorin;

namespace impala {

FnDecl::FnDecl(TypeTable& typetable)
    //: generic_builder_(typetable)
{}

TokenKind Literal::literal2type() const {
    switch (kind()) {
#define IMPALA_LIT(itype, atype) \
        case LIT_##itype: return Token::TYPE_##itype;
#include "impala/tokenlist.h"
        case LIT_bool:    return Token::TYPE_bool;
        default: THORIN_UNREACHABLE;
    }
}

bool Id::is_lvalue() const { 
    assert(decl());
    //if (auto vardecl = decl()->isa<VarDecl>())
        //return vardecl->is_mut();
    return false;
}

uint64_t Literal::get_u64() const { return bcast<uint64_t, Box>(box()); }

bool MapExpr::is_lvalue() const {
    assert(false && "TODO");
    return true;
}

} // namespace impala
