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

bool Call::is_continuation_call() const { return type()->isa<NoRet>() != nullptr; }

bool Id::is_lvalue() const { 
    assert(decl());
    //if (auto vardecl = decl()->isa<VarDecl>())
        //return vardecl->is_mut();
    return false;
}

Location Call::args_location() const {
    if (ops().size() == 1)
        return Location(pos2());
    return Location(op(1)->pos1(), ops_.back()->pos2());
}

uint64_t Literal::get_u64() const { return bcast<uint64_t, Box>(box()); }

} // namespace impala
