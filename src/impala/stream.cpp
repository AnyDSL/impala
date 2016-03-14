#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

using namespace thorin;

Prec prec = BOTTOM;

/*
 * AST types
 */

std::ostream& ErrorASTType::stream(std::ostream& os) const { return os << "<error>"; }

std::ostream& PtrASTType::stream(std::ostream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << referenced_ast_type();
}

std::ostream& DefiniteArrayASTType::stream(std::ostream& os) const { return streamf(os, "[% * %]", elem_ast_type(), dim()); }
std::ostream& IndefiniteArrayASTType::stream(std::ostream& os) const { return streamf(os, "[%]", elem_ast_type()); }
std::ostream& SimdASTType::stream(std::ostream& os) const { return streamf(os, "simd[% * %]", elem_ast_type(), size()); }

std::ostream& TupleASTType::stream(std::ostream& os) const {
    return stream_list(os, args(), [&](const ASTType* type) { os << type; }, "(", ")");
}

std::ostream& FnASTType::stream(std::ostream& os) const {
    auto ret = ret_fn_type();
    stream_ast_type_params(os << "fn");
    stream_list(os, ret != nullptr ? args().skip_back() : args(), [&](const ASTType* arg) { os << arg; }, "(", ")");
    if (ret != nullptr) {
        os << " -> ";
        if (ret->num_args() == 1)
            os << ret->args().front();
        else
            stream_list(os, ret->args(), [&](const ASTType* arg) { os << arg; }, "(", ")");
    }
    return os;
}

std::ostream& ASTTypeApp::stream(std::ostream& os) const {
    os << symbol();
    if (num_args() != 0)
        stream_list(os, args(), [&](const ASTType* arg) { os << arg; }, "[", "]");
    return os;
}

std::ostream& PrimASTType::stream(std::ostream& os) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& Typeof::stream(std::ostream& os) const {
    return streamf(os, "typeof(%)", expr());
}

/*
 * paths
 */

std::ostream& Identifier::stream(std::ostream& os) const { return os << symbol(); }
std::ostream& PathElem::stream(std::ostream& os) const { return os << symbol(); }
std::ostream& Path::stream(std::ostream& os) const {
    os << (is_global() ? "::" : "");
    return stream_list(os, path_elems(), [&](const PathElem* path_elem) { os << path_elem; }, "", "", "::");
}

/*
 * parameters
 */

std::ostream& ASTTypeParam::stream(std::ostream& os) const {
    os << symbol() << (bounds_.empty() ? "" : ": ");
    return stream_list(os, bounds(), [&](const ASTType* type) { os << type; }, "", "", " + ");
}

std::ostream& ASTTypeParamList::stream_ast_type_params(std::ostream& os) const {
    if (!ast_type_params().empty())
        stream_list(os, ast_type_params(), [&](const ASTTypeParam* ast_type_param) { os << ast_type_param; }, "[", "]");
    return os;
}

/*
 * other decls
 */

std::ostream& Fn::stream_params(std::ostream& os, bool returning) const {
    return stream_list(os, returning ? params().skip_back() : params(), [&](const Param* param) {
        if (!param->is_anonymous())
            os << (param->is_mut() ? "mut " : "") << param->symbol() <<
                ((param->ast_type() || param->type()) ? ": " : "");
        if (auto type = param->type())
            os << type;
        else if (auto ast_type = param->ast_type())
            os << ast_type;
    });

}

std::ostream& ValueDecl::stream(std::ostream& os) const {
    os << (is_mut() ? "mut " : "" );
    if (!is_anonymous()) {
        os << symbol();
        if (type())
            os << ": " << type();
        else if (ast_type())
            os << ": " << ast_type();
    }

    return os;
}

/*
 * items + item helpers
 */

std::ostream& ModContents::stream(std::ostream& os) const {
    return stream_list(os, items(), [&](const Item* item) { os << item << endl; }, "", "", "", true);
}

std::ostream& ModDecl::stream(std::ostream& os) const {
    stream_ast_type_params(os << "mod " << symbol());
    if (mod_contents()) {
        return os << " {" << up << endl << mod_contents() << down << endl << "}";
    } else
        return os << ';';
}

std::ostream& ExternBlock::stream(std::ostream& os) const {
    os << "extern ";
    if (!abi_.empty())
        os << abi_.str() << ' ';
    os << '{' << up << endl;
    stream_list(os, fns(), [&](const FnDecl* fn) { os << fn; }, "", "", "", true);
    return os << down << endl << '}';
}

std::ostream& FnDecl::stream(std::ostream& os) const {
    if (is_extern())
        os << "extern ";
    os << "fn ";
    if (export_name_)
        os << export_name_->symbol() << ' ';
    stream_ast_type_params(os << symbol());

    const FnASTType* ret = nullptr;
    if (!params().empty() && params().back()->symbol() == "return") {
        if (auto fn_type = params().back()->ast_type()->isa<FnASTType>())
            ret = fn_type;
    }

    stream_params(os << '(', ret != nullptr) << ")";

    if (ret) {
        os << " -> ";
        if (ret->num_args() == 1)
            os << ret->arg(0);
        else
            stream_list(os, ret->args(), [&](const ASTType* type) { os << type; }, "(", ")", ", ");
    }

    if (body()) {
        os << ' ' << body();
    } else
        os << ';';

    return os;
}

std::ostream& FieldDecl::stream(std::ostream& os) const {
    return streamf(os, "%%: %", visibility().str(), symbol(), ast_type());
}

std::ostream& StaticItem::stream(std::ostream& os) const {
    ValueDecl::stream(os << "static ");
    if (init())
        os << " = " << init();
    return os << ";";
}

std::ostream& StructDecl::stream(std::ostream& os) const {
    stream_ast_type_params(streamf(os, "%struct %", visibility().str(), symbol())) << " {" << up << endl;
    return stream_list(os, field_decls(), [&](const FieldDecl* field) { os << field; }, "", "", ",", true) << down << endl << "}";
}

std::ostream& Typedef::stream(std::ostream& os) const {
    return stream_ast_type_params(streamf(os, "%type %", visibility().str(), symbol())) << " = " << ast_type() << ';';
}

std::ostream& TraitDecl::stream(std::ostream& os) const {
    os << "trait " << symbol();
    stream_ast_type_params(os);

    if (!super_traits().empty()) {
        os << " : ";
        stream_list(os, super_traits(), [&](const ASTTypeApp* type_app) { os << type_app; });
    }

    os << " {" << up << endl;
    stream_list(os, methods(), [&](const FnDecl* method) { os << method; }, "", "", "", true);
    return os << down << endl << '}';
}

std::ostream& ImplItem::stream(std::ostream& os) const {
    os << "impl";
    stream_ast_type_params(os) << ' ';
    if (trait())
        os << trait() << " for ";
    os << ast_type() << " {" << up << endl;
    stream_list(os, methods(), [&](const FnDecl* method) { os << method; }, "", "", "", true);
    return os << down << endl << "}";
}

/*
 * expressions
 */

std::ostream& BlockExprBase::stream(std::ostream& os) const {
    os << prefix();
    if (empty())
        return os << endl << '}';

    stream_list(os << up << endl, stmts(), [&](const Stmt* stmt) { os << stmt; }, "", "", "", true);

    if (!expr()->isa<EmptyExpr>()) {
        if (!stmts().empty())
            os << endl;
        os << expr();
    }

    return os << down << endl << "}";
}

std::ostream& LiteralExpr::stream(std::ostream& os) const {
    switch (kind()) {
        case LIT_i8:  return os << box().get_s8()  << "i8";
        case LIT_i16: return os << box().get_s16() << "i16";
        case LIT_i32: return os << box().get_s32();
        case LIT_i64: return os << box().get_s64() << "i64";
        case LIT_u8:  return os << box().get_s8()  << "u8";
        case LIT_u16: return os << box().get_s16() << "u16";
        case LIT_u32: return os << box().get_s32() << "u";
        case LIT_u64: return os << box().get_s64() << "u64";
        case LIT_f32: return os << box().get_f32() << "f";
        case LIT_f64: return os << box().get_f64() << "f64";
        case LIT_bool: return os << (box().get_bool() ? "true" : "false");
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& CharExpr::stream(std::ostream& os) const {
    return os << symbol();
}

std::ostream& StrExpr::stream(std::ostream& os) const {
    if (symbols().size() == 1)
        return os << '\'' << symbols().front().remove_quotation() << '\'';
    stream_list(os << up << endl, symbols() , [&](Symbol symbol) { os << symbol; }, "", "", "", true);
    return os << down << endl;
}

std::ostream& PathExpr ::stream(std::ostream& os) const { return os << path(); }
std::ostream& EmptyExpr::stream(std::ostream& os) const { return os << "/*empty*/"; }
std::ostream& TupleExpr::stream(std::ostream& os) const {
    return stream_list(os, args(), [&](const Expr* expr) { os << expr; }, "(", ")");
}

std::ostream& DefiniteArrayExpr::stream(std::ostream& os) const {
    return stream_list(os, args(), [&](const Expr* expr) { os << expr; }, "[", "]");
}

std::ostream& RepeatedDefiniteArrayExpr::stream(std::ostream& os) const {
    return streamf(os, "[%, .. %]", value(), count());
}

std::ostream& IndefiniteArrayExpr::stream(std::ostream& os) const {
    return streamf(os, "[%: %]", dim(), elem_ast_type());
}

std::ostream& SimdExpr::stream(std::ostream& os) const {
    return stream_list(os, args(), [&](const Expr* expr) { os << expr; }, "simd[", "]");
}

std::ostream& PrefixExpr::stream(std::ostream& os) const {
    Prec r = PrecTable::prefix_r[kind()];
    Prec old = prec;
    bool paren = !fancy() || prec <= r;
    if (paren) os << "(";

    const char* op;
    switch (kind()) {
#define IMPALA_PREFIX(tok, str, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }

    os << op;
    prec = r;
    os << rhs();
    prec = old;

    if (paren) os << ")";
    return os;
}

static std::pair<Prec, bool> open(std::ostream& os, int kind) {
    std::pair<Prec, bool> result;
    Prec l = PrecTable::postfix_l[kind];
    result.first = prec;
    result.second = !fancy() || prec > l;
    if (result.second)
        os << "(";
    prec = l;
    return result;
}

static std::ostream& close(std::ostream& os, std::pair<Prec, bool> pair) {
    prec = pair.first;
    if (pair.second)
        os << ")";
    return os;
}

std::ostream& InfixExpr::stream(std::ostream& os) const {
    auto open_state = open(os, kind());
    const char* op;
    switch (kind()) {
#define IMPALA_INFIX_ASGN(tok, str, lprec, rprec) case tok: op = str; break;
#define IMPALA_INFIX(     tok, str, lprec, rprec) case tok: op = str; break;
#include "impala/tokenlist.h"
    }

    os << lhs() << " " << op << " ";
    prec = PrecTable::infix_r[kind()];
    os << rhs();
    return close(os, open_state);
}

std::ostream& PostfixExpr::stream(std::ostream& os) const {
    auto open_state = open(os, kind());
    const char* op;
    switch (kind()) {
        case INC: op = "++"; break;
        case DEC: op = "--"; break;
        default: THORIN_UNREACHABLE;
    }

    os << lhs() << op;
    return close(os, open_state);
}

std::ostream& FieldExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Token::DOT);
    os << lhs() << '.' << symbol();
    return close(os, open_state);
}

std::ostream& CastExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Token::AS);
    streamf(os, "% as %", lhs(), ast_type());
    return close(os, open_state);
}

std::ostream& TypeArgs::stream_ast_type_args(std::ostream& os) const {
    if (num_ast_type_args() != 0)
        return stream_list(os, ast_type_args(), [&](const ASTType* ast_type) { os << ast_type; }, "[", "]");
    return os;
}

std::ostream& TypeArgs::stream_type_args(std::ostream& os) const {
    if (num_type_args() != 0)
        return stream_list(os, type_args(), [&](const Type* type) { os << type; }, "[", "]", ", ", false);
    return os;
}

std::ostream& StructExpr::stream(std::ostream& os) const {
    path()->stream(os);
    if (num_type_args() == 0)
        stream_ast_type_args(os);
    else
        stream_type_args(os);
    return stream_list(os, elems(), [&](const Elem& elem) { os << elem.symbol() << ": " << elem.expr(); }, "{", "}");
}

std::ostream& MapExpr::stream(std::ostream& os) const {
    Prec l = PrecTable::postfix_l[Token::L_PAREN];
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) os << "(";

    prec = l;
    os << lhs();
    if (num_type_args() == 0)
        stream_ast_type_args(os);
    else
        stream_type_args(os);
    stream_list(os, args(), [&](const Expr* expr) { os << expr; }, "(", ")");
    prec = old;
    if (paren) os << ")";
    return os;
}

std::ostream& FnExpr::stream(std::ostream& os) const {
    bool has_return_type = !params().empty() && params().back()->symbol() == "return";
    os << '|';
    stream_params(os, has_return_type);
    os << "| ";

    if (has_return_type) {
        os << "-> ";
        auto ret = params().back();
        if (ret->type()) {
            auto rettype = ret->type()->as<FnType>();
            if (rettype->size() == 1)
                os << rettype->arg(0);
            else
                stream_list(os, rettype->args(), [&](const Type* type) { os << type; }, "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->num_args() == 1)
                os << rettype->arg(0);
            else
                stream_list(os, rettype->args(), [&](const ASTType* type) { os << type; }, "(", ")", ", ");
        }
        os << " ";
    }

    return os << body();
}

std::ostream& IfExpr::stream(std::ostream& os) const {
    streamf(os, "if % %", cond(), then_expr());
    if (has_else())
        os << " else " << else_expr();
    return os;
}

std::ostream& WhileExpr::stream(std::ostream& os) const {
    return streamf(os, "while % %", cond(), body());
}

std::ostream& ForExpr::stream(std::ostream& os) const {
    stream_list(os << "for ", fn_expr()->params().skip_back(), [&](const Param* param) { os << param; }) << " in ";
    return os << expr() << ' ' << fn_expr()->body();
}

/*
 * statements
 */

std::ostream& ItemStmt::stream(std::ostream& os) const { return os << item(); }

std::ostream& LetStmt::stream(std::ostream& os) const {
    os << "let " << local();
    if (init())
        os << " = " << init();
    return os << ';';
}

std::ostream& ExprStmt::stream(std::ostream& os) const {
    bool no_semi = expr()->isa<IfExpr>() || expr()->isa<ForExpr>();
    os << expr();
    if (!no_semi)
        os << ';';
    return os;
}

}
