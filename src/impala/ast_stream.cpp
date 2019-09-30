#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

using namespace thorin;

Prec prec = Prec::Bottom;

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

std::ostream& DefiniteArrayASTType::stream(std::ostream& os) const { return streamf(os, "[{} * {}]", elem_ast_type(), dim()); }
std::ostream& IndefiniteArrayASTType::stream(std::ostream& os) const { return streamf(os, "[{}]", elem_ast_type()); }

std::ostream& TupleASTType::stream(std::ostream& os) const {
    return stream_list(os, ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
}

std::ostream& FnASTType::stream(std::ostream& os) const {
    auto ret = ret_fn_ast_type();
    stream_ast_type_params(os << "fn");
    stream_list(os, ret != nullptr ? ast_type_args().skip_back() : ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
    if (ret != nullptr) {
        os << " -> ";
        if (ret->num_ast_type_args() == 1)
            os << ret->ast_type_args().front().get();
        else
            stream_list(os, ret->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
    }
    return os;
}

std::ostream& ASTTypeApp::stream(std::ostream& os) const {
    os << symbol();
    if (num_ast_type_args() != 0)
        stream_list(os, ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "[", "]");
    return os;
}

std::ostream& PrimASTType::stream(std::ostream& os) const {
    switch (tag()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& Typeof::stream(std::ostream& os) const {
    return streamf(os, "typeof({})", expr());
}

/*
 * paths
 */

std::ostream& Identifier::stream(std::ostream& os) const { return os << symbol(); }
std::ostream& Path::Elem::stream(std::ostream& os) const { return os << symbol(); }
std::ostream& Path::stream(std::ostream& os) const {
    os << (is_global() ? "::" : "");
    return stream_list(os, elems(), [&](const auto& elem) { os << elem.get(); }, "", "", "::");
}

/*
 * parameters
 */

std::ostream& ASTTypeParam::stream(std::ostream& os) const {
    os << symbol() << (bounds_.empty() ? "" : ": ");
    return stream_list(os, bounds(), [&](const auto& type) { os << type.get(); }, "", "", " + ");
}

std::ostream& ASTTypeParamList::stream_ast_type_params(std::ostream& os) const {
    if (!ast_type_params().empty())
        stream_list(os, ast_type_params(), [&](const auto& ast_type_param) { os << ast_type_param.get(); }, "[", "]");
    return os;
}

/*
 * other decls
 */

std::ostream& Fn::stream_params(std::ostream& os, bool returning) const {
    return stream_list(os, returning ? params().skip_back() : params(), [&](const auto& param) { param->stream(os); });
}

std::ostream& LocalDecl::stream(std::ostream& os) const {
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

std::ostream& Param::stream(std::ostream& os) const {
    if (!is_anonymous())
        os << (is_mut() ? "mut " : "") << symbol() <<
            ((ast_type() || type()) ? ": " : "");

    if (type())
        os << type();
    else if (ast_type())
        os << ast_type();

    return os;
}

/*
 * items + item helpers
 */

std::ostream& Module::stream(std::ostream& os) const {
    return stream_list(os, items(), [&](const auto& item) { os << item.get() << endl; }, "", "", "", true);
}

std::ostream& ModuleDecl::stream(std::ostream& os) const {
    return stream_ast_type_params(os << "mod " << symbol()) << ';';
}

std::ostream& ExternBlock::stream(std::ostream& os) const {
    os << "extern ";
    if (!abi_.empty())
        os << abi_.str() << ' ';
    os << '{' << up << endl;
    stream_list(os, fn_decls(), [&](const auto& fn_decl) { os << fn_decl.get(); }, "", "", "", true);
    return os << down << endl << '}';
}

std::ostream& FnDecl::stream(std::ostream& os) const {
    if (is_extern())
        os << "extern ";
    os << "fn ";
    if (filter())
        os << '@' << filter() << ' ';
    if (export_name_)
        os << export_name_ << ' ';
    stream_ast_type_params(os << symbol());

    const FnASTType* ret = nullptr;
    if (!params().empty() && params().back()->symbol() == "return" && params().back()->ast_type()) {
        if (auto fn_type = params().back()->ast_type()->isa<FnASTType>())
            ret = fn_type;
    }

    stream_params(os << '(', ret != nullptr) << ")";

    if (ret) {
        os << " -> ";
        if (ret->num_ast_type_args() == 1)
            os << ret->ast_type_arg(0);
        else
            stream_list(os, ret->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")", ", ");
    }

    if (body()) {
        os << ' ' << body();
    } else
        os << ';';

    return os;
}

std::ostream& FieldDecl::stream(std::ostream& os) const {
    return streamf(os, "{}{}: {}", visibility().str(), symbol(), ast_type());
}

std::ostream& OptionDecl::stream(std::ostream& os) const {
    streamf(os, "{}", symbol());
    if (num_args() > 0) {
        return stream_list(os, args(), [&](const auto& arg) { os << arg.get(); }, "(", ")", ", ");
    } else {
        return os;
    }
}

std::ostream& StaticItem::stream(std::ostream& os) const {
    streamf(os, "static {} {}: {}", is_mut() ? "mut " : "", identifier(), type() ? type()->to_string() : ast_type()->to_string());
    if (init())
        streamf(os, " = {}", init());
    return os << ";";
}

std::ostream& StructDecl::stream(std::ostream& os) const {
    stream_ast_type_params(streamf(os, "{}struct {}", visibility().str(), symbol())) << " {" << up << endl;
    return stream_list(os, field_decls(), [&](const auto& field) { os << field.get(); }, "", "", ",", true) << down << endl << "}";
}

std::ostream& EnumDecl::stream(std::ostream& os) const {
    stream_ast_type_params(streamf(os, "{}enum {}", visibility().str(), symbol())) << " {" << up << endl;
    return stream_list(os, option_decls(), [&](const auto& option) { os << option.get(); }, "", "", ",", true) << down << endl << "}";
}

std::ostream& Typedef::stream(std::ostream& os) const {
    return stream_ast_type_params(streamf(os, "{}type {}", visibility().str(), symbol())) << " = " << ast_type() << ';';
}

std::ostream& TraitDecl::stream(std::ostream& os) const {
    os << "trait " << symbol();
    stream_ast_type_params(os);

    if (!super_traits().empty()) {
        os << " : ";
        stream_list(os, super_traits(), [&](const auto& type_app) { os << type_app.get(); });
    }

    os << " {" << up << endl;
    stream_list(os, methods(), [&](const auto& method) { os << method.get(); }, "", "", "", true);
    return os << down << endl << '}';
}

std::ostream& ImplItem::stream(std::ostream& os) const {
    os << "impl";
    stream_ast_type_params(os) << ' ';
    if (trait())
        os << trait() << " for ";
    os << ast_type() << " {" << up << endl;
    stream_list(os, methods(), [&](const auto& method) { os << method.get(); }, "", "", "", true);
    return os << down << endl << "}";
}

/*
 * expressions
 */

std::ostream& BlockExpr::stream(std::ostream& os) const {
    os << '{';
    if (empty())
        return os << endl << '}';

    stream_list(os << up << endl, stmts(), [&](const auto& stmt) { os << stmt.get(); }, "", "", "", true);

    if (!expr()->isa<EmptyExpr>()) {
        if (!stmts().empty())
            os << endl;
        os << expr();
    }

    return os << down << endl << "}";
}

std::ostream& LiteralExpr::stream(std::ostream& os) const {
    switch (tag()) {
        case LIT_bool: return os << (get<bool>() ? "true" : "false");
        case LIT_i8:   return os << (int)get< s8>() << "i8";
        case LIT_i16:  return os <<      get<s16>() << "i16";
        case LIT_i32:  return os <<      get<s32>();
        case LIT_i64:  return os <<      get<s64>() << "i64";
        case LIT_u8:   return os << (int)get< u8>() << "u8";
        case LIT_u16:  return os <<      get<u16>() << "u16";
        case LIT_u32:  return os <<      get<u32>() << "u";
        case LIT_u64:  return os <<      get<u64>() << "u64";
        case LIT_f16:  return os <<      get<r16>() << "h";
        case LIT_f32:  return os <<      get<r32>() << "f";
        case LIT_f64:  return os <<      get<r64>() << "f64";
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
    return stream_list(os, args(), [&](const auto& expr) { os << expr.get(); }, "(", ")");
}

std::ostream& DefiniteArrayExpr::stream(std::ostream& os) const {
    return stream_list(os, args(), [&](const auto& expr) { os << expr.get(); }, "[", "]");
}

std::ostream& RepeatedDefiniteArrayExpr::stream(std::ostream& os) const {
    return streamf(os, "[{}, .. {}]", value(), count());
}

std::ostream& IndefiniteArrayExpr::stream(std::ostream& os) const {
    return streamf(os, "[{}: {}]", dim(), elem_ast_type());
}

static std::pair<Prec, bool> open(std::ostream& os, Prec l) {
    std::pair<Prec, bool> result;
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

std::ostream& PrefixExpr::stream(std::ostream& os) const {
    const char* op;
    switch (tag()) {
#define IMPALA_PREFIX(tok, str) case tok: op = str; break;
#include "impala/tokenlist.h"
        case MUT: op = "&mut "; break;
        default: THORIN_UNREACHABLE;
    }

    os << op;
    if (auto prefix_expr = rhs()->isa<PrefixExpr>()) {
        if ((tag() == ADD || tag() == SUB) && tag() == prefix_expr->tag())
            os << ' ';
    }

    auto open_state = open(os, Prec::Unary);
    os << rhs();
    return close(os, open_state);
}

std::ostream& InfixExpr::stream(std::ostream& os) const {
    auto open_state = open(os, PrecTable::infix_l(tag()));
    const char* op;
    switch (tag()) {
#define IMPALA_INFIX_ASGN(tok, str)       case tok: op = str; break;
#define IMPALA_INFIX(     tok, str, prec) case tok: op = str; break;
#include "impala/tokenlist.h"
    }

    os << lhs() << " " << op << " ";
    prec = PrecTable::infix_r(tag());
    os << rhs();
    return close(os, open_state);
}

std::ostream& PostfixExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Prec::Unary);
    const char* op;
    switch (tag()) {
        case INC: op = "++"; break;
        case DEC: op = "--"; break;
        default: THORIN_UNREACHABLE;
    }

    os << lhs() << op;
    return close(os, open_state);
}

std::ostream& FieldExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Prec::Unary);
    os << lhs() << '.' << symbol();
    return close(os, open_state);
}

std::ostream& ExplicitCastExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Prec::As);
    streamf(os, "{} as {}", src(), ast_type());
    return close(os, open_state);
}

std::ostream& ImplicitCastExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Prec::As);
    streamf(os, "implicit_cast({}, {})", src(), type());
    return close(os, open_state);
}

std::ostream& RValueExpr::stream(std::ostream& os) const {
    auto open_state = open(os, Prec::As);
    if (type())
        streamf(os, "rvalue({}, {})", src(), type());
    else
        streamf(os, "rvalue({}, ?)", src());
    return close(os, open_state);
}

std::ostream& StructExpr::Elem::stream(std::ostream& os) const {
    return streamf(os, "{}: {}", symbol(), expr());
}

std::ostream& StructExpr::stream(std::ostream& os) const {
    ast_type_app()->stream(os);
    return stream_list(os, elems(), [&](const auto& elem) { os << elem.get(); }, "{", "}");
}

std::ostream& TypeAppExpr::stream(std::ostream& os) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) os << "(";

    prec = l;
    os << lhs();
    if (num_type_args() == 0)
        stream_list(os, ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "[", "]");
    else
        stream_list(os, type_args(), [&](const Type* type) { os << type; }, "[", "]");

    prec = old;
    if (paren) os << ")";
    return os;
}

std::ostream& MapExpr::stream(std::ostream& os) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) os << "(";

    prec = l;
    os << lhs();
    stream_list(os, args(), [&](const auto& expr) { os << expr.get(); }, "(", ")");
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
        auto ret = params().back().get();
        if (ret->type()) {
            auto rettype = ret->type()->as<FnType>();
            if (rettype->num_ops() == 1)
                os << rettype->op(0);
            else
                stream_list(os, rettype->ops(), [&](const Type* type) { os << type; }, "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->num_ast_type_args() == 1)
                os << rettype->ast_type_arg(0);
            else
                stream_list(os, rettype->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")", ", ");
        }
        os << " ";
    }

    return os << body();
}

std::ostream& IfExpr::stream(std::ostream& os) const {
    streamf(os, "if {} {}", cond(), then_expr());
    if (has_else())
        os << " else " << else_expr();
    return os;
}

std::ostream& MatchExpr::Arm::stream(std::ostream& os) const {
    return os << ptrn() << " => " << expr() << ",";
}

std::ostream& MatchExpr::stream(std::ostream& os) const {
    os << "match " << expr() << " {" << up << endl;
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        os << arm(i);
        if (i == e - 1) os << down;
        os << endl;
    }
    return (os << "}");
}

std::ostream& WhileExpr::stream(std::ostream& os) const {
    return streamf(os, "while {} {}", cond(), body());
}

std::ostream& ForExpr::stream(std::ostream& os) const {
    stream_list(os << "for ", fn_expr()->params().skip_back(), [&](const auto& param) { os << param.get(); }) << " in ";
    return os << expr() << ' ' << fn_expr()->body();
}

/*
 * patterns
 */

std::ostream& TuplePtrn::stream(std::ostream& os) const {
    stream_list(os << "(", elems(), [&] (const auto& ptrn) { os << ptrn.get(); }) << ")";
    return os;
}

std::ostream& IdPtrn::stream(std::ostream& os) const {
    return os << local();
}

std::ostream& EnumPtrn::stream(std::ostream& os) const {
    if (num_args() > 0) {
        return stream_list(os << path() << "(", args(), [&] (const auto& arg) { os << arg.get(); }) << ")";
    } else {
        return os << path();
    }
}

std::ostream& LiteralPtrn::stream(std::ostream& os) const {
    return os << literal();
}

std::ostream& CharPtrn::stream(std::ostream& os) const {
    return os << chr();
}

/*
 * statements
 */

std::ostream& ItemStmt::stream(std::ostream& os) const { return os << item(); }

std::ostream& LetStmt::stream(std::ostream& os) const {
    os << "let " << ptrn();
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

std::ostream& AsmStmt::Elem::stream(std::ostream& os) const {
    return streamf(os, "\"{}\"({})", constraint(), expr());
}

std::ostream& AsmStmt::stream(std::ostream& os) const {
    os << "asm(\"" << asm_template() << "\"";
    stream_list(os << "\n\t: ",  outputs(), [&](const auto& elem) { os << elem.get(); });
    stream_list(os << "\n\t: ",   inputs(), [&](const auto& elem) { os << elem.get(); });
    stream_list(os << "\n\t: ", clobbers(), [&](const auto& clobber) { os << "\"" << clobber << "\""; });
    stream_list(os << "\n\t: ",  options(), [&](const auto& option) { os << "\"" << option << "\""; });
    return os << ");";
}

}
