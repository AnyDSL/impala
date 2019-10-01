#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

using namespace thorin;

Prec prec = Prec::Bottom;

void ASTNode::dump() const {
    Stream s(std::cout);
    stream(s);
}

/*
 * AST types
 */

Stream& ErrorASTType::stream(Stream& os) const { return os << "<error>"; }

Stream& PtrASTType::stream(Stream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << referenced_ast_type();
}

Stream& DefiniteArrayASTType::stream(Stream& os) const { return os.fmt("[{} * {}]", elem_ast_type(), dim()); }
Stream& IndefiniteArrayASTType::stream(Stream& os) const { return os.fmt("[{}]", elem_ast_type()); }

Stream& TupleASTType::stream(Stream& os) const {
    return os.list(ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
}

Stream& FnASTType::stream(Stream& os) const {
    auto ret = ret_fn_ast_type();
    stream_ast_type_params(os << "fn");
    os.list(ret != nullptr ? ast_type_args().skip_back() : ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
    if (ret != nullptr) {
        os << " -> ";
        if (ret->num_ast_type_args() == 1)
            os << ret->ast_type_args().front().get();
        else
            os.list(ret->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")");
    }
    return os;
}

Stream& ASTTypeApp::stream(Stream& os) const {
    os << symbol();
    if (num_ast_type_args() != 0)
        os.list(ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "[", "]");
    return os;
}

Stream& PrimASTType::stream(Stream& os) const {
    switch (tag()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Stream& Typeof::stream(Stream& os) const {
    return os.fmt("typeof({})", expr());
}

/*
 * paths
 */

Stream& Identifier::stream(Stream& os) const { return os << symbol(); }
Stream& Path::Elem::stream(Stream& os) const { return os << symbol(); }
Stream& Path::stream(Stream& os) const {
    os << (is_global() ? "::" : "");
    return os.list(elems(), [&](const auto& elem) { os << elem.get(); }, "", "", "::");
}

/*
 * parameters
 */

Stream& ASTTypeParam::stream(Stream& os) const {
    os << symbol() << (bounds_.empty() ? "" : ": ");
    return os.list(bounds(), [&](const auto& type) { os << type.get(); }, "", "", " + ");
}

Stream& ASTTypeParamList::stream_ast_type_params(Stream& os) const {
    if (!ast_type_params().empty())
        os.list(ast_type_params(), [&](const auto& ast_type_param) { os << ast_type_param.get(); }, "[", "]");
    return os;
}

/*
 * other decls
 */

Stream& Fn::stream_params(Stream& os, bool returning) const {
    return os.list(returning ? params().skip_back() : params(), [&](const auto& param) { param->stream(os); });
}

Stream& LocalDecl::stream(Stream& os) const {
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

Stream& Param::stream(Stream& os) const {
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

Stream& Module::stream(Stream& os) const {
    return os.list(items(), [&](const auto& item) { (os << item.get()).endl(); }, "", "", "", true);
}

Stream& ModuleDecl::stream(Stream& os) const {
    return stream_ast_type_params(os << "mod " << symbol()) << ';';
}

Stream& ExternBlock::stream(Stream& os) const {
    os << "extern ";
    if (!abi_.empty())
        os << abi_.str() << ' ';
    (os << '{').indent().endl();
    os.list(fn_decls(), [&](const auto& fn_decl) { os << fn_decl.get(); }, "", "", "", true);
    return os.dedent().endl() << '}';
}

Stream& FnDecl::stream(Stream& os) const {
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
            os.list(ret->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")", ", ");
    }

    if (body()) {
        os << ' ' << body();
    } else
        os << ';';

    return os;
}

Stream& FieldDecl::stream(Stream& os) const {
    return os.fmt("{}{}: {}", visibility().str(), symbol(), ast_type());
}

Stream& OptionDecl::stream(Stream& os) const {
    os.fmt("{}", symbol());
    if (num_args() > 0) {
        return os.list(args(), [&](const auto& arg) { os << arg.get(); }, "(", ")", ", ");
    } else {
        return os;
    }
}

Stream& StaticItem::stream(Stream& os) const {
    os.fmt("static {}{}", is_mut() ? "mut " : "", identifier());
    if (type())
        os << type();
    else
        os << ast_type();

    if (init())
        os.fmt(" = {}", init());

    return os << ";";
}

Stream& StructDecl::stream(Stream& os) const {
    (stream_ast_type_params(os.fmt("{}struct {}", visibility().str(), symbol())) << " {").indent().endl();
    return os.list(field_decls(), [&](const auto& field) { os << field.get(); }, "", "", ",", true).dedent().endl() << "}";
}

Stream& EnumDecl::stream(Stream& os) const {
    (stream_ast_type_params(os.fmt("{}enum {}", visibility().str(), symbol())) << " {").indent().endl();
    return os.list(option_decls(), [&](const auto& option) { os << option.get(); }, "", "", ",", true).dedent().endl() << "}";
}

Stream& Typedef::stream(Stream& os) const {
    return stream_ast_type_params(os.fmt("{}type {}", visibility().str(), symbol())) << " = " << ast_type() << ';';
}

Stream& TraitDecl::stream(Stream& os) const {
    os << "trait " << symbol();
    stream_ast_type_params(os);

    if (!super_traits().empty()) {
        os << " : ";
        os.list(super_traits(), [&](const auto& type_app) { os << type_app.get(); });
    }

    (os << " {").indent().endl();
    os.list(methods(), [&](const auto& method) { os << method.get(); }, "", "", "", true);
    return os.dedent().endl() << '}';
}

Stream& ImplItem::stream(Stream& os) const {
    os << "impl";
    stream_ast_type_params(os) << ' ';
    if (trait())
        os << trait() << " for ";
    (os << ast_type() << " {").indent().endl();
    os.list(methods(), [&](const auto& method) { os << method.get(); }, "", "", "", true);
    return os.dedent().endl() << "}";
}

/*
 * expressions
 */

Stream& BlockExpr::stream(Stream& os) const {
    os << '{';
    if (empty())
        return os.endl() << '}';

    os.indent().endl().list(stmts(), [&](const auto& stmt) { os << stmt.get(); }, "", "", "", true);

    if (!expr()->isa<EmptyExpr>()) {
        if (!stmts().empty())
            os.endl();
        os << expr();
    }

    return os.dedent().endl() << "}";
}

Stream& LiteralExpr::stream(Stream& os) const {
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

Stream& CharExpr::stream(Stream& os) const {
    return os << symbol();
}

Stream& StrExpr::stream(Stream& os) const {
    if (symbols().size() == 1)
        return os << '\'' << symbols().front().remove_quotation() << '\'';
    return os.indent().endl().list(symbols() , [&](Symbol symbol) { os << symbol; }, "", "", "", true).dedent().endl();
}

Stream& PathExpr ::stream(Stream& os) const { return os << path(); }
Stream& EmptyExpr::stream(Stream& os) const { return os << "/*empty*/"; }
Stream& TupleExpr::stream(Stream& os) const {
    return os.list(args(), [&](const auto& expr) { os << expr.get(); }, "(", ")");
}

Stream& DefiniteArrayExpr::stream(Stream& os) const {
    return os.list(args(), [&](const auto& expr) { os << expr.get(); }, "[", "]");
}

Stream& RepeatedDefiniteArrayExpr::stream(Stream& os) const {
    return os.fmt("[{}, .. {}]", value(), count());
}

Stream& IndefiniteArrayExpr::stream(Stream& os) const {
    return os.fmt("[{}: {}]", dim(), elem_ast_type());
}

static std::pair<Prec, bool> open(Stream& os, Prec l) {
    std::pair<Prec, bool> result;
    result.first = prec;
    result.second = !fancy() || prec > l;
    if (result.second)
        os << "(";
    prec = l;
    return result;
}

static Stream& close(Stream& os, std::pair<Prec, bool> pair) {
    prec = pair.first;
    if (pair.second)
        os << ")";
    return os;
}

Stream& PrefixExpr::stream(Stream& os) const {
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

Stream& InfixExpr::stream(Stream& os) const {
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

Stream& PostfixExpr::stream(Stream& os) const {
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

Stream& FieldExpr::stream(Stream& os) const {
    auto open_state = open(os, Prec::Unary);
    os << lhs() << '.' << symbol();
    return close(os, open_state);
}

Stream& ExplicitCastExpr::stream(Stream& os) const {
    auto open_state = open(os, Prec::As);
    os.fmt("{} as {}", src(), ast_type());
    return close(os, open_state);
}

Stream& ImplicitCastExpr::stream(Stream& os) const {
    auto open_state = open(os, Prec::As);
    os.fmt("implicit_cast({}, {})", src(), type());
    return close(os, open_state);
}

Stream& RValueExpr::stream(Stream& os) const {
    auto open_state = open(os, Prec::As);
    if (type())
        os.fmt("rvalue({}, {})", src(), type());
    else
        os.fmt("rvalue({}, ?)", src());
    return close(os, open_state);
}

Stream& StructExpr::Elem::stream(Stream& os) const {
    return os.fmt("{}: {}", symbol(), expr());
}

Stream& StructExpr::stream(Stream& os) const {
    ast_type_app()->stream(os);
    return os.list(elems(), [&](const auto& elem) { os << elem.get(); }, "{", "}");
}

Stream& TypeAppExpr::stream(Stream& os) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) os << "(";

    prec = l;
    os << lhs();
    if (num_type_args() == 0)
        os.list(ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "[", "]");
    else
        os.list(type_args(), [&](const Type* type) { os << type; }, "[", "]");

    prec = old;
    if (paren) os << ")";
    return os;
}

Stream& MapExpr::stream(Stream& os) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) os << "(";

    prec = l;
    os << lhs();
    os.list(args(), [&](const auto& expr) { os << expr.get(); }, "(", ")");
    prec = old;
    if (paren) os << ")";
    return os;
}

Stream& FnExpr::stream(Stream& os) const {
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
                os.list(rettype->ops(), [&](const Type* type) { os << type; }, "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->num_ast_type_args() == 1)
                os << rettype->ast_type_arg(0);
            else
                os.list(rettype->ast_type_args(), [&](const auto& ast_type) { os << ast_type.get(); }, "(", ")", ", ");
        }
        os << " ";
    }

    return os << body();
}

Stream& IfExpr::stream(Stream& os) const {
    os.fmt("if {} {}", cond(), then_expr());
    if (has_else())
        os << " else " << else_expr();
    return os;
}

Stream& MatchExpr::Arm::stream(Stream& os) const {
    return os << ptrn() << " => " << expr() << ",";
}

Stream& MatchExpr::stream(Stream& os) const {
    (os << "match " << expr() << " {").indent().endl();
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        os << arm(i);
        if (i == e - 1) os.dedent();
        os.endl();
    }
    return (os << "}");
}

Stream& WhileExpr::stream(Stream& os) const {
    return os.fmt("while {} {}", cond(), body());
}

Stream& ForExpr::stream(Stream& os) const {
    (os << "for ").list(fn_expr()->params().skip_back(), [&](const auto& param) { os << param.get(); }) << " in ";
    return os << expr() << ' ' << fn_expr()->body();
}

/*
 * patterns
 */

Stream& TuplePtrn::stream(Stream& os) const {
    return os.list(elems(), [&] (const auto& ptrn) { os << ptrn.get(); }, "(", ")");
}

Stream& IdPtrn::stream(Stream& os) const {
    return os << local();
}

Stream& EnumPtrn::stream(Stream& os) const {
    if (num_args() > 0) {
        return (os << path()).list(args(), [&] (const auto& arg) { os << arg.get(); }, "(", ")");
    } else {
        return os << path();
    }
}

Stream& LiteralPtrn::stream(Stream& os) const {
    return os << literal();
}

Stream& CharPtrn::stream(Stream& os) const {
    return os << chr();
}

/*
 * statements
 */

Stream& ItemStmt::stream(Stream& os) const { return os << item(); }

Stream& LetStmt::stream(Stream& os) const {
    os << "let " << ptrn();
    if (init())
        os << " = " << init();
    return os << ';';
}

Stream& ExprStmt::stream(Stream& os) const {
    bool no_semi = expr()->isa<IfExpr>() || expr()->isa<ForExpr>();
    os << expr();
    if (!no_semi)
        os << ';';
    return os;
}

Stream& AsmStmt::Elem::stream(Stream& os) const {
    return os.fmt("\"{}\"({})", constraint(), expr());
}

Stream& AsmStmt::stream(Stream& os) const {
    (os << "asm(\"" << asm_template() << "\"").indent().endl() << ": ";
    os.list( outputs(), [&](const auto& elem) { os << elem.get(); });
    os.list(  inputs(), [&](const auto& elem) { os << elem.get(); });
    os.list(clobbers(), [&](const auto& clob) { os << "\"" << clob << "\""; });
    os.list( options(), [&](const auto& opt ) { os << "\"" << opt << "\""; });
    return (os << ");").dedent().endl();
}

}
