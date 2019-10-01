#include "impala/ast.h"
#include "impala/impala.h"

namespace impala {

using namespace thorin;

Prec prec = Prec::Bottom;

/*
 * AST types
 */

Stream& ErrorASTType::stream(Stream& s) const { return s << "<error>"; }

Stream& PtrASTType::stream(Stream& s) const {
    s << prefix();
    if (addr_space() != 0)
        s << '[' << addr_space() << ']';
    return s << referenced_ast_type();
}

Stream& DefiniteArrayASTType::stream(Stream& s) const { return s.fmt("[{} * {}]", elem_ast_type(), dim()); }
Stream& IndefiniteArrayASTType::stream(Stream& s) const { return s.fmt("[{}]", elem_ast_type()); }

Stream& TupleASTType::stream(Stream& s) const { return s.fmt("({, })", ast_type_args()); }

Stream& FnASTType::stream(Stream& s) const {
    auto ret = ret_fn_ast_type();
    stream_ast_type_params(s << "fn");
    s.fmt("({, })", ret != nullptr ? ast_type_args().skip_back() : ast_type_args());
    if (ret != nullptr) {
        s << " -> ";
        if (ret->num_ast_type_args() == 1)
            s << ret->ast_type_args().front().get();
        else
            s.fmt("({, })", ret->ast_type_args());
    }
    return s;
}

Stream& ASTTypeApp::stream(Stream& s) const {
    s << symbol();
    if (num_ast_type_args() != 0)
        s.fmt("[{, }]", ast_type_args());
    return s;
}

Stream& PrimASTType::stream(Stream& s) const {
    switch (tag()) {
#define IMPALA_TYPE(itype, atype) case Token::TYPE_##itype: return s << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Stream& Typeof::stream(Stream& s) const {
    return s.fmt("typeof({})", expr());
}

/*
 * paths
 */

Stream& Identifier::stream(Stream& s) const { return s << symbol(); }
Stream& Path::Elem::stream(Stream& s) const { return s << symbol(); }
Stream& Path::stream(Stream& s) const {
    s << (is_global() ? "::" : "");
    return s.fmt("{::}", elems());
}

/*
 * parameters
 */

Stream& ASTTypeParam::stream(Stream& s) const {
    s << symbol() << (bounds_.empty() ? "" : ": ");
    return s.fmt("{ + }", bounds());
}

Stream& ASTTypeParamList::stream_ast_type_params(Stream& s) const {
    if (!ast_type_params().empty())
        s.fmt("[{, }]", ast_type_params());
    return s;
}

/*
 * other decls
 */

Stream& Fn::stream_params(Stream& s, bool returning) const {
    return s.fmt("({, })", returning ? params().skip_back() : params());
}

Stream& LocalDecl::stream(Stream& s) const {
    s << (is_mut() ? "mut " : "" );
    if (!is_anonymous()) {
        s << symbol();
        if (type())
            s << ": " << type();
        else if (ast_type())
            s << ": " << ast_type();
    }

    return s;
}

Stream& Param::stream(Stream& s) const {
    if (!is_anonymous())
        s << (is_mut() ? "mut " : "") << symbol() <<
            ((ast_type() || type()) ? ": " : "");

    if (type())
        s << type();
    else if (ast_type())
        s << ast_type();

    return s;
}

/*
 * items + item helpers
 */

Stream& Module::stream(Stream& s) const {
    return s.fmt("{\n}", items());
}

Stream& ModuleDecl::stream(Stream& s) const {
    return stream_ast_type_params(s << "mod " << symbol()) << ';';
}

Stream& ExternBlock::stream(Stream& s) const {
    s << "extern ";
    if (!abi_.empty())
        s << abi_.str() << ' ';
    (s << '{').indent().endl();
    s.list(fn_decls(), [&](const auto& fn_decl) { s << fn_decl.get(); }, "", "", "", true);
    return s.dedent().endl() << '}';
}

Stream& FnDecl::stream(Stream& s) const {
    if (is_extern())
        s << "extern ";
    s << "fn ";
    if (filter())
        s << '@' << filter() << ' ';
    if (export_name_)
        s << export_name_ << ' ';
    stream_ast_type_params(s << symbol());

    const FnASTType* ret = nullptr;
    if (!params().empty() && params().back()->symbol() == "return" && params().back()->ast_type()) {
        if (auto fn_type = params().back()->ast_type()->isa<FnASTType>())
            ret = fn_type;
    }

    stream_params(s << '(', ret != nullptr) << ")";

    if (ret) {
        s << " -> ";
        if (ret->num_ast_type_args() == 1)
            s << ret->ast_type_arg(0);
        else
            s.list(ret->ast_type_args(), [&](const auto& ast_type) { s << ast_type.get(); }, "(", ")", ", ");
    }

    if (body()) {
        s << ' ' << body();
    } else
        s << ';';

    return s;
}

Stream& FieldDecl::stream(Stream& s) const {
    return s.fmt("{}{}: {}", visibility().str(), symbol(), ast_type());
}

Stream& OptionDecl::stream(Stream& s) const {
    s.fmt("{}", symbol());
    if (num_args() > 0) {
        return s.list(args(), [&](const auto& arg) { s << arg.get(); }, "(", ")", ", ");
    } else {
        return s;
    }
}

Stream& StaticItem::stream(Stream& s) const {
    s.fmt("static {}{}", is_mut() ? "mut " : "", identifier());
    if (type())
        s << type();
    else
        s << ast_type();

    if (init())
        s.fmt(" = {}", init());

    return s << ";";
}

Stream& StructDecl::stream(Stream& s) const {
    (stream_ast_type_params(s.fmt("{}struct {}", visibility().str(), symbol())) << " {").indent().endl();
    return s.list(field_decls(), [&](const auto& field) { s << field.get(); }, "", "", ",", true).dedent().endl() << "}";
}

Stream& EnumDecl::stream(Stream& s) const {
    (stream_ast_type_params(s.fmt("{}enum {}", visibility().str(), symbol())) << " {").indent().endl();
    return s.list(option_decls(), [&](const auto& option) { s << option.get(); }, "", "", ",", true).dedent().endl() << "}";
}

Stream& Typedef::stream(Stream& s) const {
    return stream_ast_type_params(s.fmt("{}type {}", visibility().str(), symbol())) << " = " << ast_type() << ';';
}

Stream& TraitDecl::stream(Stream& s) const {
    s << "trait " << symbol();
    stream_ast_type_params(s);

    if (!super_traits().empty()) {
        s << " : ";
        s.list(super_traits(), [&](const auto& type_app) { s << type_app.get(); });
    }

    (s << " {").indent().endl();
    s.list(methods(), [&](const auto& method) { s << method.get(); }, "", "", "", true);
    return s.dedent().endl() << '}';
}

Stream& ImplItem::stream(Stream& s) const {
    s << "impl";
    stream_ast_type_params(s) << ' ';
    if (trait())
        s << trait() << " for ";
    (s << ast_type() << " {").indent().endl();
    s.list(methods(), [&](const auto& method) { s << method.get(); }, "", "", "", true);
    return s.dedent().endl() << "}";
}

/*
 * expressions
 */

Stream& BlockExpr::stream(Stream& s) const {
    s << '{';
    if (empty())
        return s.endl() << '}';

    s.indent().endl().list(stmts(), [&](const auto& stmt) { s << stmt.get(); }, "", "", "", true);

    if (!expr()->isa<EmptyExpr>()) {
        if (!stmts().empty())
            s.endl();
        s << expr();
    }

    return s.dedent().endl() << "}";
}

Stream& LiteralExpr::stream(Stream& s) const {
    switch (tag()) {
        case LIT_bool: return s << (get<bool>() ? "true" : "false");
        case LIT_i8:   return s << (int)get< s8>() << "i8";
        case LIT_i16:  return s <<      get<s16>() << "i16";
        case LIT_i32:  return s <<      get<s32>();
        case LIT_i64:  return s <<      get<s64>() << "i64";
        case LIT_u8:   return s << (int)get< u8>() << "u8";
        case LIT_u16:  return s <<      get<u16>() << "u16";
        case LIT_u32:  return s <<      get<u32>() << "u";
        case LIT_u64:  return s <<      get<u64>() << "u64";
        case LIT_f16:  return s <<      get<r16>() << "h";
        case LIT_f32:  return s <<      get<r32>() << "f";
        case LIT_f64:  return s <<      get<r64>() << "f64";
        default: THORIN_UNREACHABLE;
    }
}

Stream& CharExpr::stream(Stream& s) const {
    return s << symbol();
}

Stream& StrExpr::stream(Stream& s) const {
    if (symbols().size() == 1)
        return s << '\'' << symbols().front().remove_quotation() << '\'';
    return s.indent().endl().list(symbols() , [&](Symbol symbol) { s << symbol; }, "", "", "", true).dedent().endl();
}

Stream& PathExpr ::stream(Stream& s) const { return s << path(); }
Stream& EmptyExpr::stream(Stream& s) const { return s << "/*empty*/"; }
Stream& TupleExpr::stream(Stream& s) const {
    return s.list(args(), [&](const auto& expr) { s << expr.get(); }, "(", ")");
}

Stream& DefiniteArrayExpr::stream(Stream& s) const {
    return s.list(args(), [&](const auto& expr) { s << expr.get(); }, "[", "]");
}

Stream& RepeatedDefiniteArrayExpr::stream(Stream& s) const {
    return s.fmt("[{}, .. {}]", value(), count());
}

Stream& IndefiniteArrayExpr::stream(Stream& s) const {
    return s.fmt("[{}: {}]", dim(), elem_ast_type());
}

static std::pair<Prec, bool> open(Stream& s, Prec l) {
    std::pair<Prec, bool> result;
    result.first = prec;
    result.second = !fancy() || prec > l;
    if (result.second)
        s << "(";
    prec = l;
    return result;
}

static Stream& close(Stream& s, std::pair<Prec, bool> pair) {
    prec = pair.first;
    if (pair.second)
        s << ")";
    return s;
}

Stream& PrefixExpr::stream(Stream& s) const {
    const char* op;
    switch (tag()) {
#define IMPALA_PREFIX(tok, str) case tok: op = str; break;
#include "impala/tokenlist.h"
        case MUT: op = "&mut "; break;
        default: THORIN_UNREACHABLE;
    }

    s << op;
    if (auto prefix_expr = rhs()->isa<PrefixExpr>()) {
        if ((tag() == ADD || tag() == SUB) && tag() == prefix_expr->tag())
            s << ' ';
    }

    auto open_state = open(s, Prec::Unary);
    s << rhs();
    return close(s, open_state);
}

Stream& InfixExpr::stream(Stream& s) const {
    auto open_state = open(s, PrecTable::infix_l(tag()));
    const char* op;
    switch (tag()) {
#define IMPALA_INFIX_ASGN(tok, str)       case tok: op = str; break;
#define IMPALA_INFIX(     tok, str, prec) case tok: op = str; break;
#include "impala/tokenlist.h"
    }

    s << lhs() << " " << op << " ";
    prec = PrecTable::infix_r(tag());
    s << rhs();
    return close(s, open_state);
}

Stream& PostfixExpr::stream(Stream& s) const {
    auto open_state = open(s, Prec::Unary);
    const char* op;
    switch (tag()) {
        case INC: op = "++"; break;
        case DEC: op = "--"; break;
        default: THORIN_UNREACHABLE;
    }

    s << lhs() << op;
    return close(s, open_state);
}

Stream& FieldExpr::stream(Stream& s) const {
    auto open_state = open(s, Prec::Unary);
    s << lhs() << '.' << symbol();
    return close(s, open_state);
}

Stream& ExplicitCastExpr::stream(Stream& s) const {
    auto open_state = open(s, Prec::As);
    s.fmt("{} as {}", src(), ast_type());
    return close(s, open_state);
}

Stream& ImplicitCastExpr::stream(Stream& s) const {
    auto open_state = open(s, Prec::As);
    s.fmt("implicit_cast({}, {})", src(), type());
    return close(s, open_state);
}

Stream& RValueExpr::stream(Stream& s) const {
    auto open_state = open(s, Prec::As);
    if (type())
        s.fmt("rvalue({}, {})", src(), type());
    else
        s.fmt("rvalue({}, ?)", src());
    return close(s, open_state);
}

Stream& StructExpr::Elem::stream(Stream& s) const {
    return s.fmt("{}: {}", symbol(), expr());
}

Stream& StructExpr::stream(Stream& s) const {
    ast_type_app()->stream(s);
    return s.list(elems(), [&](const auto& elem) { s << elem.get(); }, "{", "}");
}

Stream& TypeAppExpr::stream(Stream& s) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) s << "(";

    prec = l;
    s << lhs();
    if (num_type_args() == 0)
        s.list(ast_type_args(), [&](const auto& ast_type) { s << ast_type.get(); }, "[", "]");
    else
        s.list(type_args(), [&](const Type* type) { s << type; }, "[", "]");

    prec = old;
    if (paren) s << ")";
    return s;
}

Stream& MapExpr::stream(Stream& s) const {
    Prec l = Prec::Unary;
    Prec old = prec;
    bool paren = !fancy() || prec > l;
    if (paren) s << "(";

    prec = l;
    s << lhs();
    s.list(args(), [&](const auto& expr) { s << expr.get(); }, "(", ")");
    prec = old;
    if (paren) s << ")";
    return s;
}

Stream& FnExpr::stream(Stream& s) const {
    bool has_return_type = !params().empty() && params().back()->symbol() == "return";
    s << '|';
    stream_params(s, has_return_type);
    s << "| ";

    if (has_return_type) {
        s << "-> ";
        auto ret = params().back().get();
        if (ret->type()) {
            auto rettype = ret->type()->as<FnType>();
            if (rettype->num_ops() == 1)
                s << rettype->op(0);
            else
                s.list(rettype->ops(), [&](const Type* type) { s << type; }, "(", ")", ", ");
        } else if (ret->ast_type()) {
            auto rettype = ret->ast_type()->as<FnASTType>();
            if (rettype->num_ast_type_args() == 1)
                s << rettype->ast_type_arg(0);
            else
                s.list(rettype->ast_type_args(), [&](const auto& ast_type) { s << ast_type.get(); }, "(", ")", ", ");
        }
        s << " ";
    }

    return s << body();
}

Stream& IfExpr::stream(Stream& s) const {
    s.fmt("if {} {}", cond(), then_expr());
    if (has_else())
        s << " else " << else_expr();
    return s;
}

Stream& MatchExpr::Arm::stream(Stream& s) const {
    return s << ptrn() << " => " << expr() << ",";
}

Stream& MatchExpr::stream(Stream& s) const {
    (s << "match " << expr() << " {").indent().endl();
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        s << arm(i);
        if (i == e - 1) s.dedent();
        s.endl();
    }
    return (s << "}");
}

Stream& WhileExpr::stream(Stream& s) const {
    return s.fmt("while {} {}", cond(), body());
}

Stream& ForExpr::stream(Stream& s) const {
    (s << "for ").list(fn_expr()->params().skip_back(), [&](const auto& param) { s << param.get(); }) << " in ";
    return s << expr() << ' ' << fn_expr()->body();
}

/*
 * patterns
 */

Stream& TuplePtrn::stream(Stream& s) const {
    return s.list(elems(), [&] (const auto& ptrn) { s << ptrn.get(); }, "(", ")");
}

Stream& IdPtrn::stream(Stream& s) const {
    return s << local();
}

Stream& EnumPtrn::stream(Stream& s) const {
    if (num_args() > 0) {
        return (s << path()).list(args(), [&] (const auto& arg) { s << arg.get(); }, "(", ")");
    } else {
        return s << path();
    }
}

Stream& LiteralPtrn::stream(Stream& s) const {
    return s << literal();
}

Stream& CharPtrn::stream(Stream& s) const {
    return s << chr();
}

/*
 * statements
 */

Stream& ItemStmt::stream(Stream& s) const { return s << item(); }

Stream& LetStmt::stream(Stream& s) const {
    s << "let " << ptrn();
    if (init())
        s << " = " << init();
    return s << ';';
}

Stream& ExprStmt::stream(Stream& s) const {
    bool no_semi = expr()->isa<IfExpr>() || expr()->isa<ForExpr>();
    s << expr();
    if (!no_semi)
        s << ';';
    return s;
}

Stream& AsmStmt::Elem::stream(Stream& s) const {
    return s.fmt("\"{}\"({})", constraint(), expr());
}

Stream& AsmStmt::stream(Stream& s) const {
    (s << "asm(\"" << asm_template() << "\"").indent().endl() << ": ";
    s.list( outputs(), [&](const auto& elem) { s << elem.get(); });
    s.list(  inputs(), [&](const auto& elem) { s << elem.get(); });
    s.list(clobbers(), [&](const auto& clob) { s << "\"" << clob << "\""; });
    s.list( options(), [&](const auto& opt ) { s << "\"" << opt << "\""; });
    return (s << ");").dedent().endl();
}

}
