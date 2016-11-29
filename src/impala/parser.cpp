#include <algorithm>
#include <functional>
#include <sstream>
#include <iostream>

#include "thorin/util/array.h"
#include "thorin/util/assert.h"
#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/lexer.h"
#include "impala/prec.h"

#define VISIBILITY \
         Token::PRIV: \
    case Token::PUB

#define ITEM \
         Token::ENUM: \
    case Token::EXTERN: \
    case Token::FN: \
    case Token::IMPL: \
    case Token::MOD: \
    case Token::STATIC: \
    case Token::STRUCT: \
    case Token::TYPEDEF: \
    case Token::TRAIT

#define MOD_CONTENTS \
         VISIBILITY: \
    case ITEM

#define EXPR \
         Token::LIT_i8: \
    case Token::LIT_i16: \
    case Token::LIT_i32: \
    case Token::LIT_i64: \
    case Token::LIT_u8: \
    case Token::LIT_u16: \
    case Token::LIT_u32: \
    case Token::LIT_u64: \
    case Token::LIT_f16: \
    case Token::LIT_f32: \
    case Token::LIT_f64: \
    case Token::LIT_char: \
    case Token::LIT_str: \
    case Token::TRUE: \
    case Token::FALSE: \
    case Token::DOUBLE_COLON: \
    case Token::ADD: \
    case Token::SUB: \
    case Token::MUL: \
    case Token::AND: \
    case Token::TILDE: \
    case Token::NOT: \
    case Token::INC: \
    case Token::DEC: \
    case Token::OR: \
    case Token::OROR: \
    case Token::ID: \
    case Token::RUN: \
    case Token::HLT: \
    case Token::IF: \
    case Token::FOR: \
    case Token::WITH: \
    case Token::WHILE: \
    case Token::L_PAREN: \
    case Token::L_BRACE: \
    case Token::RUN_BLOCK: \
    case Token::L_BRACKET: \
    case Token::SIMD

#define PTRN \
         Token::MUT: \
    case Token::ID: \
         Token::L_PAREN

#define STMT_NOT_EXPR \
         Token::LET: \
    case Token::ASM: \
    case ITEM

#define STMT \
        STMT_NOT_EXPR: \
    case EXPR

#define TYPE \
         Token::FN: \
    case Token::L_PAREN: \
    case Token::ID: \
    case Token::L_BRACKET: \
    case Token::TYPE_i8: \
    case Token::TYPE_i16: \
    case Token::TYPE_i32: \
    case Token::TYPE_i64: \
    case Token::TYPE_u8: \
    case Token::TYPE_u16: \
    case Token::TYPE_u32: \
    case Token::TYPE_u64: \
    case Token::TYPE_f16: \
    case Token::TYPE_f32: \
    case Token::TYPE_f64:  \
    case Token::TYPE_bool: \
    case Token::TYPEOF: \
    case Token::TILDE: \
    case Token::AND: \
    case Token::ANDAND: \
    case Token::SIMD

using namespace thorin;

namespace impala {

class Parser;

class Parser {
public:
    Parser(std::istream& stream, const char* filename)
        : lexer_(stream, filename)
        , cur_var_handle(2) // reserve 1 for conditionals, 0 for mem
        , no_bars_(false)
    {
        lookahead_[0] = lexer_.lex();
        lookahead_[1] = lexer_.lex();
        lookahead_[2] = lexer_.lex();
        prev_loc_ = Location(filename, 1, 1, 1, 1);
    }

    const Token& la(size_t i = 0) const { assert(i < 3); return lookahead_[i]; }
    Location prev_loc() const { return prev_loc_; }

#ifdef NDEBUG
    Token eat(TokenKind) { return lex(); }
#else
    Token eat(TokenKind kind) { assert(kind == la() && "internal parser error"); return lex(); }
#endif

    bool accept(TokenKind tok);
    bool expect(TokenKind tok, const std::string& context);
    void error(const std::string& what, const std::string& context) { error(what, context, la()); }
    void error(const std::string& what, const std::string& context, const Token& tok);

    class Tracker {
    public:
        Tracker(Parser& parser)
            : parser_(parser)
            , location_(parser_.la().location().begin())
        {}

        operator Location() const { return {location_.begin(), parser_.prev_loc().end()}; }

    private:
        Parser& parser_;
        Location location_;
    };

    Tracker track() { return Tracker(*this); }

    /**
     * Parses a list of comma-separated items till one of the @p delimiters have been found.
     * The ending delimiter will @em not be eaten up by this method.
     * The list may also end with a comma.
     */
    void nibble_comma_list(Array<TokenKind> delimiters, std::function<void()> f) {
        auto is_delimiter = [&] () {
            for (auto delimiter : delimiters)
                if (la() == delimiter)
                    return true;
            return false;
        };

        if (!is_delimiter()) {
            do { f(); }
            while (accept(Token::COMMA) && !is_delimiter());
        }
    }

    /// Like @p nibble_comma_list but there is only one @p delimiter which @em will be eaten up by this method.
    void parse_comma_list(const char* context, TokenKind delimiter, std::function<void()> f) {
        nibble_comma_list({delimiter}, f);
        expect(delimiter, context);
    }

    // misc
    const Identifier* try_id(const std::string& what);
    Visibility parse_visibility();
    u64 parse_integer(const char* what);
    int parse_addr_space();
    char char_value(const char*& p);

    // paths
    const Path* parse_path();
    const Path::Elem* parse_path_elem();

    // parameters
    ASTTypeParams parse_ast_type_params();
    const ASTTypeParam* parse_ast_type_param();
    const Param* parse_param(int i, bool lambda);
    Params parse_param_list(TokenKind delimiter, bool lambda);
    void parse_return_param(Fn* fn);

    // types
    const ASTType*      parse_type();
    const ArrayASTType* parse_array_type();
    const Typeof*       parse_typeof();
    const ASTType*      parse_return_type(bool& is_continuation, bool mandatory);
    const FnASTType*    parse_fn_type();
    const PrimASTType*  parse_prim_type();
    const PtrASTType*   parse_ptr_type();
    const TupleASTType* parse_tuple_type();
    const SimdASTType*  parse_simd_type();
    const ASTTypeApp*   parse_ast_type_app();

    enum class BodyMode { None, Optional, Mandatory };

    // items + helpers
    const Item*        parse_item();
    Items              parse_items();
    const StaticItem*  parse_static_item(Tracker, Visibility);
    const EnumDecl*    parse_enum_decl(Tracker, Visibility);
    const FnDecl*      parse_fn_decl(BodyMode, Tracker, Visibility, bool is_extern, Symbol abi);
    const ImplItem*    parse_impl(Tracker, Visibility);
    const Item*        parse_module_or_module_decl(Tracker, Visibility);
    const Module*      parse_module();
    const Item*        parse_extern_block_or_fn_decl(Tracker, Visibility);
    const StructDecl*  parse_struct_decl(Tracker, Visibility);
    const FieldDecl*   parse_field_decl(const size_t i);
    const TraitDecl*   parse_trait_decl(Tracker, Visibility);
    const Typedef*     parse_typedef(Tracker, Visibility);

    // expressions
    bool is_infix();
    const Expr*             parse_expr(Prec prec);
    const Expr*             parse_expr() { return parse_expr(BOTTOM, false); }
    const Expr*             parse_expr(Prec prec, bool no_bars) { THORIN_PUSH(no_bars_, no_bars); return parse_expr(prec); }
    const Expr*             parse_prefix_expr();
    const Expr*             parse_infix_expr(Tracker, const Expr* lhs);
    const Expr*             parse_postfix_expr(Tracker, const Expr* lhs);
    const MapExpr*          parse_map_expr(Tracker, const Expr* lhs);
    const TypeAppExpr*      parse_type_app_expr(Tracker, const Expr* lhs);
    const Expr*             parse_primary_expr();
    const LiteralExpr*      parse_literal_expr();
    const CharExpr*         parse_char_expr();
    const StrExpr*          parse_str_expr();
    const FnExpr*           parse_fn_expr();
    const IfExpr*           parse_if_expr();
    const ForExpr*          parse_for_expr();
    const ForExpr*          parse_with_expr();
    const WhileExpr*        parse_while_expr();
    const BlockExprBase*    parse_block_expr();
    const BlockExprBase*    try_block_expr(const std::string& context);

    // patterns
    const Ptrn*      parse_ptrn();
    const TuplePtrn* parse_tuple_ptrn();
    const IdPtrn*    parse_id_ptrn();

    // statements
    const Stmt*     parse_stmt_not_expr();
    const ItemStmt* parse_item_stmt();
    const LetStmt*  parse_let_stmt();
    const AsmStmt*  parse_asm_stmt();

    // helpers
    std::string parse_str();
    const AsmStmt::Elem* parse_asm_op();

private:
    /// Consume next Token in input stream, fill look-ahead buffer, return consumed Token.
    Token lex();

    const LocalDecl* create_continuation_decl(const char* name, bool set_type) {
        auto identifier = new Identifier(prev_loc(), name);
        auto ast_type = set_type ? new FnASTType(prev_loc(), ASTTypeParams(), ASTTypes()) : nullptr;
        return new LocalDecl(prev_loc(), cur_var_handle++, identifier, ast_type);
    }

    Lexer lexer_;        ///< invoked in order to get next token
    Token lookahead_[3]; ///< SLL(3) look ahead
    size_t cur_var_handle;
    bool no_bars_;
    Location prev_loc_;
};

//------------------------------------------------------------------------------

Items parse(std::istream& is, const char* filename) {
    Parser parser(is, filename);
    auto items = parser.parse_items();
    if (parser.la() != Token::END_OF_FILE)
        parser.error("module item", "module contents");
    return items;
}

//------------------------------------------------------------------------------

/*
 * helpers
 */

Token Parser::lex() {
    Token result = lookahead_[0];  // remember result
    lookahead_[0] = lookahead_[1]; // copy over LA2 to LA1
    lookahead_[1] = lookahead_[2]; // copy over LA3 to LA2
    lookahead_[2] = lexer_.lex();  // fill new LA3
    prev_loc_ = result.location(); // remember previous location
    return result;
}

bool Parser::accept(TokenKind type) {
    if (type != la())
        return false;
    lex();
    return true;
}

bool Parser::expect(TokenKind tok, const std::string& context) {
    if (la() == tok) {
        lex();
        return true;
    } else {
        std::ostringstream oss;
        oss << '\'' << tok << '\'';
        error(oss.str(), context);
        return false;
    }
}

void Parser::error(const std::string& what, const std::string& context, const Token& tok) {
    impala::error(tok.location(), "expected %, got '%'%", what, tok,
            context.empty() ? "" : std::string(" while parsing ") + context.c_str());
}

const Identifier* Parser::try_id(const std::string& what) {
    Token name;
    if (la() == Token::ID)
        name = lex();
    else {
        error("identifier", what);
        name = Token(la().location(), "<error>");
    }

    return new Identifier(name);
}

Visibility Parser::parse_visibility() {
    switch (la()) {
        case VISIBILITY: return Visibility(lex().kind());
        default:         return Visibility(Visibility::None);
    }
}

u64 Parser::parse_integer(const char* what) {
    u64 dim;
    switch (la()) {
        case Token::LIT_i8:   dim = la().box().get_s8();  break;
        case Token::LIT_i16:  dim = la().box().get_s16(); break;
        case Token::LIT_i32:  dim = la().box().get_s32(); break;
        case Token::LIT_i64:  dim = la().box().get_s64(); break;
        case Token::LIT_u8:   dim = la().box().get_u8();  break;
        case Token::LIT_u16:  dim = la().box().get_u16(); break;
        case Token::LIT_u32:  dim = la().box().get_u32(); break;
        case Token::LIT_u64:  dim = la().box().get_u64(); break;
        default:
            dim = 0;
            error("integer literal", what);
    }
    lex();
    return dim;
}

int Parser::parse_addr_space() {
    if (la(0) == Token::L_BRACKET && la(1) == Token::LIT_i32) {
        eat(Token::L_BRACKET);
        int addr_space = parse_integer("address space");
        expect(Token::R_BRACKET, "address space annotation");
        return addr_space;
    }
    return 0;
}

/*
 * paths
 */

const Path::Elem* Parser::parse_path_elem() {
    return new Path::Elem(try_id("path"));
}

const Path* Parser::parse_path() {
    auto tracker = track();
    bool is_global = accept(Token::DOUBLE_COLON);
    Path::Elems elems;
    do {
        elems.emplace_back(parse_path_elem());
    } while (accept(Token::DOUBLE_COLON));
    return new Path(tracker, is_global, std::move(elems));
}

/*
 * parameters
 */

ASTTypeParams Parser::parse_ast_type_params() {
    ASTTypeParams ast_type_params;
    if (accept(Token::L_BRACKET))
        parse_comma_list("type parameter list", Token::R_BRACKET, [&] { ast_type_params.emplace_back(parse_ast_type_param()); });
    return ast_type_params;
}

const ASTTypeParam* Parser::parse_ast_type_param() {
    auto tracker = track();
    auto identifier = try_id("type parameter");
    ASTTypes bounds;
    if (accept(Token::COLON)) {
        do {
            bounds.emplace_back(parse_type());
        } while (accept(Token::ADD));
    }

    return new ASTTypeParam(tracker, identifier, std::move(bounds));
}

Params Parser::parse_param_list(TokenKind delimiter, bool lambda) {
    Params params;
    int i = 0;
    parse_comma_list("parameter list", delimiter, [&] { params.emplace_back(parse_param(i++, lambda)); });
    return params;
}

const Param* Parser::parse_param(int i, bool lambda) {
    auto tracker = track();
    bool mut = accept(Token::MUT);
    const Identifier* identifier = nullptr;
    const ASTType* type = nullptr;
    const ASTType* ast_type = nullptr;
    Token tok = la();

    if (tok == Token::ID)
        identifier = new Identifier(lex());
    else {
        switch (tok) {
            case Token::TYPE:
                type = parse_type();
                break;
            default:
                identifier = new Identifier(tok.location(), "<error>");
                error("identifier", "parameter");
        }
    }

    if (accept(Token::COLON)) {
        if (type)
            error("identifier", "parameter", tok);
        ast_type = parse_type();
    } else if (lambda) {
        if (type)
            error("identifier", "parameter", tok);
    } else {
        if (type == nullptr) {
            // we assume that the identifier refers to a type
            type = new ASTTypeApp(tok.location(), new Path(identifier));
            identifier = nullptr;
        }
        ast_type = type;
    }

    if (identifier == nullptr) {
        std::ostringstream oss;
        oss << '<' << i << ">";
        identifier = new Identifier(prev_loc(), oss.str().c_str());
    }

    return new Param(tracker, cur_var_handle++, mut, identifier, ast_type);
}

void Parser::parse_return_param(Fn* fn) {
    auto fn_type = parse_return_type(fn->is_continuation_, false);
    if (!fn->is_continuation()) {
        auto location = fn_type ? fn_type->location() : prev_loc();
        fn->params_.emplace_back(new Param(location, cur_var_handle++, new Identifier(location, "return"), fn_type));
    }
}

/*
 * items
 */

const Item* Parser::parse_item() {
    auto tracker = track();
    auto vis = parse_visibility();

    switch (la()) {
        case Token::ENUM:    return parse_enum_decl(tracker, vis);
        case Token::EXTERN:  return parse_extern_block_or_fn_decl(tracker, vis);
        case Token::FN:      return parse_fn_decl(BodyMode::Mandatory, tracker, vis, /*extern*/ false, /*abi*/ "");
        case Token::IMPL:    return parse_impl(tracker, vis);
        case Token::MOD:     return parse_module_or_module_decl(tracker, vis);
        case Token::STATIC:  return parse_static_item(tracker, vis);
        case Token::STRUCT:  return parse_struct_decl(tracker, vis);
        case Token::TRAIT:   return parse_trait_decl(tracker, vis);
        case Token::TYPEDEF: return parse_typedef(tracker, vis);
        default: THORIN_UNREACHABLE;
    }
}

const EnumDecl* Parser::parse_enum_decl(Tracker, Visibility) {
    assert(false && "TODO");
    return 0;
}

const Item* Parser::parse_extern_block_or_fn_decl(Tracker tracker, Visibility vis) {
    if (la() == Token::FN)
        return parse_fn_decl(BodyMode::Mandatory, tracker, vis, /*extern*/ true, /*abi*/ "");

    Symbol abi;
    if (la() == Token::LIT_str)
        abi = lex().symbol();

    expect(Token::L_BRACE, "opening brace of external block");
    FnDecls fn_decls;
    while (la() == Token::FN)
        fn_decls.emplace_back(parse_fn_decl(BodyMode::None, tracker, vis, /*extern*/ true, abi));
    expect(Token::R_BRACE, "closing brace of external block");

    return new ExternBlock(tracker, vis, abi, std::move(fn_decls));
}

#if 0
FnDecl* Parser::parse_fn_decl(BodyMode mode, Tracker tracker, Visibility vis) {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_decl = loc(new FnDecl());
    eat(Token::FN);
    if (la() == Token::LIT_str)
        fn_decl->export_name_ = new Identifier(lex());
    fn_decl->identifier_ = try_id("function name");
    auto ast_type_params = parse_ast_type_params;
    expect(Token::L_PAREN, "function head");
    auto params = parse_param_list(Token::R_PAREN, false);
    parse_return_param(fn_decl);

    switch (body_mode) {
        case BodyMode::None:      expect(Token::SEMICOLON, "function declaration"); break;
        case BodyMode::Mandatory: dock(fn_decl->body_, try_block_expr("body of function")); break;
        case BodyMode::Optional:
            if (!accept(Token::SEMICOLON))
                dock(fn_decl->body_, try_block_expr("body of function"));
            break;
    }

    return fn_decl;
}
#endif

const ImplItem* Parser::parse_impl(Tracker tracker, Visibility vis) {
    eat(Token::IMPL);
    auto ast_type_params = parse_ast_type_params();
    const ASTType* ast_type = nullptr;
    const ASTType* trait = nullptr;
    auto type = parse_type();
    if (accept(Token::FOR)) {
        trait = type;
        ast_type = parse_type();
    } else
        ast_type = type;
    expect(Token::L_BRACE, "impl");
    FnDecls methods;
    while (la() == Token::FN)
        methods.emplace_back(parse_fn_decl(BodyMode::Mandatory, tracker, vis, /*exter*/ false, /*abi*/ ""));
    expect(Token::R_BRACE, "closing brace of impl");

    return new ImplItem(tracker, vis, std::move(ast_type_params), trait, ast_type, std::move(methods));
}

const Item* Parser::parse_module_or_module_decl(Tracker tracker, Visibility vis) {
    eat(Token::MOD);
    auto identifier = try_id("module declaration");
    auto ast_type_params = parse_ast_type_params();
    if (accept(Token::L_BRACE)) {
        auto items = parse_items();
        expect(Token::R_BRACE, "module");
        return new Module(tracker, vis, identifier, std::move(ast_type_params), std::move(items));
    } else {
        expect(Token::SEMICOLON, "module declaration");
        return new ModuleDecl(tracker, vis, identifier, std::move(ast_type_params));
    }
}

Items Parser::parse_items() {
    Items items;
    while (true) {
        cur_var_handle = 2; // HACK
        switch (la()) {
            case VISIBILITY:
            case ITEM:
                items.emplace_back(parse_item());
                continue;
            case Token::SEMICOLON:
                lex();
                continue;
            default:
                return items;
        }
    }
}

const StaticItem* Parser::parse_static_item(Tracker tracker, Visibility vis) {
    eat(Token::STATIC);
    bool mut = accept(Token::MUT);
    auto identifier = try_id("static item");
    auto ast_type = accept(Token::COLON) ? parse_type() : nullptr;
    auto init = accept(Token::ASGN) ? parse_expr() : nullptr;
    expect(Token::SEMICOLON, "static item");
    return new StaticItem(tracker, vis, mut, identifier, ast_type, init);
}

const StructDecl* Parser::parse_struct_decl(Tracker tracker, Visibility vis) {
    eat(Token::STRUCT);
    auto identifier = try_id("struct declaration");
    auto ast_type_params = parse_ast_type_params();
    expect(Token::L_BRACE, "struct declaration");
    size_t i = 0;
    FieldDecls field_decls;
    parse_comma_list("closing brace of struct declaration", Token::R_BRACE, [&] {
        field_decls.emplace_back(parse_field_decl(i++));
    });
    return new StructDecl(tracker, vis, identifier, std::move(ast_type_params), std::move(field_decls));
}

const FieldDecl* Parser::parse_field_decl(const size_t i) {
    auto tracker = track();
    auto vis = parse_visibility();
    auto identifier = try_id("struct field");
    expect(Token::COLON, "struct field");
    auto ast_type = parse_type();
    return new FieldDecl(tracker, i, vis, identifier, ast_type);
}

const TraitDecl* Parser::parse_trait_decl(Tracker tracker, Visibility vis) {
    eat(Token::TRAIT);
    auto identifier = try_id("trait declaration");
    auto ast_type_params = parse_ast_type_params();

    ASTTypeApps super_traits;
    if (accept(Token::COLON)) {
        parse_comma_list("trait declaration", Token::L_BRACE, [&] {
            super_traits.emplace_back(parse_ast_type_app());
        });
    }

    expect(Token::L_BRACE, "trait declaration");
    FnDecls methods;
    while (la() == Token::FN)
        methods.emplace_back(parse_fn_decl(BodyMode::Optional, tracker, vis, /*exter*/ false, /*abi*/ ""));
    expect(Token::R_BRACE, "closing brace of trait declaration");

    return new TraitDecl(tracker, vis, identifier, std::move(ast_type_params), std::move(super_traits), std::move(methods));
}

const Typedef* Parser::parse_typedef(Tracker tracker, Visibility vis) {
    eat(Token::TYPEDEF);
    auto identifier = try_id("type definition");
    auto ast_type_params = parse_ast_type_params();
    expect(Token::ASGN, "type definition");
    auto ast_type = parse_type();
    expect(Token::SEMICOLON, "type definition");
    return new Typedef(tracker, vis, identifier, std::move(ast_type_params), ast_type);
}

/*
 * types
 */

const ASTType* Parser::parse_type() {
    switch (la()) {
#define IMPALA_TYPE(itype, atype) \
        case Token::TYPE_##itype:
#include "impala/tokenlist.h"
                                return parse_prim_type();
        case Token::FN:         return parse_fn_type();
        case Token::L_PAREN:    return parse_tuple_type();
        case Token::ID:         return parse_ast_type_app();
        case Token::L_BRACKET:  return parse_array_type();
        case Token::TYPEOF:     return parse_typeof();
        case Token::TILDE:
        case Token::AND:
        case Token::ANDAND:     return parse_ptr_type();
        case Token::SIMD:       return parse_simd_type();
        default:  {
            error("type", "");
            return new ErrorASTType(prev_loc());
        }
    }
}

const ArrayASTType* Parser::parse_array_type() {
    auto tracker = track();
    eat(Token::L_BRACKET);
    auto elem_ast_type = parse_type();
    if (accept(Token::MUL)) {
        auto dim = parse_integer("definite array type");
        expect(Token::R_BRACKET, "definite array type");
        return new DefiniteArrayASTType(tracker, elem_ast_type, dim);
    }

    expect(Token::R_BRACKET, "indefinite array type");
    return new IndefiniteArrayASTType(tracker, elem_ast_type);
}

const FnASTType* Parser::parse_fn_type() {
    auto tracker = track();
    eat(Token::FN);
    auto ast_type_params = parse_ast_type_params();
    ASTTypes ast_type_args;
    expect(Token::L_PAREN, "function type");
    parse_comma_list("closing parenthesis of function type", Token::R_PAREN, [&] {
        ast_type_args.emplace_back(parse_type());
    });

    bool unused;
    if (auto ret_type = parse_return_type(unused, true))
        ast_type_args.emplace_back(ret_type);

    return new FnASTType(tracker, std::move(ast_type_params), std::move(ast_type_args));
}

#if 0
const ASTType* Parser::parse_return_type(bool& is_continuation, bool mandatory) {
    is_continuation = false;
    if (accept(Token::ARROW)) {
        if (accept(Token::NOT)) {
            is_continuation = true;
            return nullptr;
        }

        auto ret_type = loc(new FnASTType());
        if (accept(Token::L_PAREN)) {                   // in-place tuple
            parse_comma_list("closing parenthesis of return type list", Token::R_PAREN, [&] {
                ret_type->ast_type_args_.emplace_back(parse_type());
            });
        } else {
            auto type = parse_type();
            assert(!type->isa<TupleASTType>());
            ret_type->ast_type_args_.emplace_back(type);
        }
        return ret_type;
    }

    if (mandatory) {
        error("return type", "function type");

        // return a type that can be printed and checked
        auto ret_type = loc(new FnASTType());
        ret_type->ast_type_args_.emplace_back(loc(new ErrorASTType()));
        return ret_type;
    }

    return nullptr;
}
#endif

const PrimASTType* Parser::parse_prim_type() {
    auto tracker = track();
    auto kind = (PrimASTType::Kind) lex().kind();
    return new PrimASTType(tracker, kind);
}

const PtrASTType* Parser::parse_ptr_type() {
    auto tracker = track();
    if (accept(Token::ANDAND)) {
        auto kind = accept(Token::MUT) ? PtrASTType::Mut : PtrASTType::Borrowed;
        auto addr_space = parse_addr_space();
        auto referenced_ast_type = parse_type();
        return new PtrASTType(tracker, PtrASTType::Borrowed, 0, new PtrASTType(tracker, kind, addr_space, referenced_ast_type));
    }

    PtrASTType::Kind kind;
    if (accept(Token::TILDE))
        kind = PtrASTType::Owned;
    else {
        eat(Token::AND);
        if (accept(Token::MUT))
            kind = PtrASTType::Mut;
        else
            kind = PtrASTType::Borrowed;
    }

    auto addr_space = parse_addr_space();
    auto referenced_ast_type = parse_type();
    return new PtrASTType(tracker, kind, addr_space, referenced_ast_type);
}

const TupleASTType* Parser::parse_tuple_type() {
    auto tracker = track();
    eat(Token::L_PAREN);
    ASTTypes ast_type_args;
    parse_comma_list("closing parenthesis of tuple type", Token::R_PAREN, [&] {
        ast_type_args.emplace_back(parse_type());
    });
    return new TupleASTType(tracker, std::move(ast_type_args));
}

const ASTTypeApp* Parser::parse_ast_type_app() {
    auto tracker = track();
    auto path = parse_path();

    ASTTypes ast_type_args;
    if (accept(Token::L_BRACKET)) {
        parse_comma_list("type arguments for type application", Token::R_BRACKET, [&] {
            ast_type_args.emplace_back(parse_type());
        });
    }

    return new ASTTypeApp(tracker, path, std::move(ast_type_args));
}

const Typeof* Parser::parse_typeof() {
    auto tracker = track();
    eat(Token::TYPEOF);
    expect(Token::L_PAREN, "typeof");
    auto expr = parse_expr();
    expect(Token::R_PAREN, "typeof");
    return new Typeof(tracker, expr);
}

const SimdASTType* Parser::parse_simd_type() {
    auto tracker = track();
    eat(Token::SIMD);
    expect(Token::L_BRACKET, "simd type");
    auto elem_ast_type = parse_type();
    expect(Token::MUL, "simd type");
    auto size = parse_integer("simd vector size");
    expect(Token::R_BRACKET, "simd type");
    return new SimdASTType(tracker, elem_ast_type, size);
}

/*
 * expressions
 */

bool Parser::is_infix() {
    bool infix = la().is_infix();
    if (no_bars_ && infix)
        return la() != Token::OR && la() != Token::OROR;
    return infix;
}

const Expr* Parser::parse_expr(Prec prec) {
    auto tracker = track();
    auto lhs = la().is_prefix() ? parse_prefix_expr() : parse_primary_expr();

    if (lhs->isa<StmtLikeExpr>())
        return lhs; // bail out for stmt-like expressions

    while (true) {
        /*
         * (lhs  op  LA) op ...  on break  (current prec > lhs prec of LA)  -->  reduce
         *  lhs  op (LA  op ...) otherwise                                  -->  shift
         */

        if (is_infix()) {
            if (prec > PrecTable::infix_l[la()])
                break;

            lhs = parse_infix_expr(tracker, lhs);
        } else if ( la().is_postfix() ) {
            if (prec > PrecTable::postfix_l[la()])
                break;

            lhs = parse_postfix_expr(tracker, lhs);
        } else
            break;
    }

    return lhs;
}

const Expr* Parser::parse_prefix_expr() {
    if (la() == Token::OR || la() == Token::OROR)
        return parse_fn_expr();

    auto tracker = track();
    auto kind = lex().kind();
    auto rhs = parse_expr(PrecTable::prefix_r[kind]);

    return new PrefixExpr(tracker, (PrefixExpr::Kind) kind, rhs);
}

const Expr* Parser::parse_infix_expr(Tracker tracker, const Expr* lhs) {
    auto kind = lex().kind();
    auto rhs = parse_expr(PrecTable::infix_r[kind]);
    return new InfixExpr(tracker, lhs, (InfixExpr::Kind) kind, rhs);
}

const MapExpr* Parser::parse_map_expr(Tracker tracker, const Expr* lhs) {
    eat(Token::L_PAREN);
    Exprs args;
    parse_comma_list("arguments of a map expression", Token::R_PAREN, [&] { args.emplace_back(parse_expr()); });
    return new MapExpr(tracker, lhs, std::move(args));
}

const TypeAppExpr* Parser::parse_type_app_expr(Tracker tracker, const Expr* lhs) {
    eat(Token::L_BRACKET);
    ASTTypes ast_type_args;
    parse_comma_list("type arguments of a map expression", Token::R_BRACKET, [&] { ast_type_args.emplace_back(parse_type()); });
    return new TypeAppExpr(tracker, lhs, std::move(ast_type_args));
}

const Expr* Parser::parse_postfix_expr(Tracker tracker, const Expr* lhs) {
    switch (la()) {
        case Token::L_BRACKET: return parse_type_app_expr(tracker, lhs);
        case Token::L_PAREN:   return parse_map_expr(tracker, lhs);
        case Token::DEC:
        case Token::INC: {
            auto kind = (PostfixExpr::Kind) lex().kind();
            return new PostfixExpr(tracker, lhs, kind);
        }
        case Token::DOT: {
            lex();
            auto identifier = try_id("field expression");
            return new FieldExpr(tracker, lhs, identifier);
        }
        case Token::AS: {
            lex();
            auto ast_type = parse_type();
            return new ExplicitCastExpr(tracker, lhs, ast_type);
        }
        default: THORIN_UNREACHABLE;
    }
}

const Expr* Parser::parse_primary_expr() {
    auto tracker = track();
    switch (la()) {
        case Token::L_PAREN: {
            auto expr = parse_expr();
            if (accept(Token::COMMA)) {
                Exprs args;
                args.emplace_back(expr);
                parse_comma_list("elements of a tuple expression", Token::R_PAREN, [&] { args.emplace_back(parse_expr()); });
                return new TupleExpr(tracker, std::move(args));
            } else {
                expect(Token::R_PAREN, "primary expression");
                return expr;
            }
        }
        case Token::L_BRACKET: {
            auto expr = parse_expr();
            if (accept(Token::COLON)) {
                auto elem_ast_type = parse_type();
                expect(Token::R_BRACKET, "indefinite array expression");
                return new IndefiniteArrayExpr(tracker, expr, elem_ast_type);
            }

            if (accept(Token::COMMA) && accept(Token::DOTDOT)) {
                auto count = parse_integer("repeated array expression");
                expect(Token::R_BRACKET, "repeated array expression");
                return new RepeatedDefiniteArrayExpr(tracker, expr, count);
            }

            Exprs args;
            args.emplace_back(expr);
            parse_comma_list("elements of an array expression", Token::R_BRACKET, [&] { args.emplace_back(parse_expr()); });
            return new DefiniteArrayExpr(tracker, std::move(args));
        }
        case Token::SIMD: {
            eat(Token::SIMD);
            expect(Token::L_BRACKET, "simd expression");
            Exprs args;
            parse_comma_list("elements of a simd expression", Token::R_BRACKET, [&] { args.emplace_back(parse_expr()); });
            return new SimdExpr(tracker, std::move(args));
        }
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype:
#include "impala/tokenlist.h"
        case Token::TRUE:
        case Token::FALSE:      return parse_literal_expr();
        case Token::LIT_char:   return parse_char_expr();
        case Token::LIT_str:    return parse_str_expr();
        case Token::DOUBLE_COLON:
        case Token::ID:  {
            auto path = parse_path();
            ASTTypes ast_type_args;
            if (accept(Token::L_BRACKET)) {     // struct or map expression
                parse_comma_list("type arguments", Token::R_BRACKET, [&] { ast_type_args.emplace_back(parse_type()); });

                if (accept(Token::L_PAREN)) {   // type app expression + map expression
                    auto type_app_expr = new TypeAppExpr(tracker, new PathExpr(path), std::move(ast_type_args));
                    Exprs args;
                    parse_comma_list("arguments of a map expression", Token::R_PAREN, [&] { args.emplace_back(parse_expr()); });
                    return new MapExpr(tracker, type_app_expr, std::move(args));
                } else if (accept(Token::L_BRACE)) {
                    auto ast_type_app = new ASTTypeApp(tracker, path, std::move(ast_type_args));
                    StructExpr::Elems elems;
                    parse_comma_list("elements of struct expression", Token::R_BRACE, [&] {
                        auto tracker = track();
                        auto symbol = try_id("identifier in struct expression");
                        expect(Token::COLON, "struct expression");
                        elems.emplace_back(new StructExpr::Elem(tracker, symbol, parse_expr()));
                    });

                    return new StructExpr(tracker, ast_type_app, std::move(elems));
                }
            }
            if (la(0) == Token::L_BRACE && (la(1) == Token::ID && la(2) == Token::COLON)) {
                eat(Token::L_BRACE);

                auto ast_type_app = new ASTTypeApp(tracker, path, ASTTypes());

                StructExpr::Elems elems;
                parse_comma_list("elements of struct expression", Token::R_BRACE, [&] {
                    auto tracker = track();
                    auto symbol = try_id("identifier in struct expression");
                    expect(Token::COLON, "struct expression");
                    elems.emplace_back(new StructExpr::Elem(tracker, symbol, parse_expr()));
                });

                return new StructExpr(tracker, ast_type_app, std::move(elems));
            }
            return new PathExpr(path);
        }
        case Token::IF:         return parse_if_expr();
        case Token::FOR:        return parse_for_expr();
        case Token::WITH:       return parse_with_expr();
        case Token::WHILE:      return parse_while_expr();
        case Token::L_BRACE:
        case Token::RUN_BLOCK:  return parse_block_expr();
        default:                error("expression", ""); return new EmptyExpr(lex().location());
    }
}

const LiteralExpr* Parser::parse_literal_expr() {
    LiteralExpr::Kind kind;
    Box box;

    switch (la()) {
        case Token::TRUE:       return new LiteralExpr(lex().location(), LiteralExpr::LIT_bool, Box(true));
        case Token::FALSE:      return new LiteralExpr(lex().location(), LiteralExpr::LIT_bool, Box(false));
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype: { \
            kind = LiteralExpr::LIT_##itype; \
            Box box = la().box(); \
            return new LiteralExpr(lex().location(), kind, box); \
        }
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

char Parser::char_value(const char*& p) {
    char value = 0;
    if (*p++ == '\\') {
        switch (*p++) {
        case '0':  value = '\0'; break;
        case 'n':  value = '\n'; break;
        case 't':  value = '\t'; break;
        case '\'': value = '\''; break;
        case '\"': value = '\"'; break;
        case '\\': value = '\\'; break;
        default:
            // TODO make location precise inside strings, reduce redundancy for single chars
            impala::error(la().location(), "expected valid escape sequence, got '\\%' while parsing %", *(p-1), la());
        }
    } else
        value = *(p-1);

    return value;
}

const CharExpr* Parser::parse_char_expr() {
    auto symbol = la().symbol();
    const char* p = symbol.str();
    assert(*p == '\'');
    ++p;
    char value = 0;
    if (*p != '\'') {
        value = char_value(p);

        if (*p++ != '\'')
            error("single character", "character constant");
        else
            assert(*p == '\0');
    } else
        error("a character", "character constant");

    return new CharExpr(lex().location(), symbol, value);
}

#if 0
const StrExpr* Parser::parse_str_expr() {
    auto str_expr = loc(new StrExpr());
    do {
        str_expr->symbols_.emplace_back(la().symbol());

        const char* p = str_expr->symbols_.back().str();
        assert(*p == '"');
        ++p;
        while (*p != '"')
            str_expr->values_.emplace_back(char_value(p));
        assert(p[1] == '\0');
        lex();
    } while (la() == Token::LIT_str);
    str_expr->values_.emplace_back('\0');
    return str_expr;
}

const FnExpr* Parser::parse_fn_expr() {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_expr = loc(new FnExpr());

    if (accept(Token::OR))
        parse_param_list(fn_expr->params_, Token::OR, true);
    else
        expect(Token::OROR, "parameter list of function expression");

    parse_return_param(fn_expr);
    // TODO pull this up into Fn - it's missing for parse_fn_decl
    fn_expr->ret_var_handle_ = cur_var_handle++; // reserve one handle - we might later on add another return param
    dock(fn_expr->body_, parse_expr());
    return fn_expr;
}
#endif

const IfExpr* Parser::parse_if_expr() {
    auto tracker = track();
    eat(Token::IF);
    auto cond =  parse_expr();
    auto then_expr = try_block_expr("consequence of an if expression");
    const Expr* else_expr = nullptr;
    if (accept(Token::ELSE)) {
        switch (la()) {
            case Token::IF:         else_expr =  parse_if_expr(); break;
            case Token::L_BRACE:
            case Token::RUN_BLOCK:  else_expr =  parse_block_expr(); break;
            default:
                error("block or if expression", "alternative of an if expression");
        }
    }

    if (else_expr == nullptr)
        else_expr = new BlockExpr(prev_loc());
    return new IfExpr(tracker, cond, then_expr, else_expr);
}

const ForExpr* Parser::parse_for_expr() {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);
    auto tracker = track();
    eat(Token::FOR);
    auto params = (la(0) == Token::IN || la(0) == Token::MUT || la(1) == Token::COLON || la(1) == Token::COMMA || la(1) == Token::IN)
                ? parse_param_list(Token::IN, true)
                : Params();
    params.emplace_back(new Param(prev_loc(), cur_var_handle++, new Identifier(prev_loc(), "continue"), nullptr));
    //fn_expr->is_continuation_ = false;
    auto expr = parse_expr();
    auto body = try_block_expr("body of for loop");
    auto break_decl = create_continuation_decl("break", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, ASTTypeParams(), std::move(params), body), expr, break_decl);
}

const ForExpr* Parser::parse_with_expr() {
    // With-expressions are like for-expressions except that
    // they have no continue statement, and their break statement
    // behaves just as a continue statement in a for-expression would
    auto tracker = track();
    eat(Token::WITH);
    auto params = (la(0) == Token::IN || la(0) == Token::MUT || la(1) == Token::COLON || la(1) == Token::COMMA || la(1) == Token::IN)
                ? parse_param_list(Token::IN, true)
                : Params();
    params.emplace_back(new Param(prev_loc(), cur_var_handle++, new Identifier(prev_loc(), "break"), nullptr));
    auto expr = parse_expr();
    auto body = try_block_expr("body of with statement");
    auto break_decl = create_continuation_decl("_", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, ASTTypeParams(), std::move(params), body), expr, break_decl);
}

const WhileExpr* Parser::parse_while_expr() {
    auto tracker = track();
    eat(Token::WHILE);
    auto continue_decl = create_continuation_decl("continue", true);
    auto cond = parse_expr();
    auto body = try_block_expr("body of while loop");
    auto break_decl = create_continuation_decl("break", true);
    return new WhileExpr(tracker, continue_decl, cond, body, break_decl);
}

const BlockExprBase* Parser::parse_block_expr() {
    assert(la() == Token::L_BRACE || la() == Token::RUN_BLOCK);
    auto tracker = track();
    bool run = accept(Token::RUN_BLOCK);
    Stmts stmts;
    const Expr* expr = nullptr;
    while (true) {
        switch (la()) {
            case Token::SEMICOLON:  lex(); continue; // ignore semicolon
            case STMT_NOT_EXPR:     stmts.emplace_back(parse_stmt_not_expr()); continue;
            case EXPR: {
                auto tracker = track();
                bool stmt_like = la().is_stmt_like();
                expr = parse_expr();
                if (accept(Token::SEMICOLON) || (stmt_like && la() != Token::R_BRACE)) {
                    stmts.emplace_back(new ExprStmt(tracker, expr));
                    continue;
                }
                // FALLTHROUGH
            }
            default:
                expect(Token::R_BRACE, "block expression");
                if (expr == nullptr)
                    expr = new EmptyExpr(prev_loc());
                if (run)
                    new RunBlockExpr(tracker, std::move(stmts), expr);
                else
                    new BlockExpr(tracker, std::move(stmts), expr);
        }
    }
    THORIN_UNREACHABLE;
    return nullptr;
}

const BlockExprBase* Parser::try_block_expr(const std::string& context) {
    switch (la()) {
        case Token::L_BRACE:
        case Token::RUN_BLOCK:
            return parse_block_expr();
        default:
            error("block expression", context);
            return new BlockExpr(prev_loc());
    }
}

/*
 * patterns
 */

const Ptrn* Parser::parse_ptrn() {
    if (la() == Token::L_PAREN)
        return parse_tuple_ptrn();
    else
        return parse_id_ptrn();
}

const TuplePtrn* Parser::parse_tuple_ptrn() {
    auto tracker = track();
    eat(Token::L_PAREN);
    Ptrns elems;
    parse_comma_list("closing parenthesis of tuple pattern", Token::R_PAREN, [&] {
        elems.emplace_back(parse_ptrn());
    });
    return new TuplePtrn(tracker, std::move(elems));
}

const IdPtrn* Parser::parse_id_ptrn() {
    auto tracker = track();
    auto mut = accept(Token::MUT);
    auto identifier = try_id("local variable in let binding");
    auto ast_type = accept(Token::COLON) ? parse_type() : nullptr;
    return new IdPtrn(new LocalDecl(tracker, cur_var_handle++, mut, identifier, ast_type));
}

/*
 * statements
 */

const Stmt* Parser::parse_stmt_not_expr() {
    switch (la()) {
        case ITEM:       return parse_item_stmt();
        case Token::LET: return parse_let_stmt();
        case Token::ASM: return parse_asm_stmt();
        default:         THORIN_UNREACHABLE;
    }
}

const LetStmt* Parser::parse_let_stmt() {
    auto tracker = track();
    eat(Token::LET);
    auto ptrn = parse_ptrn();
    auto init = accept(Token::ASGN) ? parse_expr() : nullptr;
    expect(Token::SEMICOLON, "the end of an let statement");
    return new LetStmt(tracker, ptrn, init);
}

const ItemStmt* Parser::parse_item_stmt() {
    auto tracker = track();
    auto item = parse_item();
    return new ItemStmt(tracker, item);
}

const AsmStmt::Elem* Parser::parse_asm_op() {
    auto tracker = track();
    auto constraint = parse_str();
    auto expr = parse_expr();
    return new AsmStmt::Elem(tracker, std::move(constraint), expr);
}

const AsmStmt* Parser::parse_asm_stmt() {
    static const Array<TokenKind> delimiters = {Token::COLON, Token::DOUBLE_COLON, Token::R_PAREN};

    auto tracker = track();
    eat(Token::ASM);
    expect(Token::L_PAREN, "asm statement");
    auto asm_template = parse_str();
    AsmStmt::Elems outputs, inputs;
    Strings clobbers, options;

    if (accept(Token::COLON))        goto parse_outputs;
    if (accept(Token::DOUBLE_COLON)) goto parse_inputs;
    if (accept(Token::R_PAREN))      goto out;

parse_outputs:
    nibble_comma_list(delimiters, [&]{ outputs.emplace_back(parse_asm_op()); });
    if (accept(Token::COLON))        goto parse_inputs;
    if (accept(Token::DOUBLE_COLON)) goto parse_clobbers;
    if (accept(Token::R_PAREN))      goto out;

parse_inputs:
    nibble_comma_list(delimiters, [&]{ inputs.emplace_back(parse_asm_op()); });
    if (accept(Token::COLON))        goto parse_clobbers;
    if (accept(Token::DOUBLE_COLON)) goto parse_options;
    if (accept(Token::R_PAREN))      goto out;

parse_clobbers:
    nibble_comma_list({Token::COMMA, Token::R_PAREN}, [&]{ clobbers.emplace_back(parse_str()); });
    if (accept(Token::COLON))        goto parse_options;
    expect(Token::R_PAREN, "asm statement");
    goto out;

parse_options:
    parse_comma_list("asm statement", Token::R_PAREN, [&]{ options.emplace_back(parse_str()); });

out:
    return new AsmStmt(tracker, std::move(asm_template), std::move(outputs), std::move(inputs),
                       std::move(clobbers), std::move(options));
}

// TODO: merge this code with StrExpr
std::string Parser::parse_str() {
    std::string str;
    do {
        std::string res = la().symbol().str() + 1;
        // replaces special characters
        for (size_t i = 0; i < res.length() - 1; ++i) {
            if (res[i] != '\\') {
                str += res[i];
                continue;
            }
            switch (res[++i]) {
                case 'n': str += '\n'; break;
                case 't': str += '\t'; break;
                case 'r': str += '\r'; break;
                default: assert(false); // TODO, more cases?
            }
        }
        lex();
    } while (la() == Token::LIT_str);
    return str;
}

}
