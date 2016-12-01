#include <algorithm>
#include <functional>
#include <sstream>

#include "thorin/util/array.h"
#include "thorin/util/assert.h"

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
        prev_location_ = Location(filename, 1, 1, 1, 1);
    }

    const Token& lookahead(size_t i = 0) const { assert(i < 3); return lookahead_[i]; }
    Location prev_location() const { return prev_location_; }

#ifdef NDEBUG
    Token eat(TokenKind) { return lex(); }
#else
    Token eat(TokenKind kind) { assert(kind == lookahead() && "internal parser error"); return lex(); }
#endif

    bool accept(TokenKind tok);
    bool expect(TokenKind tok, const std::string& context);
    void error(const std::string& what, const std::string& context) { error(what, context, lookahead()); }
    void error(const std::string& what, const std::string& context, const Token& tok);

    class Tracker {
    public:
        Tracker(Parser& parser)
            : parser_(parser)
            , location_(parser_.lookahead().location().begin())
        {}

        operator Location() const { return {location_.begin(), parser_.prev_location().end()}; }

    private:
        Parser& parser_;
        Location location_;
    };

    Tracker track() { return Tracker(*this); }

    template<class T, class... Args>
    const T* create(Args&&... args) { return new T(prev_location(), std::forward<Args>(args)...); }

    /**
     * Parses a list of comma-separated items till one of the @p delimiters have been found.
     * The ending delimiter will @em not be eaten up by this method.
     * The list may also end with a comma.
     */
    void nibble_comma_list(Array<TokenKind> delimiters, std::function<void()> f) {
        auto is_delimiter = [&] () {
            for (auto delimiter : delimiters)
                if (lookahead() == delimiter)
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
    const Identifier* try_identifier(const std::string& what);
    Visibility parse_visibility();
    uint64_t parse_integer(const char* what);
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
    const Param* parse_return_param();

    bool param_list() {
        return lookahead(0) == Token::IN || lookahead(0) == Token::MUT
            || lookahead(1) == Token::COLON || lookahead(1) == Token::COMMA || lookahead(1) == Token::IN;
    }

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
    void               parse_items(Items&);
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
        auto identifier = create<Identifier>(name);
        auto ast_type = set_type ? create<FnASTType>() : nullptr;
        return create<LocalDecl>(cur_var_handle++, identifier, ast_type);
    }

    Lexer lexer_;        ///< invoked in order to get next token
    Token lookahead_[3]; ///< SLL(3) look ahead
    size_t cur_var_handle;
    bool no_bars_;
    Location prev_location_;
};

//------------------------------------------------------------------------------

void parse(Items& items, std::istream& is, const char* filename) {
    Parser parser(is, filename);
    parser.parse_items(items);
    if (parser.lookahead() != Token::END_OF_FILE)
        parser.error("module item", "module contents");
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
    prev_location_ = result.location(); // remember previous location
    return result;
}

bool Parser::accept(TokenKind type) {
    if (type != lookahead())
        return false;
    lex();
    return true;
}

bool Parser::expect(TokenKind tok, const std::string& context) {
    if (lookahead() == tok) {
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

const Identifier* Parser::try_identifier(const std::string& what) {
    Token name;
    if (lookahead() == Token::ID)
        name = lex();
    else {
        error("identifier", what);
        name = Token(lookahead().location(), "<error>");
    }

    return new Identifier(name);
}

Visibility Parser::parse_visibility() {
    switch (lookahead()) {
        case VISIBILITY: return Visibility(lex().kind());
        default:         return Visibility(Visibility::None);
    }
}

uint64_t Parser::parse_integer(const char* what) {
    switch (lookahead()) {
        case Token::LIT_i8:  return lex().box().get_s8();
        case Token::LIT_i16: return lex().box().get_s16();
        case Token::LIT_i32: return lex().box().get_s32();
        case Token::LIT_i64: return lex().box().get_s64();
        case Token::LIT_u8:  return lex().box().get_u8();
        case Token::LIT_u16: return lex().box().get_u16();
        case Token::LIT_u32: return lex().box().get_u32();
        case Token::LIT_u64: return lex().box().get_u64();
        default:
            error("integer literal", what);
            return 0;
    }
}

int Parser::parse_addr_space() {
    if (lookahead(0) == Token::L_BRACKET && lookahead(1) == Token::LIT_i32) {
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
    return new Path::Elem(try_identifier("path"));
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
    auto identifier = try_identifier("type parameter");
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
    Token tok = lookahead();

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
        identifier = create<Identifier>(oss.str().c_str());
    }

    return new Param(tracker, cur_var_handle++, mut, identifier, ast_type);
}

const Param* Parser::parse_return_param() {
    bool is_continuation;
    auto fn_type = parse_return_type(is_continuation, /*mandatory*/ false);

    if (!is_continuation) {
        auto location = fn_type ? fn_type->location() : prev_location();
        return new Param(location, cur_var_handle++, new Identifier(location, "return"), fn_type);
    } else
        return nullptr;
}

/*
 * items
 */

const Item* Parser::parse_item() {
    auto tracker = track();
    auto vis = parse_visibility();

    switch (lookahead()) {
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
    eat(Token::EXTERN);
    if (lookahead() == Token::FN)
        return parse_fn_decl(BodyMode::Mandatory, tracker, vis, /*extern*/ true, /*abi*/ "");

    Symbol abi;
    if (lookahead() == Token::LIT_str)
        abi = lex().symbol();

    expect(Token::L_BRACE, "opening brace of external block");
    FnDecls fn_decls;
    while (lookahead() == Token::FN)
        fn_decls.emplace_back(parse_fn_decl(BodyMode::None, tracker, vis, /*extern*/ true, abi));
    expect(Token::R_BRACE, "closing brace of external block");

    return new ExternBlock(tracker, vis, abi, std::move(fn_decls));
}

const FnDecl* Parser::parse_fn_decl(BodyMode mode, Tracker tracker, Visibility vis, bool is_extern, Symbol abi) {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);

    eat(Token::FN);
    auto export_name = lookahead() == Token::LIT_str ? lex().symbol() : Symbol();
    auto identifier = try_identifier("function name");
    auto ast_type_params = parse_ast_type_params();
    expect(Token::L_PAREN, "function head");
    auto params = parse_param_list(Token::R_PAREN, false);
    if (auto ret_param = parse_return_param())
        params.emplace_back(ret_param);

    const Expr* body = nullptr;
    switch (mode) {
        case BodyMode::None:      expect(Token::SEMICOLON, "function declaration"); break;
        case BodyMode::Mandatory: body = try_block_expr("body of function"); break;
        case BodyMode::Optional:
            if (!accept(Token::SEMICOLON))
                body = try_block_expr("body of function");
            break;
    }

    return new FnDecl(tracker, vis, is_extern, abi, export_name, identifier, std::move(ast_type_params),
                      std::move(params), body);
}

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
    while (lookahead() == Token::FN)
        methods.emplace_back(parse_fn_decl(BodyMode::Mandatory, tracker, vis, /*exter*/ false, /*abi*/ ""));
    expect(Token::R_BRACE, "closing brace of impl");

    return new ImplItem(tracker, vis, std::move(ast_type_params), trait, ast_type, std::move(methods));
}

const Item* Parser::parse_module_or_module_decl(Tracker tracker, Visibility vis) {
    eat(Token::MOD);
    auto identifier = try_identifier("module declaration");
    auto ast_type_params = parse_ast_type_params();
    if (accept(Token::L_BRACE)) {
        Items items;
        parse_items(items);
        expect(Token::R_BRACE, "module");
        return new Module(tracker, vis, identifier, std::move(ast_type_params), std::move(items));
    } else {
        expect(Token::SEMICOLON, "module declaration");
        return new ModuleDecl(tracker, vis, identifier, std::move(ast_type_params));
    }
}

void Parser::parse_items(Items& items) {
    while (true) {
        cur_var_handle = 2; // HACK
        switch (lookahead()) {
            case VISIBILITY:
            case ITEM:
                items.emplace_back(parse_item());
                continue;
            case Token::SEMICOLON:
                lex();
                continue;
            default:
                return;
        }
    }
}

const StaticItem* Parser::parse_static_item(Tracker tracker, Visibility vis) {
    eat(Token::STATIC);
    bool mut = accept(Token::MUT);
    auto identifier = try_identifier("static item");
    auto ast_type = accept(Token::COLON) ? parse_type() : nullptr;
    auto init = accept(Token::ASGN) ? parse_expr() : nullptr;
    expect(Token::SEMICOLON, "static item");
    return new StaticItem(tracker, vis, mut, identifier, ast_type, init);
}

const StructDecl* Parser::parse_struct_decl(Tracker tracker, Visibility vis) {
    eat(Token::STRUCT);
    auto identifier = try_identifier("struct declaration");
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
    auto identifier = try_identifier("struct field");
    expect(Token::COLON, "struct field");
    auto ast_type = parse_type();
    return new FieldDecl(tracker, i, vis, identifier, ast_type);
}

const TraitDecl* Parser::parse_trait_decl(Tracker tracker, Visibility vis) {
    eat(Token::TRAIT);
    auto identifier = try_identifier("trait declaration");
    auto ast_type_params = parse_ast_type_params();

    ASTTypeApps super_traits;
    if (accept(Token::COLON)) {
        parse_comma_list("trait declaration", Token::L_BRACE, [&] {
            super_traits.emplace_back(parse_ast_type_app());
        });
    }

    expect(Token::L_BRACE, "trait declaration");
    FnDecls methods;
    while (lookahead() == Token::FN)
        methods.emplace_back(parse_fn_decl(BodyMode::Optional, tracker, vis, /*exter*/ false, /*abi*/ ""));
    expect(Token::R_BRACE, "closing brace of trait declaration");

    return new TraitDecl(tracker, vis, identifier, std::move(ast_type_params), std::move(super_traits), std::move(methods));
}

const Typedef* Parser::parse_typedef(Tracker tracker, Visibility vis) {
    eat(Token::TYPEDEF);
    auto identifier = try_identifier("type definition");
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
    switch (lookahead()) {
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
            return create<ErrorASTType>();
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
    if (auto ret_type = parse_return_type(unused, /*mandatory*/ true))
        ast_type_args.emplace_back(ret_type);

    return new FnASTType(tracker, std::move(ast_type_params), std::move(ast_type_args));
}

const ASTType* Parser::parse_return_type(bool& is_continuation, bool mandatory) {
    auto tracker = track();
    ASTTypes ast_type_args;
    is_continuation = false;
    if (accept(Token::ARROW)) {
        if (accept(Token::NOT)) {
            is_continuation = true;
            return nullptr;
        }

        if (accept(Token::L_PAREN)) {   // in-place tuple
            parse_comma_list("closing parenthesis of return type list", Token::R_PAREN, [&] {
                ast_type_args.emplace_back(parse_type());
            });
        } else {
            auto type = parse_type();
            assert(!type->isa<TupleASTType>());
            ast_type_args.emplace_back(type);
        }

        return new FnASTType(tracker, std::move(ast_type_args));
    }

    if (mandatory) {
        error("return type", "function type");
        ast_type_args.emplace_back(new ErrorASTType(tracker));
        return new FnASTType(tracker, std::move(ast_type_args));
    }

    return nullptr;
}

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
    bool infix = lookahead().is_infix();
    if (no_bars_ && infix)
        return lookahead() != Token::OR && lookahead() != Token::OROR;
    return infix;
}

const Expr* Parser::parse_expr(Prec prec) {
    auto tracker = track();
    auto lhs = lookahead().is_prefix() ? parse_prefix_expr() : parse_primary_expr();

    if (lhs->isa<StmtLikeExpr>())
        return lhs; // bail out for stmt-like expressions

    while (true) {
        /*
         * (lhs  op  LA) op ...  on break  (current prec > lhs prec of LA)  -->  reduce
         *  lhs  op (LA  op ...) otherwise                                  -->  shift
         */

        if (is_infix()) {
            if (prec > PrecTable::infix_l[lookahead()])
                break;

            lhs = parse_infix_expr(tracker, lhs);
        } else if ( lookahead().is_postfix() ) {
            if (prec > PrecTable::postfix_l[lookahead()])
                break;

            lhs = parse_postfix_expr(tracker, lhs);
        } else
            break;
    }

    return lhs;
}

const Expr* Parser::parse_prefix_expr() {
    if (lookahead() == Token::OR || lookahead() == Token::OROR)
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
    switch (lookahead()) {
        case Token::L_BRACKET: return parse_type_app_expr(tracker, lhs);
        case Token::L_PAREN:   return parse_map_expr(tracker, lhs);
        case Token::DEC:
        case Token::INC: {
            auto kind = (PostfixExpr::Kind) lex().kind();
            return new PostfixExpr(tracker, lhs, kind);
        }
        case Token::DOT: {
            lex();
            auto identifier = try_identifier("field expression");
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
    switch (lookahead()) {
        case Token::L_PAREN: {
            lex();
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
            lex();
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
            lex();;
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
                        auto symbol = try_identifier("identifier in struct expression");
                        expect(Token::COLON, "struct expression");
                        elems.emplace_back(new StructExpr::Elem(tracker, symbol, parse_expr()));
                    });

                    return new StructExpr(tracker, ast_type_app, std::move(elems));
                }
            }
            if (lookahead(0) == Token::L_BRACE && (lookahead(1) == Token::ID && lookahead(2) == Token::COLON)) {
                eat(Token::L_BRACE);

                auto ast_type_app = new ASTTypeApp(tracker, path, ASTTypes());

                StructExpr::Elems elems;
                parse_comma_list("elements of struct expression", Token::R_BRACE, [&] {
                    auto tracker = track();
                    auto symbol = try_identifier("identifier in struct expression");
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

    switch (lookahead()) {
        case Token::TRUE:       return new LiteralExpr(lex().location(), LiteralExpr::LIT_bool, Box(true));
        case Token::FALSE:      return new LiteralExpr(lex().location(), LiteralExpr::LIT_bool, Box(false));
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype: { \
            kind = LiteralExpr::LIT_##itype; \
            Box box = lookahead().box(); \
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
            impala::error(lookahead().location(), "expected valid escape sequence, got '\\%' while parsing %", *(p-1), lookahead());
        }
    } else
        value = *(p-1);

    return value;
}

const CharExpr* Parser::parse_char_expr() {
    auto symbol = lookahead().symbol();
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

const StrExpr* Parser::parse_str_expr() {
    auto tracker = track();
    Symbols symbols;
    std::vector<char> values;
    do {
       symbols.emplace_back(lookahead().symbol());

        const char* p = symbols.back().str();
        assert(*p == '"');
        ++p;
        while (*p != '"')
            values.emplace_back(char_value(p));
        assert(p[1] == '\0');
        lex();
    } while (lookahead() == Token::LIT_str);
    values.emplace_back('\0');

    return new StrExpr(tracker, std::move(symbols), std::move(values));
}

const FnExpr* Parser::parse_fn_expr() {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);
    auto tracker = track();

    Params params;
    if (accept(Token::OR))
        params = parse_param_list(Token::OR, true);
    else
        expect(Token::OROR, "parameter list of function expression");

    if (auto ret_param = parse_return_param())
        params.emplace_back(ret_param);

    auto body = parse_expr();

    return new FnExpr(tracker, std::move(params), body);
}

const IfExpr* Parser::parse_if_expr() {
    auto tracker = track();
    eat(Token::IF);
    auto cond =  parse_expr();
    auto then_expr = try_block_expr("consequence of an if expression");
    const Expr* else_expr = nullptr;
    if (accept(Token::ELSE)) {
        switch (lookahead()) {
            case Token::IF:         else_expr =  parse_if_expr(); break;
            case Token::L_BRACE:
            case Token::RUN_BLOCK:  else_expr =  parse_block_expr(); break;
            default:
                error("block or if expression", "alternative of an if expression");
        }
    }

    if (else_expr == nullptr)
        else_expr = create<BlockExpr>();
    return new IfExpr(tracker, cond, then_expr, else_expr);
}

const ForExpr* Parser::parse_for_expr() {
    //THORIN_PUSH(cur_var_handle, cur_var_handle);
    auto tracker = track();
    eat(Token::FOR);
    auto params = param_list() ? parse_param_list(Token::IN, true) : Params();
    params.emplace_back(create<Param>(cur_var_handle++, create<Identifier>("continue"), nullptr));
    //fn_expr->is_continuation_ = false;
    auto expr = parse_expr();
    auto body = try_block_expr("body of for loop");
    auto break_decl = create_continuation_decl("break", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, std::move(params), body), expr, break_decl);
}

const ForExpr* Parser::parse_with_expr() {
    // With-expressions are like for-expressions except that
    // they have no continue statement, and their break statement
    // behaves just as a continue statement in a for-expression would
    auto tracker = track();
    eat(Token::WITH);
    auto params = param_list() ? parse_param_list(Token::IN, true) : Params();
    params.emplace_back(create<Param>(cur_var_handle++, create<Identifier>("break"), nullptr));
    auto expr = parse_expr();
    auto body = try_block_expr("body of with statement");
    auto break_decl = create_continuation_decl("_", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, std::move(params), body), expr, break_decl);
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
    auto tracker = track();
    bool run = accept(Token::RUN_BLOCK) ? true : (eat(Token::L_BRACE), false);
    Stmts stmts;
    const Expr* block_expr = nullptr;
    while (true) {
        switch (lookahead()) {
            case Token::SEMICOLON:  lex(); continue; // ignore semicolon
            case STMT_NOT_EXPR:     stmts.emplace_back(parse_stmt_not_expr()); continue;
            case EXPR: {
                auto tracker = track();
                bool stmt_like = lookahead().is_stmt_like();
                auto expr = parse_expr();
                if (accept(Token::SEMICOLON) || (stmt_like && lookahead() != Token::R_BRACE)) {
                    stmts.emplace_back(new ExprStmt(tracker, expr));
                    continue;
                }
                block_expr = expr;
                // FALLTHROUGH
            }
            default:
                expect(Token::R_BRACE, "block expression");
                if (block_expr == nullptr)
                    block_expr = create<EmptyExpr>();
                if (run)
                    return new RunBlockExpr(tracker, std::move(stmts), block_expr);
                else
                    return new BlockExpr(tracker, std::move(stmts), block_expr);
        }
    }
}

const BlockExprBase* Parser::try_block_expr(const std::string& context) {
    switch (lookahead()) {
        case Token::L_BRACE:
        case Token::RUN_BLOCK:
            return parse_block_expr();
        default:
            error("block expression", context);
            return create<BlockExpr>();
    }
}

/*
 * patterns
 */

const Ptrn* Parser::parse_ptrn() {
    if (lookahead() == Token::L_PAREN)
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
    auto identifier = try_identifier("local variable in let binding");
    auto ast_type = accept(Token::COLON) ? parse_type() : nullptr;
    return new IdPtrn(new LocalDecl(tracker, cur_var_handle++, mut, identifier, ast_type));
}

/*
 * statements
 */

const Stmt* Parser::parse_stmt_not_expr() {
    switch (lookahead()) {
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
        std::string res = lookahead().symbol().str() + 1;
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
    } while (lookahead() == Token::LIT_str);
    return str;
}

}
