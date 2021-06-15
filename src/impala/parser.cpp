#include <algorithm>
#include <functional>
#include <sstream>

#include "thorin/util/array.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/lexer.h"

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
    case Token::RUN: \
    case Token::RUNKNOWN: \
    case Token::RUNRUN: \
    case Token::OR: \
    case Token::OROR: \
    case Token::ID: \
    case Token::HLT: \
    case Token::KNOWN: \
    case Token::IF: \
    case Token::MATCH: \
    case Token::FOR: \
    case Token::WITH: \
    case Token::WHILE: \
    case Token::L_PAREN: \
    case Token::L_BRACE: \
    case Token::L_BRACKET: \
    case Token::SIMD: \
    case Token::REV_DIFF

#define STMT \
         Token::LET: \
    case Token::ASM: \
    case ITEM: \
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
    {
        lookahead_[0] = lexer_.lex();
        lookahead_[1] = lexer_.lex();
        lookahead_[2] = lexer_.lex();
        prev_loc_ = Loc(filename, 1, 1, 1, 1);
    }

    const Token& lookahead(size_t i = 0) const { assert(i < 3); return lookahead_[i]; }
    Loc prev_loc() const { return prev_loc_; }

#ifdef NDEBUG
    Token eat(TokenTag) { return lex(); }
#else
    Token eat(TokenTag tag) { assert(tag == lookahead() && "internal parser error"); return lex(); }
#endif

    bool accept(TokenTag tok);
    bool expect(TokenTag tok, const std::string& context);
    void error(const std::string& what, const std::string& context) { error(what, context, lookahead()); }
    void error(const std::string& what, const std::string& context, const Token& tok);

    class Tracker {
    public:
        Tracker(Parser& parser, const Loc& loc)
            : parser_(parser), loc_(loc)
        {}

        operator Loc() const { return {loc_.begin(), parser_.prev_loc().finis()}; }

    private:
        Parser& parser_;
        Loc loc_;
    };

    Tracker track() { return Tracker(*this, lookahead().loc().begin()); }
    Tracker track(const Loc& loc) { return Tracker(*this, loc); }

    template<class T, class... Args>
    const T* create(Args&&... args) { return new T(prev_loc(), std::forward<Args>(args)...); }

    /**
     * Parses a list of comma-separated items till one of the @p delimiters have been found.
     * The ending delimiter will @em not be eaten up by this method.
     * The list may also end with a comma.
     */
    void nibble_comma_list(Array<TokenTag> delimiters, std::function<void()> f) {
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
    void parse_comma_list(const char* context, TokenTag delimiter, std::function<void()> f) {
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
    Params parse_param_list(TokenTag delimiter, bool lambda);
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
    const ASTTypeApp*   parse_ast_type_app();

    enum class BodyMode { None, Optional, Mandatory };

    // items + helpers
    const Item*        parse_item();
    void               parse_items(Items&);
    const StaticItem*  parse_static_item(Tracker, Visibility);
    const EnumDecl*    parse_enum_decl(Tracker, Visibility);
    const OptionDecl*  parse_option_decl(const size_t);
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
    const Expr*         parse_expr(Prec prec);
    const Expr*         parse_expr() { return parse_expr(Prec::Bottom); }
    const Expr*         parse_prefix_expr();
    const Expr*         parse_infix_expr(Tracker, const Expr* lhs);
    const Expr*         parse_postfix_expr(Tracker, const Expr* lhs);
    const MapExpr*      parse_map_expr(Tracker, const Expr* lhs);
    const TypeAppExpr*  parse_type_app_expr(Tracker, const Expr* lhs);
    const Expr*         parse_primary_expr();
    const LiteralExpr*  parse_literal_expr();
    const CharExpr*     parse_char_expr();
    const StrExpr*      parse_str_expr();
    const FnExpr*       parse_fn_expr(bool nested = false);
    const IfExpr*       parse_if_expr();
    const MatchExpr*    parse_match_expr();
    const ForExpr*      parse_for_expr();
    const ForExpr*      parse_with_expr();
    const WhileExpr*    parse_while_expr();
    const BlockExpr*    parse_block_expr();
    const RevDiffExpr*  parse_rev_diff_expr();
    const BlockExpr*    try_block_expr(const std::string& context);
    const Expr*         parse_filter(const char* context);

    // patterns
    const Ptrn*        parse_ptrn();
    const TuplePtrn*   parse_tuple_ptrn();
    const IdPtrn*      parse_id_ptrn(const Identifier*);
    const EnumPtrn*    parse_enum_ptrn(const Path*);
    const LiteralPtrn* parse_literal_ptrn();
    const CharPtrn*    parse_char_ptrn();

    // statements
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
        return create<LocalDecl>(identifier, ast_type);
    }

    Lexer lexer_;        ///< invoked in order to get next token
    Token lookahead_[3]; ///< SLL(3) look ahead
    Loc prev_loc_;
};

//------------------------------------------------------------------------------

void parse(Items& items, std::istream& is, const char* filename) {
    Parser parser(is, filename);
    parser.parse_items(items);
    if (parser.lookahead() != Token::Eof)
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
    prev_loc_ = result.loc(); // remember previous loc
    return result;
}

bool Parser::accept(TokenTag type) {
    if (type != lookahead())
        return false;
    lex();
    return true;
}

bool Parser::expect(TokenTag tok, const std::string& context) {
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
    impala::error(tok.loc(), "expected {}, got '{}'{}", what, tok,
            context.empty() ? "" : std::string(" while parsing ") + context.c_str());
}

const Identifier* Parser::try_identifier(const std::string& what) {
    Token name;
    if (lookahead() == Token::ID)
        name = lex();
    else {
        error("identifier", what);
        name = Token(lookahead().loc(), "<error>");
    }

    return new Identifier(name);
}

Visibility Parser::parse_visibility() {
    switch (lookahead()) {
        case VISIBILITY: return Visibility(lex().tag());
        default:         return Visibility(Visibility::None);
    }
}

uint64_t Parser::parse_integer(const char* what) {
    switch (lookahead()) {
        case Token::LIT_i8:  return lex().get< s8>();
        case Token::LIT_i16: return lex().get<s16>();
        case Token::LIT_i32: return lex().get<s32>();
        case Token::LIT_i64: return lex().get<s64>();
        case Token::LIT_u8:  return lex().get< u8>();
        case Token::LIT_u16: return lex().get<u16>();
        case Token::LIT_u32: return lex().get<u32>();
        case Token::LIT_u64: return lex().get<u64>();
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

Params Parser::parse_param_list(TokenTag delimiter, bool lambda) {
    Params params;
    int i = 0;
    parse_comma_list("parameter list", delimiter, [&] { params.emplace_back(parse_param(i++, lambda)); });
    return params;
}

const Param* Parser::parse_param(int /*i*/, bool lambda) {
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
                identifier = new Identifier(tok.loc(), "<error>");
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
            type = new ASTTypeApp(tok.loc(), new Path(identifier));
            identifier = nullptr;
        }
        ast_type = type;
    }

    if (identifier == nullptr)
        identifier = create<Identifier>("_");

    return new Param(tracker, mut, identifier, ast_type);
}

const Param* Parser::parse_return_param() {
    bool is_continuation;
    auto fn_type = parse_return_type(is_continuation, /*mandatory*/ false);

    if (!is_continuation) {
        auto loc = fn_type ? fn_type->loc() : prev_loc();
        return new Param(loc, new Identifier(loc, "return"), fn_type);
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

const EnumDecl* Parser::parse_enum_decl(Tracker tracker, Visibility vis) {
    eat(Token::ENUM);
    auto identifier = try_identifier("enum declaration");
    auto ast_type_params = parse_ast_type_params();
    expect(Token::L_BRACE, "enum declaration");
    size_t i = 0;
    OptionDecls option_decls;
    parse_comma_list("closing brace of enum declaration", Token::R_BRACE, [&] {
        option_decls.emplace_back(parse_option_decl(i++));
    });
    return new EnumDecl(tracker, vis, identifier, std::move(ast_type_params), std::move(option_decls));
}

const OptionDecl* Parser::parse_option_decl(const size_t i) {
    auto tracker = track();
    auto identifier = try_identifier("enum option");
    ASTTypes args;
    if (lookahead() == Token::L_PAREN) {
        eat(Token::L_PAREN);
        parse_comma_list("closing parenthesis of enum option", Token::R_PAREN, [&] {
            args.emplace_back(parse_type());
        });
    }
    return new OptionDecl(tracker, i, identifier, std::move(args));
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
    eat(Token::FN);
    auto export_name = lookahead() == Token::LIT_str ? lex().symbol() : Symbol();

    const Expr* filter = parse_filter("partial evaluation filter of function declaration");
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

    return new FnDecl(tracker, vis, is_extern, abi, filter, export_name, identifier,
                      std::move(ast_type_params), std::move(params), body);
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
    ASTTypes ast_types;
    is_continuation = false;
    if (accept(Token::ARROW)) {
        if (accept(Token::NOT)) {
            is_continuation = true;
            return nullptr;
        }

        ast_types.emplace_back(parse_type());
        return new FnASTType(tracker, std::move(ast_types));
    }

    if (mandatory) {
        error("return type", "function type");
        ast_types.emplace_back(new ErrorASTType(tracker));
        return new FnASTType(tracker, std::move(ast_types));
    }

    return nullptr;
}

const PrimASTType* Parser::parse_prim_type() {
    auto tracker = track();
    auto tag = (PrimASTType::Tag) lex().tag();
    return new PrimASTType(tracker, tag);
}

const PtrASTType* Parser::parse_ptr_type() {
    auto tracker = track();
    if (accept(Token::ANDAND)) {
        auto tag = accept(Token::MUT) ? PtrASTType::Mut : PtrASTType::Borrowed;
        auto addr_space = parse_addr_space();
        auto referenced_ast_type = parse_type();
        return new PtrASTType(tracker, PtrASTType::Borrowed, 0, new PtrASTType(tracker, tag, addr_space, referenced_ast_type));
    }

    PtrASTType::Tag tag;
    if (accept(Token::TILDE))
        tag = PtrASTType::Owned;
    else {
        eat(Token::AND);
        if (accept(Token::MUT))
            tag = PtrASTType::Mut;
        else
            tag = PtrASTType::Borrowed;
    }

    auto addr_space = parse_addr_space();
    auto referenced_ast_type = parse_type();
    return new PtrASTType(tracker, tag, addr_space, referenced_ast_type);
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

/*
 * expressions
 */

const Expr* Parser::parse_expr(Prec prec) {
    auto tracker = track();
    auto lhs = lookahead().is_prefix() ? parse_prefix_expr() : parse_primary_expr();

    while (true) {
        /*
         * (lhs  op  LA) op ...  on break  (current prec > lhs prec of LA)  -->  reduce
         *  lhs  op (LA  op ...) otherwise                                  -->  shift
         */

        if (lookahead().is_infix()) {
            if (prec > PrecTable::infix_l(lookahead()))
                break;

            lhs = parse_infix_expr(tracker, lhs);
        } else if (lookahead().is_postfix()) {
            if (prec > Prec::Unary)
                break;

            lhs = parse_postfix_expr(tracker, lhs);
        } else
            break;
    }

    return lhs;
}

const Expr* Parser::parse_prefix_expr() {
    if (lookahead() == Token::OR || lookahead() == Token::OROR || lookahead() == Token::RUN)
        return parse_fn_expr();

    auto tracker = track();
    auto tag = lex().tag();
    bool mut = tag == Token::AND ? accept(Token::MUT) : false;
    Prec prec;
    switch (tag) {
        case Token::HLT:    prec = Prec::Hlt; break;
        case Token::RUNRUN: prec = Prec::RunRun; break;
        default:            prec = Prec::Unary;
    }
    auto rhs = parse_expr(prec);

    return new PrefixExpr(tracker, mut ? PrefixExpr::MUT : (PrefixExpr::Tag) tag, rhs);
}

const Expr* Parser::parse_infix_expr(Tracker tracker, const Expr* lhs) {
    auto tag = lex().tag();
    if (tag == Token::AS)
        return new ExplicitCastExpr(tracker, lhs, parse_type());
    auto rhs = parse_expr(PrecTable::infix_r(tag));
    return new InfixExpr(tracker, lhs, (InfixExpr::Tag) tag, rhs);
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
            auto tag = (PostfixExpr::Tag) lex().tag();
            return new PostfixExpr(tracker, lhs, tag);
        }
        case Token::DOT: {
            lex();
            auto identifier = try_identifier("field expression");
            return new FieldExpr(tracker, lhs, identifier);
        }
        default: THORIN_UNREACHABLE;
    }
}

const Expr* Parser::parse_primary_expr() {
    auto tracker = track();
    switch (lookahead()) {
        case Token::R_PAREN: return new TupleExpr(tracker, {});
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
                }
            }
            // lookahead required because of if cond { expr }
            if (lookahead(0) == Token::L_BRACE &&
                ((lookahead(1) == Token::ID && lookahead(2) == Token::COLON) ||
                  lookahead(1) == Token::R_BRACE)) {
                eat(Token::L_BRACE);

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
            return new PathExpr(path);
        }
        case Token::IF:            return parse_if_expr();
        case Token::MATCH:         return parse_match_expr();
        case Token::FOR:           return parse_for_expr();
        case Token::WITH:          return parse_with_expr();
        case Token::WHILE:         return parse_while_expr();
        case Token::L_BRACE:       return parse_block_expr();
        case Token::REV_DIFF:      return parse_rev_diff_expr();
        default:                   error("expression", ""); return new EmptyExpr(lex().loc());
    }
}

const LiteralExpr* Parser::parse_literal_expr() {
    LiteralExpr::Tag tag;

    switch (lookahead()) {
        case Token::TRUE:       return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, 1);
        case Token::FALSE:      return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, 0);
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype: { \
            tag = LiteralExpr::LIT_##itype; \
            auto val = lookahead().get(); \
            return new LiteralExpr(lex().loc(), tag, val); \
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
            // TODO make loc precise inside strings, reduce redundancy for single chars
            impala::error(lookahead().loc(), "expected valid escape sequence, got '\\{}' while parsing {}", *(p-1), lookahead());
        }
    } else
        value = *(p-1);

    return value;
}

const CharExpr* Parser::parse_char_expr() {
    auto symbol = lookahead().symbol();
    const char* p = symbol.c_str();
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

    return new CharExpr(lex().loc(), symbol, value);
}

const StrExpr* Parser::parse_str_expr() {
    auto tracker = track();
    Symbols symbols;
    std::vector<char> values;
    do {
       symbols.emplace_back(lookahead().symbol());

        const char* p = symbols.back().c_str();
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

const FnExpr* Parser::parse_fn_expr(bool nested) {
    auto tracker = track();

    const Expr* filter = nullptr;
    if (nested)
        filter = new LiteralExpr(lookahead().loc(), LiteralExpr::LIT_bool, false);
    else
        filter = parse_filter("partial evaluation filter of function expression");

    Params params;
    if (accept(Token::OR) || nested) {
        // cannot use parse_param_list here because the list may end with OR or OROR
        int i = 0;
        nibble_comma_list({ Token::OR, Token::OROR }, [&] {
            params.emplace_back(parse_param(i++, true));
        });
        // special case for nested lambdas (e.g. |x||y| x + y)
        if (accept(Token::OROR)) {
            params.emplace_back(parse_return_param());
            auto body = parse_fn_expr(true);
            return new FnExpr(tracker, filter, std::move(params), body);
        }
        expect(Token::OR, "parameter list of function expression");
    } else
        expect(Token::OROR, "parameter list of function expression");

    if (auto ret_param = parse_return_param())
        params.emplace_back(ret_param);

    auto body = parse_expr();

    return new FnExpr(tracker, filter, std::move(params), body);
}

const IfExpr* Parser::parse_if_expr() {
    auto tracker = track();
    eat(Token::IF);
    auto cond = parse_expr();
    auto then_expr = try_block_expr("consequence of an if expression");
    const Expr* else_expr = nullptr;
    if (accept(Token::ELSE)) {
        switch (lookahead()) {
            case Token::IF:      else_expr =  parse_if_expr(); break;
            case Token::L_BRACE: else_expr =  parse_block_expr(); break;
            default:
                error("block or if expression", "alternative of an if expression");
        }
    }

    if (else_expr == nullptr)
        else_expr = create<BlockExpr>();
    return new IfExpr(tracker, cond, then_expr, else_expr);
}

const MatchExpr* Parser::parse_match_expr() {
    auto tracker = track();
    eat(Token::MATCH);
    auto expr = parse_expr();
    expect(Token::L_BRACE, "match expression");
    MatchExpr::Arms arms;
    parse_comma_list("closing brace of match expression", Token::R_BRACE, [&] {
        auto tracker = track();
        auto ptrn = parse_ptrn();
        expect(Token::FAT_ARRROW, "pattern of match expression");
        auto expr = parse_expr();
        arms.emplace_back(new MatchExpr::Arm(tracker, ptrn, expr));
    });
    return new MatchExpr(tracker, expr, std::move(arms));
}

const ForExpr* Parser::parse_for_expr() {
    auto tracker = track();
    eat(Token::FOR);
    auto params = param_list() ? parse_param_list(Token::IN, true) : Params();
    params.emplace_back(create<Param>(create<Identifier>("continue"), nullptr));
    auto expr = parse_expr();
    auto filter = parse_filter("partial evaluation filter of for loop");
    auto body = try_block_expr("body of for loop");
    auto break_decl = create_continuation_decl("break", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, filter, std::move(params), body), expr, break_decl);
}

const ForExpr* Parser::parse_with_expr() {
    // With-expressions are like for-expressions except that
    // they have no continue statement, and their break statement
    // behaves just as a continue statement in a for-expression would
    auto tracker = track();
    eat(Token::WITH);
    auto params = param_list() ? parse_param_list(Token::IN, true) : Params();
    params.emplace_back(create<Param>(create<Identifier>("break"), nullptr));
    auto expr = parse_expr();
    auto filter = parse_filter("partial evaluation filter of with statement");
    auto body = try_block_expr("body of with statement");
    auto break_decl = create_continuation_decl("_", /*set type during InferSema*/ false);
    return new ForExpr(tracker, new FnExpr(tracker, filter, std::move(params), body), expr, break_decl);
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

const BlockExpr* Parser::parse_block_expr() {
    auto tracker = track();
    eat(Token::L_BRACE);
    Stmts stmts;
    const Expr* final_expr = nullptr;
    while (true) {
        switch (lookahead()) {
            case Token::SEMICOLON: lex(); continue; // ignore semicolon
            case ITEM:             stmts.emplace_back(parse_item_stmt()); continue;
            case Token::LET:       stmts.emplace_back(parse_let_stmt()); continue;
            case Token::ASM:       stmts.emplace_back(parse_asm_stmt()); continue;
            case EXPR: {
                auto tracker = track();
                const Expr* expr;
                bool stmt_like = true;
                switch (lookahead()) {
                    case Token::IF:            expr = parse_if_expr(); break;
                    case Token::MATCH:         expr = parse_match_expr(); break;
                    case Token::FOR:           expr = parse_for_expr(); break;
                    case Token::WITH:          expr = parse_with_expr(); break;
                    case Token::WHILE:         expr = parse_while_expr(); break;
                    case Token::L_BRACE:       expr = parse_block_expr(); break;
                    case Token::REV_DIFF:      expr = parse_rev_diff_expr(); break;
                    default:                   expr = parse_expr(); stmt_like = false;
                }

                if (accept(Token::SEMICOLON) || (stmt_like && lookahead() != Token::R_BRACE)) {
                    stmts.emplace_back(new ExprStmt(tracker, expr));
                    continue;
                }

                final_expr = expr;
                [[fallthrough]];
            }
            default:
                expect(Token::R_BRACE, "block expression");
                if (final_expr == nullptr)
                    final_expr = create<EmptyExpr>();
                return new BlockExpr(tracker, std::move(stmts), final_expr);
        }
    }
}

const BlockExpr* Parser::try_block_expr(const std::string& context) {
    switch (lookahead()) {
        case Token::L_BRACE:
            return parse_block_expr();
        default:
            error("block expression", context);
            return create<BlockExpr>();
    }
}

const Expr* Parser::parse_filter(const char* context) {
    const Expr* filter = nullptr;
    if (accept(Token::RUN)) {
        if (lookahead() == Token::L_PAREN) {
            eat(Token::L_PAREN);
            filter = parse_expr();
            expect(Token::R_PAREN, context);
        } else {
            filter = new LiteralExpr(lookahead().loc(), LiteralExpr::LIT_bool, true);
        }
    } else
        filter = new LiteralExpr(lookahead().loc(), LiteralExpr::LIT_bool, false);

    return filter;
}

const RevDiffExpr *Parser::parse_rev_diff_expr() {
    auto tracker = track();

    eat(Token::REV_DIFF);
    expect(Token::L_PAREN, "grad expression");
    auto expr = parse_expr();
    expect(Token::R_PAREN, "grad expression");
    return new RevDiffExpr(tracker, expr);
}

/*
 * patterns
 */

const Ptrn* Parser::parse_ptrn() {
    switch (lookahead()) {
        case Token::SUB:
        case Token::TRUE:
        case Token::FALSE:
#define IMPALA_LIT(itype, atype) case Token::LIT_##itype:
#include "impala/tokenlist.h"
            return parse_literal_ptrn();

        case Token::LIT_char: return parse_char_ptrn();
        case Token::L_PAREN:  return parse_tuple_ptrn();
        case Token::MUT:      return parse_id_ptrn(nullptr);
        default: {
            std::unique_ptr<const Path> path(parse_path());
            if (lookahead() == Token::L_PAREN   ||
                lookahead() == Token::L_BRACKET ||
                path->num_elems() > 1) {
                return parse_enum_ptrn(path.release());
            }
            auto id = path->elem(0)->identifier();
            return parse_id_ptrn(new Identifier(path->loc(), id->symbol()));
        }
    }
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

const IdPtrn* Parser::parse_id_ptrn(const Identifier* id) {
    auto tracker = id ? track(id->loc()) : track();
    auto mut = id ? false : accept(Token::MUT);
    auto identifier = id ? id : try_identifier("local variable in let binding");
    auto ast_type = accept(Token::COLON) ? parse_type() : nullptr;
    return new IdPtrn(new LocalDecl(tracker, mut, identifier, ast_type));
}

const EnumPtrn* Parser::parse_enum_ptrn(const Path* path) {
    auto tracker = track(path->loc());
    Ptrns args;
    if (lookahead() == Token::L_PAREN) {
        eat(Token::L_PAREN);
        parse_comma_list("closing parenthesis of enum pattern", Token::R_PAREN, [&] {
            args.emplace_back(parse_ptrn());
        });
    }
    return new EnumPtrn(tracker, path, std::move(args));
}

const LiteralPtrn* Parser::parse_literal_ptrn() {
    bool minus = accept(Token::SUB);
    return new LiteralPtrn(parse_literal_expr(), minus);
}

const CharPtrn* Parser::parse_char_ptrn() {
    return new CharPtrn(parse_char_expr());
}

/*
 * statements
 */

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
    static const Array<TokenTag> delimiters = {Token::COLON, Token::DOUBLE_COLON, Token::R_PAREN};

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
        std::string res = lookahead().symbol().c_str() + 1;
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
