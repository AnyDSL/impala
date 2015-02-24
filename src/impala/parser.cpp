#include <algorithm>
#include <functional>
#include <sstream>
#include <iostream>

#include "thorin/util/array.h"
#include "thorin/util/assert.h"
#include "thorin/util/push.h"

#include "impala/ast.h"
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
    case Token::WHILE: \
    case Token::SIZEOF: \
    case Token::L_PAREN: \
    case Token::L_BRACE: \
    case Token::RUN_BLOCK: \
    case Token::L_BRACKET

#define STMT_NOT_EXPR \
         Token::LET: \
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
    case Token::TYPE_f32: \
    case Token::TYPE_f64:  \
    case Token::TYPE_bool: \
    case Token::TYPEOF: \
    case Token::TILDE: \
    case Token::AND: \
    case Token::ANDAND

using namespace thorin;

namespace impala {

class Parser;

template<class T>
class Loc {
public:
    inline Loc(Parser& parser, T* node);
    inline ~Loc();

    operator T*() const { return node_; }
    T* operator -> () const { return node_; }
    T* get() const { return node_; }

private:
    Parser& parser_;
    T* node_;
};

class Parser {
public:
    Parser(std::istream& stream, const std::string& filename)
        : lexer(stream, filename)
        , cur_var_handle(2) // reserve 1 for conditionals, 0 for mem
        , no_bars_(false)
        , result_(true)
    {
        lookahead[0] = lexer.lex();
        lookahead[1] = lexer.lex();
        lookahead[2] = lexer.lex();
        prev_loc_ = Location(filename, 1, 1, 1, 1);
    }

    const Token& la(size_t i = 0) const { assert(i < 3); return lookahead[i]; }
    Location prev_loc() const { return prev_loc_; }

#ifdef NDEBUG
    Token eat(TokenKind /*what*/) { return lex(); }
#else
    Token eat(TokenKind what) { assert(what == la() && "internal parser error"); return lex(); }
#endif

    bool accept(TokenKind tok);
    bool expect(TokenKind tok, const std::string& context);
    void error(const std::string& what, const std::string& context) { error(what, context, la()); }
    void error(const std::string& what, const std::string& context, const Token& tok);
    bool result() const { return result_; }
    template<class T>
    Loc<T> loc(T* node) { return Loc<T>(*this, node); }

    void parse_comma_list(TokenKind delimiter, const char* context, std::function<void()> f) {
        if (la() != delimiter) {
            do { f(); }
            while (accept(Token::COMMA) && la() != delimiter);
        }
        expect(delimiter, context);
    }

    // misc
    const Identifier* try_id(const std::string& what);
    Visibility parse_visibility();
    u64 parse_integer(const char* what);

    // paths
    const Path* parse_path();
    const PathElem* parse_path_elem();

    // parameters
    void parse_type_params(AutoVector<const TypeParam*>&);
    const TypeParam* parse_type_param();
    const Param* parse_param(int i, bool lambda);
    void parse_param_list(AutoVector<const Param*>& params, TokenKind delimiter, bool lambda);
    void parse_return_param(AutoVector<const Param*>&);

    // types
    const ASTType*      parse_type();
    const ArrayASTType* parse_array_type();
    const Typeof*       parse_typeof();
    const ASTType*      parse_return_type();
    const FnASTType*    parse_fn_type();
    const PrimASTType*  parse_prim_type();
    const PtrASTType*   parse_ptr_type();
    const TupleASTType* parse_tuple_type();
    const SimdASTType*  parse_simd_type();
    const ASTTypeApp*   parse_type_app();

    enum class BodyMode { None, Optional, Mandatory };

    // items
    Item*       parse_item();
    StaticItem* parse_static_item();
    EnumDecl*   parse_enum_decl();
    FnDecl*     parse_fn_decl(BodyMode);
    ImplItem*   parse_impl();
    ModDecl*    parse_mod_decl();
    Item*       parse_extern_block_or_fn_decl();
    StructDecl* parse_struct_decl();
    TraitDecl*  parse_trait_decl();
    Typedef*    parse_typedef();

    // item helpers
    const ModContents* parse_mod_contents();
    void parse_mod_contents(ModContents*);
    const FieldDecl* parse_field_decl(const int i);

    // expressions
    bool is_infix();
    const Expr*             parse_expr(Prec prec);
    const Expr*             parse_expr() { return parse_expr(BOTTOM, false); }
    const Expr*             parse_expr(Prec prec, bool no_bars) { THORIN_PUSH(no_bars_, no_bars); return parse_expr(prec); }
    const Expr*             parse_prefix_expr();
    const Expr*             parse_infix_expr(const Expr* lhs);
    const Expr*             parse_postfix_expr(const Expr* lhs);
    const Expr*             parse_primary_expr();
    const SizeofExpr*       parse_sizeof_expr();
    const LiteralExpr*      parse_literal_expr();
    const CharExpr*         parse_char_expr();
    const StrExpr*          parse_str_expr();
    const FnExpr*           parse_fn_expr();
    const IfExpr*           parse_if_expr();
    const ForExpr*          parse_for_expr();
    const WhileExpr*        parse_while_expr();
    const BlockExprBase*    parse_block_expr();
    const BlockExprBase*    try_block_expr(const std::string& context);

    // statements
    const Stmt*     parse_stmt_not_expr();
    const ItemStmt* parse_item_stmt();
    const LetStmt*  parse_let_stmt();

private:
    Token lex();        ///< Consume next Token in input stream, fill look-ahead buffer, return consumed Token.

    const LocalDecl* create_continuation_decl(const char* name, bool set_type) {
        auto decl = loc(new LocalDecl(cur_var_handle++));
        decl->is_mut_ = false;
        decl->identifier_ = new Identifier(name, prev_loc_);
        decl->set_loc(prev_loc_);
        decl->ast_type_ = set_type ? new FnASTType(prev_loc()) : nullptr;
        return decl;
    }

    Lexer lexer;        ///< invoked in order to get next token
    Token lookahead[3]; ///< SLL(3) look ahead
    size_t cur_var_handle;
    bool no_bars_;
    Location prev_loc_;
    bool result_;
};

//------------------------------------------------------------------------------

template<class T>
Loc<T>::Loc(Parser& parser, T* node)
    : parser_(parser)
    , node_(node)
{
    node_->set_pos1(parser_.la().pos1());
}
template<class T>
Loc<T>::~Loc() { node_->set_pos2(parser_.prev_loc().pos2()); }

//------------------------------------------------------------------------------

bool parse(ModContents* mod_contents, std::istream& i, const std::string& filename) {
    Parser parser(i, filename);
    parser.parse_mod_contents(mod_contents);
    if (parser.la() != Token::END_OF_FILE)
        parser.error("module item", "module contents");
    return parser.result();
}

//------------------------------------------------------------------------------

/*
 * helpers
 */

Token Parser::lex() {
    Token result = lookahead[0]; // remember result
    lookahead[0] = lookahead[1]; // copy over LA2 to LA1
    lookahead[1] = lookahead[2]; // copy over LA3 to LA2
    lookahead[2] = lexer.lex();  // fill new LA3
    prev_loc_ = result.loc();    // remember previous location
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
    result_ = false;
    std::ostream& os = tok.error() << "expected " << what << ", got '" << tok << "'";
    if (!context.empty())
        os << " while parsing " << context;
    os << "\n";
}

const Identifier* Parser::try_id(const std::string& what) {
    Token name;
    if (la() == Token::ID)
        name = lex();
    else {
        error("identifier", what);
        name = Token(la().loc(), "<error>");
    }

    return new Identifier(name);
}

Visibility Parser::parse_visibility() {
    Visibility visibility;
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

/*
 * paths
 */

const PathElem* Parser::parse_path_elem() {
    auto path_elem = loc(new PathElem());
    path_elem->identifier_ = try_id("path");
    return path_elem;
}

const Path* Parser::parse_path() {
    auto path = loc(new Path());
    path->is_global_ = accept(Token::DOUBLE_COLON);
    do {
        path->path_elems_.push_back(parse_path_elem());
    } while (accept(Token::DOUBLE_COLON));
    return path;
}

/*
 * parameters
 */

void Parser::parse_type_params(AutoVector<const TypeParam*>& type_params) {
    if (accept(Token::L_BRACKET))
        parse_comma_list(Token::R_BRACKET, "type parameter list", [&] { type_params.push_back(parse_type_param()); });
}

const TypeParam* Parser::parse_type_param() {
    auto type_param = loc(new TypeParam());
    type_param->identifier_ = try_id("type parameter");

    if (accept(Token::COLON)) {
        do
            type_param->bounds_.push_back(parse_type());
        while (accept(Token::ADD));
    }

    return type_param;
}

void Parser::parse_param_list(AutoVector<const Param*>& params, TokenKind delimiter, bool lambda) {
    int i = 0;
    parse_comma_list(delimiter, "parameter list", [&] { params.push_back(parse_param(i++, lambda)); });
}

const Param* Parser::parse_param(int i, bool lambda) {
    auto param = loc(new Param(cur_var_handle++));
    param->is_mut_ = accept(Token::MUT);
    const Identifier* ident = nullptr;
    const ASTType* type = nullptr;
    Token tok = la();

    if (tok == Token::ID)
        ident = new Identifier(lex());
    else {
        switch (tok) {
            case TYPE:
                type = parse_type(); break;
            default:
                ident = new Identifier("<error>", tok.loc());
                error("identifier", "parameter");
        }
    }

    if (accept(Token::COLON)) {
        if (type)
            error("identifier", "parameter", tok);
        param->identifier_ = ident;
        param->ast_type_ = parse_type();
    } else if (lambda) {
        if (type)
            error("identifier", "parameter", tok);
        param->identifier_ = ident;
    } else {
        if (type == nullptr) {
            // we assume that the identifier refers to a type
            auto type_app = new ASTTypeApp();
            type_app->set_loc(tok.loc());
            type_app->identifier_ = ident;
            type = type_app;
        }
        param->ast_type_ = type;
    }


    if (param->identifier_ == nullptr) {
        std::ostringstream oss;
        oss << '<' << i << ">";
        param->identifier_ = new Identifier(oss.str().c_str(), prev_loc());
    }

    return param;
}

void Parser::parse_return_param(AutoVector<const Param*>& params) {
    if (auto fn_type = parse_return_type()) {
        const Location& loc = fn_type->loc();
        params.push_back(Param::create(cur_var_handle++, new Identifier("return", loc), loc, fn_type));
    }
}

/*
 * items
 */

Item* Parser::parse_item() {
    Position pos1 = la().pos1();
    auto visibility = parse_visibility();

    Item* item = nullptr;
    switch (la()) {
        case Token::ENUM:    item = parse_enum_decl();                  break;
        case Token::EXTERN:  item = parse_extern_block_or_fn_decl();    break;
        case Token::FN:      item = parse_fn_decl(BodyMode::Mandatory); break;
        case Token::IMPL:    item = parse_impl();                       break;
        case Token::MOD:     item = parse_mod_decl();                   break;
        case Token::STATIC:  item = parse_static_item();                break;
        case Token::STRUCT:  item = parse_struct_decl();                break;
        case Token::TRAIT:   item = parse_trait_decl();                 break;
        case Token::TYPEDEF: item = parse_typedef();                    break;
        default: THORIN_UNREACHABLE;
    }

    item->set_pos1(pos1);
    item->visibility_ = visibility;
    return item;
}

EnumDecl* Parser::parse_enum_decl() {
    assert(false && "TODO");
    return 0;
}

Item* Parser::parse_extern_block_or_fn_decl() {
    Position pos1 = eat(Token::EXTERN).pos1();
    Item* item;
    if (la() == Token::FN) {
        auto fn_decl = parse_fn_decl(BodyMode::Mandatory);
        fn_decl->is_extern_ = true;
        item = fn_decl;
    } else {
        auto extern_block = new ExternBlock();
        if (la() == Token::LIT_str)
            extern_block->abi_ = lex().symbol();
        expect(Token::L_BRACE, "opening brace of external block");
        while (la() == Token::FN)
            extern_block->fns_.push_back(parse_fn_decl(BodyMode::None));
        expect(Token::R_BRACE, "closing brace of external block");
        extern_block->set_pos2(prev_loc().pos2());
        item = extern_block;
    }

    item->set_pos1(pos1);
    return item;
}

FnDecl* Parser::parse_fn_decl(BodyMode body_mode) {
    THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_decl = loc(new FnDecl());
    eat(Token::FN);
    if (la() == Token::LIT_str)
        fn_decl->export_name_ = new Identifier(lex());
    fn_decl->identifier_ = try_id("function name");
    parse_type_params(fn_decl->type_params_);
    expect(Token::L_PAREN, "function head");
    parse_param_list(fn_decl->params_, Token::R_PAREN, false);
    parse_return_param(fn_decl->params_);

    switch (body_mode) {
        case BodyMode::None:      expect(Token::SEMICOLON, "function declaration"); break;
        case BodyMode::Mandatory: fn_decl->body_ = try_block_expr("body of function"); break;
        case BodyMode::Optional:
            if (!accept(Token::SEMICOLON))
                fn_decl->body_ = try_block_expr("body of function"); break;
    }

    return fn_decl;
}

ImplItem* Parser::parse_impl() {
    auto impl = loc(new ImplItem());
    eat(Token::IMPL);
    parse_type_params(impl->type_params_);
    auto type = parse_type();
    if (accept(Token::FOR)) {
        impl->trait_ = type;
        impl->ast_type_ = parse_type();
    } else
        impl->ast_type_ = type;
    expect(Token::L_BRACE, "impl");
    while (la() == Token::FN)
        impl->methods_.push_back(parse_fn_decl(BodyMode::Mandatory));
    expect(Token::R_BRACE, "closing brace of impl");

    return impl;
}

ModDecl* Parser::parse_mod_decl() {
    auto mod_decl = loc(new ModDecl());
    eat(Token::MOD);
    mod_decl->identifier_ = try_id("module declaration");
    parse_type_params(mod_decl->type_params_);
    if (accept(Token::L_BRACE)) {
         mod_decl->mod_contents_ = parse_mod_contents();
        expect(Token::R_BRACE, "module");
    } else
        expect(Token::SEMICOLON, "module declaration");

    return mod_decl;
}

StaticItem* Parser::parse_static_item() {
    auto static_item = loc(new StaticItem());
    eat(Token::STATIC);
    static_item->is_mut_ = accept(Token::MUT);
    static_item->identifier_ = try_id("static item");
    if (accept(Token::COLON))
        static_item->ast_type_ = parse_type();
    if (accept(Token::ASGN))
        static_item->init_ = parse_expr();
    expect(Token::SEMICOLON, "static item");
    return static_item;
}

StructDecl* Parser::parse_struct_decl() {
    auto struct_decl = loc(new StructDecl());
    eat(Token::STRUCT);
    struct_decl->identifier_ = try_id("struct declaration");
    parse_type_params(struct_decl->type_params_);
    expect(Token::L_BRACE, "struct declaration");
    int i = 0;
    parse_comma_list(Token::R_BRACE, "closing brace of struct declaration", [&] {
        struct_decl->field_decls_.push_back(parse_field_decl(i++));
    });
    return struct_decl;
}

TraitDecl* Parser::parse_trait_decl() {
    auto trait_decl = loc(new TraitDecl);
    eat(Token::TRAIT);
    trait_decl->identifier_ = try_id("trait declaration");
    parse_type_params(trait_decl->type_params_);

    if (accept(Token::COLON)) {
        parse_comma_list(Token::L_BRACE, "trait declaration", [&] {
            trait_decl->super_traits_.push_back(parse_type_app());
        });
    } else
        expect(Token::L_BRACE, "trait declaration");

    while (la() == Token::FN)
        trait_decl->methods_.push_back(parse_fn_decl(BodyMode::Optional));

    expect(Token::R_BRACE, "closing brace of trait declaration");
    const_cast<SelfParam&>(trait_decl->self_param_).set_loc(trait_decl->loc().pos1());
    return trait_decl;
}

Typedef* Parser::parse_typedef() {
    auto type_def = loc(new Typedef());
    eat(Token::TYPEDEF);
    type_def->identifier_ = try_id("type definition");
    parse_type_params(type_def->type_params_);
    expect(Token::ASGN, "type definition");
    type_def->ast_type_ = parse_type();
    expect(Token::SEMICOLON, "type definition");
    return type_def;
}

/*
 * item helpers
 */

const ModContents* Parser::parse_mod_contents() {
    auto mod_contents = loc(new ModContents());
    parse_mod_contents(mod_contents);
    return mod_contents;
}

void Parser::parse_mod_contents(ModContents* mod_contents) {
    while (true) {
        switch (la()) {
            case VISIBILITY:
            case ITEM:
                mod_contents->items_.push_back(parse_item());
                continue;
            case Token::SEMICOLON:
                lex();
                continue;
            default:
                return;
        }
    }
}

const FieldDecl* Parser::parse_field_decl(const int i) {
    auto field_decl = loc(new FieldDecl);
    field_decl->index_ = i;
    field_decl->visibility_ = parse_visibility();
    field_decl->identifier_ = try_id("struct field");
    expect(Token::COLON, "struct field");
    field_decl->ast_type_ = parse_type();
    return field_decl;
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
        case Token::ID:         return parse_type_app();
        case Token::L_BRACKET:  return parse_array_type();
        case Token::TYPEOF:     return parse_typeof();
        case Token::TILDE:
        case Token::AND:
        case Token::ANDAND:     return parse_ptr_type();
        case Token::SIMD:       return parse_simd_type();
        default:  {
            error("type", "");
            auto error_type = new ErrorASTType(prev_loc());
            lex();
            return error_type;
        }
    }
}

const ArrayASTType* Parser::parse_array_type() {
    auto pos1 = la().pos1();
    eat(Token::L_BRACKET);
    const ASTType* elem_type = parse_type();
    if (accept(Token::MUL)) {
        auto definite_array = new DefiniteArrayASTType();
        definite_array->elem_type_ = elem_type;
        definite_array->dim_ = parse_integer("definite array type");
        expect(Token::R_BRACKET, "definite array type");
        definite_array->set_loc(pos1, prev_loc().pos2());
        return definite_array;
    }

    auto indefinite_array = new IndefiniteArrayASTType();
    indefinite_array->elem_type_ = elem_type;
    expect(Token::R_BRACKET, "indefinite array type");
    indefinite_array->set_loc(pos1, prev_loc().pos2());
    return indefinite_array;
}

const FnASTType* Parser::parse_fn_type() {
    auto fn_type = loc(new FnASTType());
    eat(Token::FN);
    parse_type_params(fn_type->type_params_);
    expect(Token::L_PAREN, "function type");
    parse_comma_list(Token::R_PAREN, "closing parenthesis of function type", [&] {
        fn_type->args_.push_back(parse_type());
    });

    if (auto ret_type = parse_return_type())
        fn_type->args_.push_back(ret_type);

    return fn_type;
}

const ASTType* Parser::parse_return_type() {
    if (accept(Token::ARROW)) {
        if (accept(Token::NOT))
            return nullptr;

        auto ret_type = loc(new FnASTType());
        if (accept(Token::L_PAREN)) {                   // in-place tuple
            parse_comma_list(Token::R_PAREN, "closing parenthesis of return type list", [&] {
                ret_type->args_.push_back(parse_type());
            });
        } else {
            auto type = parse_type();
            assert(!type->isa<TupleASTType>());
            ret_type->args_.push_back(type);
        }
        return ret_type;
    }
    return nullptr;
}

const PrimASTType* Parser::parse_prim_type() {
    auto prim_type = loc(new PrimASTType());
    prim_type->kind_ = (PrimASTType::Kind) lex().kind();
    return prim_type;
}

const PtrASTType* Parser::parse_ptr_type() {
    if (la() == Token::ANDAND) {
        auto pos1 = la().pos1();
        auto inner = new PtrASTType();
        inner->kind_ = '&';
        lex();
        inner->referenced_type_ = parse_type();
        auto outer = new PtrASTType();
        outer->kind_ = '&';
        outer->referenced_type_ = inner;
        inner->set_loc(pos1, prev_loc().pos2());
        outer->loc_ = inner->loc();
        return outer;
    }
    auto ptr_type = loc(new PtrASTType());
    ptr_type->kind_ = lex().symbol().str()[0];
    ptr_type->referenced_type_ = parse_type();
    return ptr_type;
}

const TupleASTType* Parser::parse_tuple_type() {
    auto tuple_type = loc(new TupleASTType());
    eat(Token::L_PAREN);
    parse_comma_list(Token::R_PAREN, "closing parenthesis of tuple type", [&] {
        tuple_type->args_.push_back(parse_type());
    });
    return tuple_type;
}

const ASTTypeApp* Parser::parse_type_app() {
    auto type_app = loc(new ASTTypeApp());
    type_app->identifier_ = new Identifier(lex());
    if (accept(Token::L_BRACKET)) {
        parse_comma_list(Token::R_BRACKET, "type arguments for type application", [&] {
            type_app->args_.push_back(parse_type());
        });
    }
    return type_app;
}

const Typeof* Parser::parse_typeof() {
    auto typeof = loc(new Typeof());
    eat(Token::TYPEOF);
    expect(Token::L_PAREN, "typeof");
    typeof->expr_ = parse_expr();
    expect(Token::R_PAREN, "typeof");
    return typeof;
}

const SimdASTType* Parser::parse_simd_type() {
    auto simd = loc(new SimdASTType());
    eat(Token::SIMD);
    expect(Token::L_BRACKET, "simd type");
    simd->scalar_type_ = parse_type();
    expect(Token::MUL, "simd type");
    simd->size_ = parse_integer("simd vector size");
    expect(Token::R_BRACKET, "simd type");
    return simd;
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

            lhs = parse_infix_expr(lhs);
        } else if ( la().is_postfix() ) {
            if (prec > PrecTable::postfix_l[la()])
                break;

            lhs = parse_postfix_expr(lhs);
        } else
            break;
    }

    return lhs;
}

const Expr* Parser::parse_prefix_expr() {
    if (la() == Token::OR || la() == Token::OROR)
        return parse_fn_expr();

    auto expr = loc(new PrefixExpr());
    auto kind = lex().kind();
    expr->kind_ = (PrefixExpr::Kind) kind;
    expr->rhs_ = parse_expr(PrecTable::prefix_r[kind]);

    return expr;
}

const Expr* Parser::parse_infix_expr(const Expr* lhs) {
    auto expr = new InfixExpr();
    auto kind = lex().kind();
    expr->kind_ = (InfixExpr::Kind) kind;
    expr->lhs_ = lhs;
    expr->rhs_ = parse_expr(PrecTable::infix_r[kind]);
    expr->set_loc(lhs->pos1(), expr->rhs()->pos2());
    return expr;
}

const Expr* Parser::parse_postfix_expr(const Expr* lhs) {
    switch (la()) {
        case Token::L_BRACKET:
        case Token::L_PAREN: {
            auto map = new MapExpr();
            map->lhs_ = lhs;
            if (accept(Token::L_BRACKET))
                parse_comma_list(Token::R_BRACKET, "type arguments of a map expression", [&] { map->type_args_.push_back(parse_type()); });
            if (accept(Token::L_PAREN))
                parse_comma_list(Token::R_PAREN, "arguments of a map expression", [&] { map->args_.push_back(parse_expr()); });
            map->set_loc(lhs->pos1(), prev_loc().pos2());
            return map;
        }
        case Token::DEC:
        case Token::INC: {
            auto expr = new PostfixExpr();
            expr->lhs_ = lhs;
            expr->kind_ = (PostfixExpr::Kind) lex().kind();
            expr->set_loc(lhs->pos1(), prev_loc().pos2());
            return expr;
        }
        case Token::DOT: {
            lex();
            auto field = new FieldExpr();
            field->lhs_ = lhs;
            field->identifier_ = try_id("field expression");
            field->set_loc(lhs->pos1(), prev_loc().pos2());
            return field;
        }
        case Token::AS: {
            lex();
            auto expr = new CastExpr();
            expr->lhs_ = lhs;
            expr->ast_type_ = parse_type();
            expr->set_loc(lhs->pos1(), prev_loc().pos2());
            return expr;
        }
        default: THORIN_UNREACHABLE;
    }
}

const Expr* Parser::parse_primary_expr() {
    switch (la()) {
        case Token::L_PAREN: {
            auto pos1 = lex().pos1();
            auto expr = parse_expr();
            if (accept(Token::COMMA)) {
                auto tuple = new TupleExpr();
                tuple->set_pos1(pos1);
                tuple->args_.push_back(expr);
                parse_comma_list(Token::R_PAREN, "elements of a tuple expression", [&] { tuple->args_.push_back(parse_expr()); });
                tuple->set_pos2(prev_loc().pos2());
                return tuple;
            } else {
                expect(Token::R_PAREN, "primary expression");
                return expr;
            }
        }
        case Token::L_BRACKET: {
            auto pos1 = lex().pos1();
            auto expr = parse_expr();
            if (accept(Token::COLON)) {
                auto indefinite_array_expr = new IndefiniteArrayExpr();
                indefinite_array_expr->dim_ = expr;
                indefinite_array_expr->elem_type_ = parse_type();
                expect(Token::R_BRACKET, "indefinite array expression");
                indefinite_array_expr->set_loc(pos1, prev_loc().pos2());
                return indefinite_array_expr;
            }
            if (accept(Token::COMMA) && accept(Token::DOTDOT)) {
                auto repeated_array_expr = loc(new RepeatedDefiniteArrayExpr());
                repeated_array_expr->set_pos1(pos1);
                repeated_array_expr->value_ = expr;
                repeated_array_expr->count_ = parse_integer("repeated array expression");
                expect(Token::R_BRACKET, "repeated array expression");
                return repeated_array_expr;
            }
            auto array = new DefiniteArrayExpr();
            array->set_pos1(pos1);
            array->args_.push_back(expr);
            parse_comma_list(Token::R_BRACKET, "elements of an array expression", [&] { array->args_.push_back(parse_expr()); });
            array->set_pos2(prev_loc().pos2());
            return array;
        }
        case Token::SIMD: {
            auto simd = loc(new SimdExpr());
            eat(Token::SIMD);
            expect(Token::L_BRACKET, "simd expression");
            parse_comma_list(Token::R_BRACKET, "elements of a simd expression", [&] { simd->args_.push_back(parse_expr()); });
            return simd;
        }
        case Token::SIZEOF:     return parse_sizeof_expr();
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
            ASTTypes type_args;
            if (accept(Token::L_BRACKET)) {     // struct or map expression
                parse_comma_list(Token::R_BRACKET, "type arguments", [&] { type_args.push_back(parse_type()); });

                if (accept(Token::L_PAREN)) {   // map expression
                    auto map = new MapExpr();
                    map->lhs_ = new PathExpr(path);
                    swap(map->type_args_, type_args);
                    parse_comma_list(Token::R_PAREN, "arguments of a map expression", [&] { map->args_.push_back(parse_expr()); });
                    map->set_loc(path->pos1(), prev_loc().pos2());
                    return map;
                } else if (accept(Token::L_BRACE)) {
                    auto struct_expr = new StructExpr();
                    struct_expr->path_ = path;
                    swap(struct_expr->type_args_, type_args);
                    parse_comma_list(Token::R_BRACE, "elements of struct expression", [&] {
                        auto symbol = try_id("identifier in struct expression");
                        expect(Token::COLON, "struct expression");
                        struct_expr->elems_.emplace_back(symbol, parse_expr());
                    });
                    struct_expr->set_loc(path->pos1(), prev_loc().pos2());
                    return struct_expr;
                }
            }
            if (la(0) == Token::L_BRACE && (la(1) == Token::ID && la(2) == Token::COLON)) {
                eat(Token::L_BRACE);
                auto struct_expr = new StructExpr();
                struct_expr->path_ = path;
                parse_comma_list(Token::R_BRACE, "elements of struct expression", [&] {
                    auto symbol = try_id("identifier in struct expression");
                    expect(Token::COLON, "struct expression");
                    struct_expr->elems_.emplace_back(symbol, parse_expr());
                });
                struct_expr->set_loc(path->pos1(), prev_loc().pos2());
                return struct_expr;
            }
            return new PathExpr(path);
        }
        case Token::IF:         return parse_if_expr();
        case Token::FOR:        return parse_for_expr();
        case Token::WHILE:      return parse_while_expr();
        case Token::L_BRACE:
        case Token::RUN_BLOCK:  return parse_block_expr();
        default:                error("expression", ""); return new EmptyExpr(lex().loc());
    }
}

const SizeofExpr* Parser::parse_sizeof_expr() {
    auto sizeof_expr = loc(new SizeofExpr());
    eat(Token::SIZEOF);
    expect(Token::L_PAREN, "sizeof expression");
    sizeof_expr->ast_type_ = parse_type();
    expect(Token::R_PAREN, "sizeof expression");
    return sizeof_expr;
}

const LiteralExpr* Parser::parse_literal_expr() {
    LiteralExpr::Kind kind;
    Box box;

    switch (la()) {
        case Token::TRUE:       return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, Box(true));
        case Token::FALSE:      return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, Box(false));
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype: { \
            kind = LiteralExpr::LIT_##itype; \
            Box box = la().box(); \
            return new LiteralExpr(lex().loc(), kind, box); \
        }
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const CharExpr* Parser::parse_char_expr() {
    auto symbol = la().symbol();
    return new CharExpr(lex().loc(), symbol);
}

const StrExpr* Parser::parse_str_expr() {
    auto str_expr = loc(new StrExpr());
    do {
        str_expr->symbols_.emplace_back(la().symbol());
        lex();
    } while (la() == Token::LIT_str);
    return str_expr;
}

const FnExpr* Parser::parse_fn_expr() {
    THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_expr = loc(new FnExpr());

    if (accept(Token::OR))
        parse_param_list(fn_expr->params_, Token::OR, true);
    else
        expect(Token::OROR, "parameter list of function expression");

    parse_return_param(fn_expr->params_);
    fn_expr->ret_var_handle_ = cur_var_handle++; // reserve one hanlde - we might later on add another return param
    fn_expr->body_ = parse_expr();
    return fn_expr;
}

const IfExpr* Parser::parse_if_expr() {
    auto if_expr = loc(new IfExpr());
    eat(Token::IF);
    if_expr->cond_ = parse_expr();
    if_expr->then_expr_ = try_block_expr("consequence of an if expression");
    if (accept(Token::ELSE)) {
        switch (la()) {
            case Token::IF:         if_expr->else_expr_ = parse_if_expr(); break;
            case Token::L_BRACE:
            case Token::RUN_BLOCK:  if_expr->else_expr_ = parse_block_expr(); break;
            default:
                error("block or if expression", "alternative of an if expression");
        }
    }

    if (if_expr->else_expr_ == nullptr)
        if_expr->else_expr_ = new BlockExpr(prev_loc());
    return if_expr;
}

const ForExpr* Parser::parse_for_expr() {
    auto for_expr = loc(new ForExpr());
    eat(Token::FOR);
    THORIN_PUSH(cur_var_handle, cur_var_handle);
    auto fn_expr = loc(new FnExpr());
    for_expr->fn_expr_ = fn_expr.get();
    if (la(0) == Token::IN || la(0) == Token::MUT || la(1) == Token::COLON || la(1) == Token::COMMA || la(1) == Token::IN)
        parse_param_list(fn_expr->params_, Token::IN, true);
    fn_expr->params_.push_back(Param::create(cur_var_handle++, new Identifier("continue", prev_loc()), prev_loc(), nullptr));
    for_expr->break_decl_ = create_continuation_decl("break", /*set type during TypeSema*/ false);
    for_expr->expr_ = parse_expr();
    fn_expr->body_ = try_block_expr("body of function");
    return for_expr;
}

const WhileExpr* Parser::parse_while_expr() {
    auto while_expr = loc(new WhileExpr());
    eat(Token::WHILE);
    while_expr->cond_ = parse_expr();
    while_expr->break_decl_ = create_continuation_decl("break", true);
    while_expr->continue_decl_ = create_continuation_decl("continue", true);
    while_expr->body_ = try_block_expr("body of while loop");
    return while_expr;
}

const BlockExprBase* Parser::parse_block_expr() {
    assert(la() == Token::L_BRACE || la() == Token::RUN_BLOCK);
    auto block = loc(lex() == Token::L_BRACE ? (BlockExprBase*) new BlockExpr() : (BlockExprBase*) new RunBlockExpr());
    auto& stmts = block->stmts_;
    while (true) {
        switch (la()) {
            case Token::SEMICOLON:  lex(); continue; // ignore semicolon
            case STMT_NOT_EXPR:     stmts.push_back(parse_stmt_not_expr()); continue;
            case EXPR: {
                bool stmt_like = la().is_stmt_like();
                auto expr = parse_expr();
                if (accept(Token::SEMICOLON) || (stmt_like && la() != Token::R_BRACE)) {
                    auto expr_stmt = new ExprStmt();
                    expr_stmt->set_loc(expr->pos1(), prev_loc().pos2());
                    expr_stmt->expr_ = expr;
                    stmts.push_back(expr_stmt);
                    continue;
                } else
                    block->expr_ = expr;
                // FALLTHROUGH
            }
            default:
                expect(Token::R_BRACE, "block expression");
                if (block->expr_ == nullptr)
                    block->expr_ = new EmptyExpr(prev_loc());
                return block;
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
 * statements
 */

const Stmt* Parser::parse_stmt_not_expr() {
    switch (la()) {
        case ITEM:       return parse_item_stmt();
        case Token::LET: return parse_let_stmt();
        default:         THORIN_UNREACHABLE;
    }
}

const LetStmt* Parser::parse_let_stmt() {
    auto let_stmt = loc(new LetStmt());
    eat(Token::LET);
    auto local = loc(new LocalDecl(cur_var_handle++));
    local->is_mut_ = accept(Token::MUT);
    local->identifier_ = try_id("local variable in let binding");
    if (accept(Token::COLON))
        local->ast_type_ = parse_type();
    if (accept(Token::ASGN))
        let_stmt->init_ = parse_expr();
    expect(Token::SEMICOLON, "the end of an let statement");

    let_stmt->local_ = local.get();
    return let_stmt;
}

const ItemStmt* Parser::parse_item_stmt() {
    auto item_stmt = loc(new ItemStmt());
    item_stmt->item_ = parse_item();
    return item_stmt;
}

}
