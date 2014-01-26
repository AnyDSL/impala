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
#include "impala/type.h"

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
    case Token::TRAIT

#define MOD_CONTENTS \
         VISIBILITY: \
    case ITEM

#define EXPR \
         Token::LIT_int8: \
    case Token::LIT_int16: \
    case Token::LIT_int32: \
    case Token::LIT_int64: \
    case Token::LIT_float: \
    case Token::LIT_double: \
    case Token::TRUE: \
    case Token::FALSE: \
    case Token::ADD: \
    case Token::SUB: \
    case Token::MUL: \
    case Token::AND: \
    case Token::NOT: \
    case Token::L_N: \
    case Token::INC: \
    case Token::DEC: \
    case Token:: OR: \
    case Token::L_O: \
    case Token::ID: \
    case Token::RUN: \
    case Token::HALT: \
    case Token::IF: \
    case Token::FOR: \
    case Token::L_PAREN: \
    case Token::L_BRACE: \
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
    case Token::TYPE_int8: \
    case Token::TYPE_int16: \
    case Token::TYPE_int32: \
    case Token::TYPE_int64: \
    case Token::TYPE_float: \
    case Token::TYPE_double: \
    case Token::TYPE_bool: \
    case Token::TYPE_int: \
    case Token::NOT: \
    case Token::AND: \
    case Token::L_N

using namespace thorin;

namespace impala {

class Parser;

template<class T>
class Loc {
public:
    inline Loc(Parser& parser, T* node);
    inline ~Loc();

    operator T*() { return node_; }
    T* operator -> () { return node_; }

private:
    Parser& parser_;
    T* node_;
};

class Parser {
public:
    Parser(TypeTable& typetable, std::istream& stream, const std::string& filename)
        : typetable(typetable)
        , lexer(stream, filename)
        , cur_var_handle(2) // reserve 1 for conditionals, 0 for mem
        , no_bars_(false)
        , result_(true)
    {
        lookahead[0] = lexer.lex();
        lookahead[1] = lexer.lex();
        prev_loc_ = Location(filename, 1, 1, 1, 1);
    }

    const Token& la(size_t i) const { return lookahead[i]; }
    const Token& la () const { return lookahead[0]; }
    const Token& la2() const { return lookahead[1]; }
    thorin::Location prev_loc() const { return prev_loc_; }

#ifdef NDEBUG
    Token eat(TokenKind /*what*/) { return lex(); }
#else
    Token eat(TokenKind what) { assert(what == la() && "internal parser error"); return lex(); }
#endif

    bool accept(TokenKind tok);
    bool expect(TokenKind tok, const std::string& context);
    void error(const std::string& what, const std::string& context);
    bool result() const { return result_; }
    template<class T>
    Loc<T> loc(T* node) { return Loc<T>(*this, node); }

    void parse_comma_list(TokenKind delimiter, const char* context, std::function<void()> f) {
        if (la() != delimiter) {
            do { f(); }
            while ( accept(Token::COMMA) );
        }
        expect(delimiter, context);
    }

    // misc
    Symbol try_id(const std::string& what);
    bool is_expr() const {
        switch (la()) {
            case EXPR: return true;
            default:   return false;
        }
    }
    Visibility parse_visibility();

    // parameters
    void parse_type_params(TypeParams&);
    const TypeParam* parse_type_param();
    const Param* parse_param(bool lambda);
    bool parse_return_param(Params&);
    void parse_param_list(Params& params, TokenKind delimiter, bool lambda);

    // types
    const Type* parse_type();
    const Type* parse_array_type();
    const Type* parse_type_app();
    const Type* parse_fn_type();
    const Type* parse_tuple_type();
    void parse_types(Types& types);

    // items
    Item*       parse_item();
    ConstItem*  parse_const_item();
    EnumDecl*   parse_enum_decl();
    FnDecl*     parse_fn_decl(bool maybe_empty);
    ForeignMod* parse_foreign_mod();
    Impl*       parse_impl();
    ModDecl*    parse_mod_decl();
    Item*       parse_foreign_mod_or_fn_decl();
    StructDecl* parse_struct_decl();
    TraitDecl*  parse_trait_decl();
    Typedef*    parse_typedef();

    // item helpers
    const ModContents* parse_mod_contents();
    const FieldDecl* parse_field_decl();

    // expressions
    bool is_infix();
    const Expr*        parse_expr(Prec prec);
    const Expr*        parse_expr() { return parse_expr(BOTTOM, false); }
    const Expr*        parse_expr(Prec prec, bool no_bars) { THORIN_PUSH(no_bars_, no_bars); return parse_expr(prec); }
    const Expr*        parse_prefix_expr();
    const Expr*        parse_infix_expr(const Expr* lhs);
    const Expr*        parse_postfix_expr(const Expr* lhs);
    const Expr*        parse_primary_expr();
    const LiteralExpr* parse_literal_expr();
    const FnExpr*      parse_fn_expr();
    const IfExpr*      parse_if_expr();
    const ForExpr*     parse_for_expr();
    const BlockExpr*   parse_block_expr();
    const BlockExpr*   try_block_expr(const std::string& context);

    // statements
    const Stmt*     parse_stmt_not_expr();
    const ExprStmt* parse_expr_stmt();
    const ItemStmt* parse_item_stmt();
    const LetStmt*  parse_let_stmt();

private:
    /// Consume next Token in input stream, fill look-ahead buffer, return consumed Token.
    Token lex();

    TypeTable& typetable;
    Lexer lexer;       ///< invoked in order to get next token
    Token lookahead[2];///< LL(2) look ahead
    size_t cur_var_handle;
    bool no_bars_;
    thorin::Location prev_loc_;
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

const ModContents* parse(bool& result, TypeTable& typetable, std::istream& i, const std::string& filename) {
    Parser parser(typetable, i, filename);
    auto mod = parser.parse_mod_contents();
    result = parser.result();
    return mod;
}

//------------------------------------------------------------------------------

/*
 * helpers
 */

Token Parser::lex() {
    Token result  = lookahead[0]; // remember result
    lookahead[0] = lookahead[1];  // copy over LA2 to LA1
    lookahead[1] = lexer.lex();   // fill new LA2
    prev_loc_ = result.loc();     // remember previous location

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

void Parser::error(const std::string& what, const std::string& context) {
    result_ = false;
    std::ostream& os = la().error() << "expected " << what << ", got '" << la() << "'";
    if (!context.empty())
        os << " while parsing " << context;
    os << "\n";
}

Symbol Parser::try_id(const std::string& what) {
    Token name;
    if (la() == Token::ID)
        name = lex();
    else {
        error("identifier", what);
        name = Token(la().loc(), "<error>");
    }

    return name.symbol();
}

Visibility Parser::parse_visibility() {
    Visibility visibility;
    switch (la()) {
        case VISIBILITY: return Visibility(lex().kind());
        default:         return Visibility(Visibility::None);
    }
}

/*
 * parameters
 */

void Parser::parse_type_params(TypeParams& type_params) {
    if (accept(Token::L_BRACKET))
        parse_comma_list(Token::R_BRACKET, "type parameter list", [&] { type_params.push_back(parse_type_param()); });
}

const TypeParam* Parser::parse_type_param() {
    auto type_param = loc(new TypeParam());
    type_param->symbol_ = try_id("type parameter");

    if (accept(Token::COLON)) {
        do 
            type_param->bounds_.push_back(parse_type());
        while (accept(Token::ADD));
    }

    return type_param;
}

void Parser::parse_param_list(Params& params, TokenKind delimiter, bool lambda) {
    parse_comma_list(delimiter, "parameter list", [&] { params.push_back(parse_param(lambda)); });
}

const Param* Parser::parse_param(bool lambda) {
    auto param = loc(new Param(cur_var_handle++));
    param->is_mut_ = accept(Token::MUT);
    Symbol symbol;
    const Type* type = nullptr;

    if (la() == Token::ID)
        symbol = lex().symbol();
    else {
        switch (la()) {
            case Token::TYPE: type = parse_type(); break;
            default:    
                symbol = "<error>";
                error("identifier", "parameter");
        }
    }

    if (accept(Token::COLON)) {
        if (type)
            error("identifier", "parameter");
        param->symbol_ = symbol;
        param->type_ = parse_type();
    } else if (lambda) {
        if (type)
            error("identifier", "parameter");
        param->symbol_ = symbol;
    } else
        param->type_ = type != nullptr ? type : typetable.type_app(symbol, {});

    return param;
}

bool Parser::parse_return_param(Params& params) {
    if (accept(Token::ARROW)) {
        Position pos1 = prev_loc().pos1();
        if (accept(Token::L_N))
            return true;
        auto type = typetable.pack_return_type(parse_type());
        Position pos2 = prev_loc().pos2();
        auto param = new Param(cur_var_handle++);
        param->is_mut_ = false;
        param->symbol_ = "return";
        param->type_ = type;
        param->set_loc(pos1, pos2);
        params.push_back(param);
        return true;
    } else
        return false;
}

/*
 * items
 */

Item* Parser::parse_item() {
    Position pos1 = la().pos1();
    auto visibility = parse_visibility();

    Item* item = nullptr;
    switch (la()) {
        case Token::ENUM:    item = parse_enum_decl();              break;
        case Token::EXTERN:  item = parse_foreign_mod_or_fn_decl(); break;
        case Token::FN:      item = parse_fn_decl(false);           break;
        case Token::IMPL:    item = parse_impl();                   break;
        case Token::MOD:     item = parse_mod_decl();               break;
        case Token::STATIC:  item = parse_const_item();             break;
        case Token::STRUCT:  item = parse_struct_decl();            break;
        case Token::TRAIT:   item = parse_trait_decl();             break;
        case Token::TYPEDEF: item = parse_typedef();                break;
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

Item* Parser::parse_foreign_mod_or_fn_decl() {
    Position pos1 = eat(Token::EXTERN).pos1();
    Item* item;
    if (la() == Token::FN) {
        auto fn_decl = parse_fn_decl(false);
        fn_decl->extern_ = true;
        item = fn_decl;
    } else {
        assert(false && "TODO");
    }

    item->set_pos1(pos1);
    return item;
}

ForeignMod* Parser::parse_foreign_mod() {
    assert(false && "TODO");
    return 0;
}

FnDecl* Parser::parse_fn_decl(bool maybe_empty) {
    THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_decl = loc(new FnDecl(typetable));
    auto& fn = fn_decl->fn_;
    eat(Token::FN);
    fn_decl->symbol_ = try_id("function name");
    parse_type_params(fn_decl->type_params_);
    expect(Token::L_PAREN, "function head");
    parse_param_list(fn.params_, Token::R_PAREN, false);
    parse_return_param(fn.params_);

    if (maybe_empty && accept(Token::SEMICOLON)) {
        // do nothing
    } else
        fn.body_ = try_block_expr("body of function");

    return fn_decl;
}

Impl* Parser::parse_impl() {
    auto impl = loc(new Impl());
    eat(Token::IMPL);
    impl->symbol_ = try_id("impl");
    parse_type_params(impl->type_params_);
    impl->for_type_ = accept(Token::FOR) ? parse_type() : nullptr;
    expect(Token::L_BRACE, "impl");
    while (la() == Token::FN)
        impl->methods_.push_back(parse_fn_decl(false)); 
    expect(Token::R_BRACE, "closing brace of impl");

    return impl;
}

ModDecl* Parser::parse_mod_decl() {
    auto mod_decl = loc(new ModDecl());
    eat(Token::MOD);
    mod_decl->symbol_ = try_id("module declaration");
    parse_type_params(mod_decl->type_params_);
    if (accept(Token::L_BRACE)) {
        mod_decl->mod_contents_ = parse_mod_contents();
        expect(Token::R_BRACE, "module");
    } else
        expect(Token::SEMICOLON, "module declaration");

    return mod_decl;
}

ConstItem* Parser::parse_const_item() {
    assert(false && "TODO");
    return 0;
}

StructDecl* Parser::parse_struct_decl() {
    auto struct_decl = loc(new StructDecl());
    eat(Token::STRUCT);
    struct_decl->symbol_ = try_id("struct declaration");
    parse_type_params(struct_decl->type_params_);
    expect(Token::L_BRACE, "struct declaration");
    parse_comma_list(Token::R_BRACE, "closing brace of struct declaration", [&] { 
        struct_decl->fields_.push_back(parse_field_decl()); 
    });
    return struct_decl;
}

TraitDecl* Parser::parse_trait_decl() {
    auto trait_decl = loc(new TraitDecl);
    eat(Token::TRAIT);
    trait_decl->symbol_ = try_id("trait declaration");
    parse_type_params(trait_decl->type_params_);

    if (accept(Token::COLON)) {
        parse_comma_list(Token::L_BRACE, "trait declaration", [&] { 
            trait_decl->super_.push_back(try_id("list of super traits"));
        });
    } else
        expect(Token::L_BRACE, "trait declaration");

    while (la() == Token::FN)
        trait_decl->methods_.push_back(parse_fn_decl(true)); 

    expect(Token::R_BRACE, "closing brace of trait declaration");
    return trait_decl;
}

Typedef* Parser::parse_typedef() {
    assert(false && "TODO");
    return 0;
}

/*
 * item helpers
 */

const ModContents* Parser::parse_mod_contents() {
    auto mod_contents = loc(new ModContents());

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
                return mod_contents;
        }
    }
}

const FieldDecl* Parser::parse_field_decl() {
    auto field_decl = loc(new FieldDecl);
    field_decl->visibility_ = parse_visibility();
    field_decl->is_mut_ = accept(Token::MUT);
    field_decl->symbol_ = try_id("struct field");
    expect(Token::COLON, "struct field");
    field_decl->type_ = parse_type();
    return field_decl;
}

/*
 * types
 */

const Type* Parser::parse_type() {
    switch (la()) {
#define IMPALA_TYPE(itype, atype) \
        case Token::TYPE_##itype:   lex(); return typetable.type_##itype();
#include "impala/tokenlist.h"
        case Token::TYPE_int:       lex(); return typetable.type_int32();
        case Token::L_N:            lex(); return typetable.noret();
        case Token::FN:                    return parse_fn_type();
        case Token::L_PAREN:               return parse_tuple_type();
        case Token::ID:                    return parse_type_app();
        case Token::L_BRACKET:      lex(); return parse_array_type();
        case Token::NOT:            lex(); return typetable.   owned_ptr(parse_type());
        case Token::AND:            lex(); return typetable.borrowed_ptr(parse_type());
        default: error("type", ""); lex(); return typetable.type_error();
    }
}

const Type* Parser::parse_array_type() {
    const Type* result = parse_type();

    while (accept(Token::MUL)) {
        u64 length;
        switch (la()) {
            case Token::LIT_int8:   length = la().box().get_s8();  break;
            case Token::LIT_int16:  length = la().box().get_s16(); break;
            case Token::LIT_int32:  length = la().box().get_s32(); break;
            case Token::LIT_int64:  length = la().box().get_s64(); break;
            default:
                length = 0;
                error("integer literal", "definite array type");
        }
        lex();
        result = typetable.definite_array(result, length);
    }

    if (!result->isa<DefiniteArray>())
        result = typetable.indefinite_array(result);

    expect(Token::R_BRACKET, "array type");
    return result;
}

const Type* Parser::parse_fn_type() {
    eat(Token::FN);
    std::vector<const Type*> elems;
    expect(Token::L_PAREN, "parameter list of function type");
    parse_comma_list(Token::R_PAREN, "closing parenthesis of function type", [&] { elems.push_back(parse_type()); });

    if (accept(Token::ARROW)) {
        if (accept(Token::L_N)) {
        } else 
            elems.push_back(typetable.fntype({parse_type()}));
    }

    return typetable.fntype(elems);
}

const Type* Parser::parse_type_app() {
    auto symbol = lex().symbol();
    std::vector<const Type*> elems;
    if (accept(Token::L_BRACKET))
        parse_comma_list(Token::R_BRACKET, "type arguments for type application", [&] { elems.push_back(parse_type()); });
    return typetable.type_app(symbol, elems);
}

const Type* Parser::parse_tuple_type() {
    eat(Token::L_PAREN);
    std::vector<const Type*> elems;
    parse_comma_list(Token::R_PAREN, "closing parenthesis of tuple type", [&] { elems.push_back(parse_type()); });

    return typetable.tupletype(elems);
}

void Parser::parse_types(Types& types) {
}

/*
 * expressions
 */

bool Parser::is_infix() {
    bool infix = la().is_infix();
    if (no_bars_ && infix)
        return la() != Token::OR && la() != Token::L_O;
    return infix;
}

const Expr* Parser::parse_expr(Prec prec) {
    auto lhs = la().is_prefix() ? parse_prefix_expr() : parse_primary_expr();

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
    if (la() == Token::OR || la() == Token::L_O)
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
    switch (auto kind = lex().kind()) {
        case Token::L_PAREN: {
            auto map = new MapExpr();
            map->lhs_ = lhs;
            parse_comma_list(Token::R_PAREN, "arguments of a map expression", [&] { map->ops_.push_back(parse_expr()); });
            map->set_loc(lhs->pos1(), prev_loc().pos2());
            return map;
        }
        case Token::DEC:
        case Token::INC: {
            auto expr = new PostfixExpr();
            expr->lhs_ = lhs;
            expr->kind_ = (PostfixExpr::Kind) kind;
            expr->set_loc(lhs->pos1(), prev_loc().pos2());
            return expr;
        }
        case Token::DOT: {
            auto expr = new DotExpr();
            expr->lhs_ = lhs;
            expr->symbol_ = try_id("dot expression");
            expr->set_loc(lhs->pos1(), prev_loc().pos2());
            return expr;
        }
        default: THORIN_UNREACHABLE;
    }
}

const Expr* Parser::parse_primary_expr() {
    switch (la()) {
        case Token::L_PAREN: {
            Position pos1 = lex().pos1();
            auto expr = parse_expr();
            if (accept(Token::COMMA)) {
                auto tuple = new TupleExpr();
                tuple->set_pos1(pos1);
                tuple->ops_.push_back(expr);
                parse_comma_list(Token::R_PAREN, "elements of tuple expression", [&] { tuple->ops_.push_back(parse_expr()); });
                tuple->set_pos2(prev_loc().pos2());
                return tuple;
            } else {
                expect(Token::R_PAREN, "primary expression");
                return expr;
            }
        }
        case Token::L_BRACKET: {
            auto array = new ArrayExpr();
            array->set_pos1(lex().pos1());
            parse_comma_list(Token::R_BRACKET, "elements of array expression", [&] { array->ops_.push_back(parse_expr()); });
            array->set_pos2(prev_loc().pos2());
            return array;
        }
#define IMPALA_LIT(itype, atype) \
        case Token::LIT_##itype:
#include "impala/tokenlist.h"
        case Token::TRUE:
        case Token::FALSE:      return parse_literal_expr();
        case Token::ID:  {
            auto tok = lex();
            if (accept(Token::L_BRACE)) {
                auto struct_expr = new StructExpr();
                struct_expr->symbol_ = tok.symbol();
                parse_comma_list(Token::R_BRACE, "elements of struct expression", [&] {
                    auto symbol = try_id("identifier in struct expression");
                    expect(Token::COLON, "struct expression");
                    struct_expr->elems_.emplace_back(symbol, std::unique_ptr<const Expr>(parse_expr()));
                });
                struct_expr->set_loc(tok.pos1(), prev_loc().pos2());
                return struct_expr;
            }
            return new IdExpr(tok);
        }
        case Token::IF:         return parse_if_expr();
        case Token::FOR:        return parse_for_expr();
        case Token::L_BRACE:    return parse_block_expr();
        default:                error("expression", ""); return new EmptyExpr(lex().loc());
    }
}

const LiteralExpr* Parser::parse_literal_expr() {
    LiteralExpr::Kind kind;
    Box box;

    switch (la()) {
        case Token::TRUE:  return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, Box(true));
        case Token::FALSE: return new LiteralExpr(lex().loc(), LiteralExpr::LIT_bool, Box(false));
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

const FnExpr* Parser::parse_fn_expr() {
    THORIN_PUSH(cur_var_handle, cur_var_handle);

    auto fn_expr = loc(new FnExpr(typetable));
    auto& fn = fn_expr->fn_;

    if (accept(Token::OR))
        parse_param_list(fn.params_, Token::OR, true);
    else
        expect(Token::L_O, "parameter list of function expression");

    fn_expr->has_return_type_ = parse_return_param(fn.params_);
    fn.body_ = parse_expr();
    return fn_expr;
}

const IfExpr* Parser::parse_if_expr() {
    auto if_expr = loc(new IfExpr());
    eat(Token::IF);
    if_expr->cond_ = parse_expr();
    if_expr->then_expr_ = try_block_expr("then branch of an if expression");
    if (accept(Token::ELSE)) {
        switch (la()) {
            case Token::IF:      if_expr->else_expr_ = parse_if_expr(); break;
            case Token::L_BRACE: if_expr->else_expr_ = parse_block_expr(); break;
            default:
                error("block or if expression", "else branch of an if expression");
        }
    }
        
    if (if_expr->else_expr_ == nullptr)
        if_expr->else_expr_ = new BlockExpr(prev_loc());
    return if_expr;
}

const ForExpr* Parser::parse_for_expr() {
    auto for_expr = loc(new ForExpr());
    auto& fn = for_expr->fn_;
    eat(Token::FOR);
    parse_param_list(fn.params_, Token::IN, true);
    for_expr->expr_ = parse_expr();
    fn.body_ = try_block_expr("body of function");
    return for_expr;
}

const BlockExpr* Parser::parse_block_expr() {
    auto block = loc(new BlockExpr());
    eat(Token::L_BRACE);
    auto& stmts = block->stmts_;
    while (true) {
        switch (la()) {
            case Token::SEMICOLON:  lex(); continue; // ignore semicolon
            case STMT_NOT_EXPR:     stmts.push_back(parse_stmt_not_expr()); continue;
            case EXPR: {
                bool stmt_like = la() == Token::IF || la() == Token::FOR;
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

const BlockExpr* Parser::try_block_expr(const std::string& context) {
    if (la() == Token::L_BRACE)
        return parse_block_expr();
    else {
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

const ExprStmt* Parser::parse_expr_stmt() {
    auto s = loc(new ExprStmt());
    s->expr_ = parse_expr();
    expect(Token::SEMICOLON, "the end of an expression statement");
    return s;
}

const LetStmt* Parser::parse_let_stmt() {
    auto let_stmt = loc(new LetStmt());
    eat(Token::LET);
    auto local = loc(new LocalDecl(cur_var_handle++));
    local->is_mut_ = accept(Token::MUT);
    local->symbol_ = try_id("local variable in let binding");
    if (accept(Token::COLON))
        local->type_ = parse_type();
    if (accept(Token::ASGN))
        let_stmt->init_ = parse_expr();
    expect(Token::SEMICOLON, "the end of an let statement");

    let_stmt->local_ = local;
    return let_stmt;
}

const ItemStmt* Parser::parse_item_stmt() {
    auto item_stmt = loc(new ItemStmt());
    item_stmt->item_ = parse_item();
    return item_stmt;
}

} // namespace impala
