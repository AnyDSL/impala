#ifndef IMPALA_TOKEN_H
#define IMPALA_TOKEN_H

#include <ostream>
#include <string>

#include "thorin/enums.h"
#include "thorin/util/assert.h"

#include "impala/location.h"
#include "impala/symbol.h"

namespace impala {

class Token : public impala::HasLocation {
public:
    enum Kind {
        /*
         * !!! DO NOT CHANGE THIS ORDER !!!
         */

        // add prefix and postfix tokens manually in order to avoid duplicates in the enum
#define IMPALA_INFIX(     tok, t_str, r, l) tok,
#define IMPALA_INFIX_ASGN(tok, t_str, r, l) tok,
#define IMPALA_KEY_EXPR(  tok, t_str)       tok,
#define IMPALA_KEY_STMT(  tok, t_str)       tok,
#define IMPALA_KEY_MISC(  tok, t_str)       tok,
#define IMPALA_MISC(      tok, t_str)       tok,
#define IMPALA_LIT(       tok, t)           LIT_##tok,
#define IMPALA_TYPE(itype, atype)           TYPE_##itype,
#include "impala/tokenlist.h"

        // manually insert missing unary prefix/postfix types
        TILDE, NOT, INC, DEC, RUN, HLT, DOT,
        // these do ont appear in impala/tokenlist.h -- they are too special
        MUT, ID, END_OF_FILE,
        TYPE_app, TYPE_generic, TYPE_genericref, TYPE_error, TYPE_tuple, TYPE_definite_array, TYPE_indefinite_array,
        LIT_char, LIT_str,
        NUM_TOKENS
    };

    struct KindHash {
        size_t operator () (Kind kind) const { return thorin::hash_value((int) kind); }
    };

    Token() {}
    /// Create an operator token
    Token(const Location& loc, Kind tok);
    /// Create an identifier or a keyword (depends on \p str)
    Token(const Location& loc, const std::string& str);
    /// Create a literal
    Token(const Location& loc, Kind type, const std::string& str);

    Symbol symbol() const { return symbol_; }
    thorin::Box box() const { return box_; }
    Kind kind() const { return kind_; }
    operator Kind () const { return kind_; }

    enum Op {
        NONE    = 0,
        PREFIX  = 1,
        INFIX   = 2,
        POSTFIX = 4,
        ASGN_OP = 8
    };

    bool is_prefix()  const { return is_prefix(kind_); }
    bool is_infix()   const { return is_infix(kind_); }
    bool is_postfix() const { return is_postfix(kind_); }
    bool is_assign()  const { return is_assign(kind_); }
    bool is_op()      const { return is_op(kind_); }

    static Kind sym2lit(Symbol sym);
    static Kind sym2flit(Symbol sym);
    static bool is_prefix(Kind kind)  { return (tok2op_[kind] &  PREFIX) != 0; }
    static bool is_infix(Kind kind)   { return (tok2op_[kind] &   INFIX) != 0; }
    static bool is_postfix(Kind kind) { return (tok2op_[kind] & POSTFIX) != 0; }
    static bool is_assign(Kind kind)  { return (tok2op_[kind] & ASGN_OP) != 0; }
    static bool is_op(Kind kind)      { return is_prefix(kind) || is_infix(kind) || is_postfix(kind); }
    static bool is_rel(Kind kind);
    static Kind separate_assign(Kind kind);
    static int to_binop(Kind kind);
    static thorin::ArithOpKind to_arithop(Kind kind) { return (thorin::ArithOpKind) to_binop(kind); }
    static thorin::CmpKind     to_cmp    (Kind kind) { return (thorin::CmpKind)     to_binop(kind); }

    bool operator == (const Token& t) const { return kind_ == t; }
    bool operator != (const Token& t) const { return kind_ != t; }

private:
    static void init();
    static Symbol insert(Kind tok, const char* str);
    static void insert_key(Kind tok, const char* str);

    Symbol symbol_;
    Kind kind_;
    thorin::Box box_;

    static int tok2op_[NUM_TOKENS];
    static thorin::HashMap<Kind, const char*, KindHash> tok2str_;
    static thorin::HashMap<Kind, Symbol, KindHash> tok2sym_;
    static thorin::HashMap<Symbol, Kind> keywords_;
    static thorin::HashMap<Symbol, Kind> sym2lit_;
    static thorin::HashMap<Symbol, Kind> sym2flit_;

    friend void init();
    friend std::ostream& operator << (std::ostream& os, const Token& tok);
    friend std::ostream& operator << (std::ostream& os, const Kind&  tok);
};

typedef Token::Kind TokenKind;

//------------------------------------------------------------------------------

std::ostream& operator << (std::ostream& os, const Token& tok);
std::ostream& operator << (std::ostream& os, const TokenKind& tok);

//------------------------------------------------------------------------------

}

#endif
