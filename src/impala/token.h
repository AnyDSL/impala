#ifndef IMPALA_TOKEN_H
#define IMPALA_TOKEN_H

#include <ostream>
#include <string>

#include "thorin/enums.h"
#include "thorin/util/assert.h"
#include "thorin/util/location.h"

#include "impala/symbol.h"

namespace impala {

class Token : public thorin::HasLocation {
public:
    enum Kind {
        // !!! DO NOT CHANGE THIS ORDER !!!
        // add prefix and postfix tokens manually in order to avoid duplicates in the enum
#define IMPALA_INFIX(     tok, t_str, r, l) tok,
#define IMPALA_INFIX_ASGN(tok, t_str, r, l) tok,
#define IMPALA_KEY(       tok, t_str)       tok,
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
        Num_Tokens,
        Sentinel,
    };

    struct KindHash {
        static uint64_t hash(Kind kind) { return uint64_t(kind); }
        static bool eq(Kind k1, Kind k2) { return k1 == k2; }
        static Kind sentinel() { return Sentinel; }
    };

    Token() {}
    /// Create an operator token
    Token(const thorin::Location& loc, Kind tok);
    /// Create an identifier or a keyword (depends on \p str)
    Token(const thorin::Location& loc, const std::string& str);
    /// Create a literal
    Token(const thorin::Location& loc, Kind type, const std::string& str);

    Symbol symbol() const { return symbol_; }
    thorin::Box box() const { return box_; }
    Kind kind() const { return kind_; }
    operator Kind() const { return kind_; }

    enum Op {
        None    = 0,
        Prefix  = 1,
        Infix   = 2,
        Postfix = 4,
        Asgn_Op = 8
    };

    bool is_stmt_like() const { return kind() == L_BRACE || kind() == IF || kind() == FOR || kind() == WHILE || kind() == WITH; }
    bool is_prefix()    const { return is_prefix(kind_); }
    bool is_infix()     const { return is_infix(kind_); }
    bool is_postfix()   const { return is_postfix(kind_); }
    bool is_assign()    const { return is_assign(kind_); }
    bool is_op()        const { return is_op(kind_); }

    static Kind sym2lit(Symbol sym);
    static Kind sym2flit(Symbol sym);
    static bool is_prefix(Kind kind)  { return (tok2op_[kind] &  Prefix) != 0; }
    static bool is_infix(Kind kind)   { return (tok2op_[kind] &   Infix) != 0; }
    static bool is_postfix(Kind kind) { return (tok2op_[kind] & Postfix) != 0; }
    static bool is_assign(Kind kind)  { return (tok2op_[kind] & Asgn_Op) != 0; }
    static bool is_op(Kind kind)      { return is_prefix(kind) || is_infix(kind) || is_postfix(kind); }
    static bool is_rel(Kind kind);
    static Kind separate_assign(Kind kind);
    static int to_binop(Kind kind);
    static thorin::ArithOpKind to_arithop(Kind kind) { return (thorin::ArithOpKind) to_binop(kind); }
    static thorin::CmpKind     to_cmp    (Kind kind) { return (thorin::CmpKind)     to_binop(kind); }
    static const char* tok2str(Kind kind);

    bool operator==(const Token& t) const { return kind_ == t; }
    bool operator!=(const Token& t) const { return kind_ != t; }

private:
    static void init();
    static Symbol insert(Kind tok, const char* str);
    static void insert_key(Kind tok, const char* str);

    Symbol symbol_;
    Kind kind_;
    thorin::Box box_;

    typedef thorin::HashMap<Symbol, Kind> Sym2Kind;
    typedef thorin::HashMap<Kind, const char*, KindHash> Kind2Str;
    typedef thorin::HashMap<Kind, Symbol, KindHash> Kind2Sym;
    static int tok2op_[Num_Tokens];
    static Kind2Str tok2str_;
    static Kind2Sym tok2sym_;
    static Sym2Kind keywords_;
    static Sym2Kind sym2lit_; ///< Table of \em all (including floating) suffixes for literals.
    static Sym2Kind sym2flit_;///< Table of suffixes for \em floating point literals.

    friend void init();
    friend std::ostream& operator<<(std::ostream& os, const Token& tok);
    friend std::ostream& operator<<(std::ostream& os, const Kind&  tok);
};

typedef Token::Kind TokenKind;

//------------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Token& tok);
std::ostream& operator<<(std::ostream& os, const TokenKind& tok);

//------------------------------------------------------------------------------

}

#endif
