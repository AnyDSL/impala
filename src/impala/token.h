#ifndef IMPALA_TOKEN_H
#define IMPALA_TOKEN_H

#include <ostream>
#include <string>

#include "thorin/enums.h"

#include "impala/loc.h"
#include "impala/symbol.h"

namespace impala {

using thorin::Symbol;

class Token {
public:
    enum Tag {
        // !!! DO NOT CHANGE THIS ORDER !!!
        // add prefix and postfix tokens manually in order to avoid duplicates in the enum
#define IMPALA_INFIX(     tok, t_str, prec) tok,
#define IMPALA_INFIX_ASGN(tok, t_str)       tok,
#define IMPALA_KEY(       tok, t_str)       tok,
#define IMPALA_MISC(      tok, t_str)       tok,
#define IMPALA_LIT(       tok, t)           LIT_##tok,
#define IMPALA_TYPE(itype, atype)           TYPE_##itype,
#include "impala/tokenlist.h"

        // manually insert missing unary prefix/postfix types
        TILDE, NOT, INC, DEC, HLT, KNOWN, DOT, RUN, RUNKNOWN, RUNRUN,
        // these do not appear in impala/tokenlist.h -- they are too special
        MUT, ID, Eof, Error,
        LIT_char, LIT_str,
        Num,
    };

    struct TagHash {
        static uint32_t hash(Tag tag) { return uint32_t(tag); }
        static bool eq(Tag k1, Tag k2) { return k1 == k2; }
        static Tag sentinel() { return Num; }
    };

    Token() {}
    /// Create an operator token
    Token(Loc loc, Tag tok);
    /// Create an identifier or a keyword (depends on \p str)
    Token(Loc loc, const std::string& str);
    /// Create a literal
    Token(Loc loc, Tag type, const std::string& str);

    Loc loc() const { return loc_; }
    Symbol symbol() const { return symbol_; }
    thorin::Box box() const { return box_; }
    Tag tag() const { return tag_; }
    operator Tag() const { return tag_; }

    enum Op {
        None    = 0,
        Prefix  = 1,
        Infix   = 2,
        Postfix = 4,
        Asgn_Op = 8
    };

    bool is_prefix()    const { return is_prefix(tag_); }
    bool is_infix()     const { return is_infix(tag_); }
    bool is_postfix()   const { return is_postfix(tag_); }
    bool is_assign()    const { return is_assign(tag_); }
    bool is_op()        const { return is_op(tag_); }

    static Tag sym2lit(Symbol sym);
    static Tag sym2flit(Symbol sym);
    static bool is_prefix(Tag tag)  { return (tok2op_[tag] &  Prefix) != 0; }
    static bool is_infix(Tag tag)   { return (tok2op_[tag] &   Infix) != 0; }
    static bool is_postfix(Tag tag) { return (tok2op_[tag] & Postfix) != 0; }
    static bool is_assign(Tag tag)  { return (tok2op_[tag] & Asgn_Op) != 0; }
    static bool is_op(Tag tag)      { return is_prefix(tag) || is_infix(tag) || is_postfix(tag); }
    static const char* tok2str(Tag tag);

    bool operator==(const Token& t) const { return tag_ == t; }
    bool operator!=(const Token& t) const { return tag_ != t; }

private:
    static void init();
    static Symbol insert(Tag tok, const char* str);
    static void insert_key(Tag tok, const char* str);

    Loc loc_;
    Symbol symbol_;
    Tag tag_;
    thorin::Box box_;

    typedef thorin::HashMap<Symbol, Tag> Sym2Tag;
    typedef thorin::HashMap<Tag, const char*, TagHash> Tag2Str;
    typedef thorin::HashMap<Tag, Symbol, TagHash> Tag2Sym;
    static int tok2op_[Num];
    static Tag2Str tok2str_; // TODO do we need this thing?
    static Tag2Sym tok2sym_;
    static Sym2Tag keywords_;
    static Sym2Tag sym2lit_; ///< Table of \em all (including floating) suffixes for literals.
    static Sym2Tag sym2flit_;///< Table of suffixes for \em floating point literals.

    friend void init();
    friend std::ostream& operator<<(std::ostream& os, const Token& tok);
    friend std::ostream& operator<<(std::ostream& os, const Tag&  tok);
};

typedef Token::Tag TokenTag;

//------------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const Token& tok);
std::ostream& operator<<(std::ostream& os, const TokenTag& tok);

//------------------------------------------------------------------------------

}

#endif
