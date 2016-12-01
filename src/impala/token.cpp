#include "impala/token.h"

#include <algorithm>
#include <cerrno>
#include <cstdlib>
#include <limits>

#include "thorin/util/cast.h"

#include "impala/impala.h"

using namespace thorin;

namespace impala {

Token::Token(Location location, Kind tok)
    : location_(location)
    , symbol_(tok2sym_[tok])
    , kind_(tok)
{}

Token::Token(Location location, const std::string& str)
    : location_(location)
    , symbol_(str)
{
    assert(!str.empty());
    auto i = keywords_.find(str);
    if (i == keywords_.end())
        kind_ = Token::ID;
    else
        kind_ = i->second;
}

template<class T, class V>
static bool inrange(V val) {
    return std::numeric_limits<T>::lowest() <= val && val <= std::numeric_limits<T>::max();
}

Token::Token(Location location, Kind kind, const std::string& str)
    : location_(location)
    , symbol_(str)
    , kind_(kind)
{
    using namespace std;
    using thorin::half;

    if (kind_ == LIT_str || kind_ == LIT_char)
        return;

    std::string literal;
    int base = 10;
    auto begin = str.begin();

    // find out base and move begin iterator to the actual number
    if (str.size() >= 2) {
        if (str[0] == '0') {
            if (str[1] == 'b') {
                base = 2;
                begin += 2;
            } else if (str[1] == 'o') {
                base = 8;
                begin += 2;
            } else if (str[1] == 'x') {
                base = 16;
                begin += 2;
            }
        }
    }

    // remove underscores and '0b'/'0o'/'0x' prefix if applicable
    std::copy_if(begin, str.end(), std::back_inserter(literal), [](char c) { return c != '_'; });
    auto nptr = &literal.front();

    bool err = 0;
    errno = 0;
    int64_t ival; uint64_t uval; half hval; float fval; double dval;

    switch (kind_) {
        case LIT_i8: case LIT_i16: case LIT_i32: case LIT_i64:
                      ival = strtoll (nptr, 0, base);  err = errno; break;
        case LIT_u8: case LIT_u16: case LIT_u32: case LIT_u64:
                      uval = strtoull(nptr, 0, base);  err = errno; break;
        case LIT_f16: hval = strtof(symbol_.str(), 0); err = errno; break; // TODO: errno for half not correctly set
        case LIT_f32: fval = strtof(symbol_.str(), 0); err = errno; break;
        case LIT_f64: dval = strtod(symbol_.str(), 0); err = errno; break;
        default: THORIN_UNREACHABLE;
    }

    switch (kind_) {
        case LIT_i8:  box_ =   int8_t(ival); err |= !inrange<  int8_t>(ival); break;
        case LIT_i16: box_ =  int16_t(ival); err |= !inrange< int16_t>(ival); break;
        case LIT_i32: box_ =  int32_t(ival); err |= !inrange< int32_t>(ival); break;
        case LIT_i64: box_ =  int64_t(ival); err |= !inrange< int64_t>(ival); break;
        case LIT_u8:  box_ =  uint8_t(uval); err |= !inrange< uint8_t>(uval); break;
        case LIT_u16: box_ = uint16_t(uval); err |= !inrange<uint16_t>(uval); break;
        case LIT_u32: box_ = uint32_t(uval); err |= !inrange<uint32_t>(uval); break;
        case LIT_u64: box_ = uint64_t(uval); err |= !inrange<uint64_t>(uval); break;
        case LIT_f16: box_ =     half(hval); err |= !inrange<    half>(hval); break;
        case LIT_f32: box_ =    float(fval); err |= !inrange<   float>(fval); break;
        case LIT_f64: box_ =   double(dval); err |= !inrange<  double>(dval); break;
        default: THORIN_UNREACHABLE;
    }

    if (err)
        switch (kind_) {
#define IMPALA_LIT(itype, atype) \
            case LIT_##itype: error(location, "literal out of range for type '%'," #itype); return;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

bool Token::is_rel(Kind op) {
    switch (op) {
        case EQ: case LT: case LE:
        case NE: case GT: case GE: return true;
        default: return false;
    }
}

TokenKind Token::separate_assign(TokenKind kind) {
    assert(is_assign(kind) && "must be an assignment other than ASGN");

    switch (kind) {
        case ADD_ASGN: return ADD;
        case SUB_ASGN: return SUB;
        case MUL_ASGN: return MUL;
        case DIV_ASGN: return DIV;
        case REM_ASGN: return REM;
        case AND_ASGN: return AND;
        case  OR_ASGN: return OR;
        case XOR_ASGN: return XOR;
        case SHL_ASGN: return SHL;
        case SHR_ASGN: return SHR;
        default: THORIN_UNREACHABLE;
    }
}

int Token::to_binop(Kind kind) {
    switch (kind) {
        case INC:
        case ADD: return ArithOp_add;
        case DEC:
        case SUB: return ArithOp_sub;
        case MUL: return ArithOp_mul;
        case DIV: return ArithOp_div;
        case REM: return ArithOp_rem;
        case AND: return ArithOp_and;
        case  OR: return ArithOp_or;
        case XOR: return ArithOp_xor;
        case SHL: return ArithOp_shl;
        case SHR: return ArithOp_shr;
        case  EQ: return Cmp_eq;
        case  NE: return Cmp_ne;
        case  LT: return Cmp_lt;
        case  LE: return Cmp_le;
        case  GT: return Cmp_gt;
        case  GE: return Cmp_ge;
        default: THORIN_UNREACHABLE;
    }
}

/*
 * static member variables
 */

int Token::tok2op_[Num_Tokens];
Token::Kind2Str Token::tok2str_;
Token::Kind2Sym Token::tok2sym_;
Token::Sym2Kind Token::keywords_;
Token::Sym2Kind Token::sym2lit_;
Token::Sym2Kind Token::sym2flit_;

/*
 * static methods
 */

TokenKind Token::sym2lit(Symbol sym) {
    auto i = sym2lit_.find(sym);
    if (i != sym2lit_.end())
        return i->second;
    return TYPE_error;
}

TokenKind Token::sym2flit(Symbol sym) {
    auto i = sym2flit_.find(sym);
    if (i != sym2flit_.end())
        return i->second;
    return TYPE_error;
}

void Token::init() {
    THORIN_CALL_ONCE;

    /*
     * - set pre-/in-/postfix operators
     * - register literals
     * - register keywords
     * - register misc tokens
     */

    for (size_t i = 0; i < Num_Tokens; ++i)
        tok2op_[i] = None;

#define IMPALA_PREFIX(    tok, str, r   ) insert(tok, str); tok2op_[tok] |= Prefix;
#define IMPALA_POSTFIX(   tok, str,    l) insert(tok, str); tok2op_[tok] |= Postfix;
#define IMPALA_INFIX(     tok, str, r, l) insert(tok, str); tok2op_[tok] |= Infix;
#define IMPALA_INFIX_ASGN(tok, str, r, l) insert(tok, str); tok2op_[tok] |= Infix | Asgn_Op;
#define IMPALA_MISC(      tok, str)       insert(tok, str);
#define IMPALA_KEY(       tok, str)       insert_key(tok, str);
#define IMPALA_LIT(       tok, atype)     tok2str_[LIT_##tok] = Symbol("<literal>").str();
#define IMPALA_TYPE(itype, atype)         insert_key(TYPE_ ## itype, #itype );
#include "impala/tokenlist.h"

    // type aliases
    insert_key(TYPE_i32, "int");
    insert_key(TYPE_u32, "uint");
    insert_key(TYPE_f16, "half");
    insert_key(TYPE_f32, "float");
    insert_key(TYPE_f64, "double");

    // literals
    sym2lit_["i"]   = LIT_i32; sym2lit_["u"]   = LIT_u32;
    sym2lit_["i8"]  = LIT_i8;  sym2lit_["u8"]  = LIT_u8;
    sym2lit_["i16"] = LIT_i16; sym2lit_["u16"] = LIT_u16;
    sym2lit_["i32"] = LIT_i32; sym2lit_["u32"] = LIT_u32;
    sym2lit_["i64"] = LIT_i64; sym2lit_["u64"] = LIT_u64;

    sym2lit_["h"]   = LIT_f16; sym2flit_["h"]   = LIT_f16;
    sym2lit_["f16"] = LIT_f16; sym2flit_["f16"] = LIT_f16;
    sym2lit_["f"]   = LIT_f32; sym2flit_["f"]   = LIT_f32;
    sym2lit_["f32"] = LIT_f32; sym2flit_["f32"] = LIT_f32;
    sym2lit_["f64"] = LIT_f64; sym2flit_["f64"] = LIT_f64;

    // special tokens
    tok2str_[ID]         = Symbol("<identifier>").str();
    insert(END_OF_FILE, "<end of file>");
    insert_key(MUT, "mut");
}

void Token::insert_key(TokenKind tok, const char* str) {
    Symbol s = str;
    assert(keywords_.find(s) == keywords_.end() && "already inserted");
    keywords_[s] = tok;
    tok2str_ [tok] = s.str();
}

Symbol Token::insert(TokenKind tok, const char* str) {
    Symbol s = str;
    const auto& p = tok2sym_.emplace(tok, s);

#ifndef NDEBUG
    if (!p.second) {
        Kind   oldTok = p.first->first;
        Symbol oldSym = p.first->second;
        assert(s == oldSym && tok == oldTok && "inserted ambiguous duplicate");
    }
#endif

    tok2str_[tok] = s.str();
    return p.first->second;
}

//------------------------------------------------------------------------------

const char* Token::tok2str(TokenKind kind) {
    auto i = Token::tok2str_.find(kind);
    assert(i != Token::tok2str_.end() && "must be found");
    return Symbol(i->second).str();
}

std::ostream& operator<<(std::ostream& os, const TokenKind& kind) { return os << Token::tok2str(kind); }

std::ostream& operator<<(std::ostream& os, const Token& tok) {
    const char* sym = tok.symbol().str();
    if (std::strcmp(sym, "") == 0)
        return os << Symbol(Token::tok2str_[tok.kind()]).str();
    else
        return os << sym;
}

//------------------------------------------------------------------------------

}
