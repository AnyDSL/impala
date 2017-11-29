#include "impala/lexer.h"

#include <cctype>
#include <cstdio>
#include <stdexcept>

#include "impala/impala.h"
#include "impala/symbol.h"

using namespace thorin;

namespace impala {

static inline bool sym(int c) { return std::isalpha(c) || c == '_'; }
static inline bool dec_nonzero(int c) { return c >= '1' && c <= '9'; }
static inline bool space(int c) { return std::isspace(c) != 0; }
static inline bool bin(int c) { return '0' <= c && c <= '1'; }
static inline bool oct(int c) { return '0' <= c && c <= '7'; }
static inline bool dec(int c) { return std::isdigit(c) != 0; }
static inline bool hex(int c) { return std::isxdigit(c) != 0; }
static inline bool eE(int c) { return c == 'e' || c == 'E'; }
static inline bool sgn(int c){ return c == '+' || c == '-'; }

Lexer::Lexer(std::istream& stream, const char* filename)
    : stream_(stream)
    , filename_(filename)
{
    if (!stream_)
        throw std::runtime_error("stream is bad");

    stream_.exceptions(std::istream::badbit);
}

int Lexer::next() {
    int c = stream_.get();

    back_line_ = peek_line_;
    back_col_  = peek_col_;

    if (c == '\n') {
        ++peek_line_;
        peek_col_ = 1;
    } else if (c != std::istream::traits_type::eof())
        ++peek_col_;

    return c;
}

Token Lexer::lex() {
    while (true) {
        std::string str; // the token string is concatenated here
        front_line_ = peek_line_;
        front_col_ = peek_col_;

        // end of file
        if (accept(std::istream::traits_type::eof()))
            return {location(), Token::Eof};

        // skip whitespace
        if (accept(space)) {
            while (accept(space)) {}
            continue;
        }

        // +, ++, +=
        if (accept('+')) {
            if (accept('+')) return {location(), Token::INC};
            if (accept('=')) return {location(), Token::ADD_ASGN};
            return {location(), Token::ADD};
        }

        // -, --, -=, ->
        if (accept('-')) {
            if (accept('-')) return {location(), Token::DEC};
            if (accept('=')) return {location(), Token::SUB_ASGN};
            if (accept('>')) return {location(), Token::ARROW};
            return {location(), Token::SUB};
        }

        // =, ==, =>
        if (accept('=')) {
            if (accept('=')) return {location(), Token::EQ};
            if (accept('>')) return {location(), Token::FAT_ARRROW};
            return {location(), Token::ASGN};
        }

        // *, *=, %, %=, ^, ^=, !, !=, :, :=
#define IMPALA_LEX_OP(op, tok1, tok2) \
        if (accept( op )) { \
            if (accept('=')) return {location(), Token:: tok2}; \
            return {location(), Token:: tok1}; \
        }
        IMPALA_LEX_OP('*', MUL, MUL_ASGN)
        IMPALA_LEX_OP('%', REM, REM_ASGN)
        IMPALA_LEX_OP('^', XOR, XOR_ASGN)
        IMPALA_LEX_OP('!', NOT,  NE)

        // <, <=, <<, <<=, >, >=, >>, >>=
#define IMPALA_LEX_REL_SHIFT(op, tok_rel, tok_rel_eq, tok_shift, tok_shift_asgn) \
        if (accept( op )) { \
            if (accept('=')) return {location(), Token:: tok_rel_eq}; \
            if (accept(op)) {  \
                if (accept('=')) return {location(), Token:: tok_shift_asgn}; \
                return {location(), Token:: tok_shift}; \
            } \
            return {location(), Token:: tok_rel}; \
        }
        IMPALA_LEX_REL_SHIFT('<', LT, LE, SHL, SHL_ASGN)
        IMPALA_LEX_REL_SHIFT('>', GT, GE, SHR, SHR_ASGN)

        // /, /=, comments
#define IMPALA_WITHIN_COMMENT(delim) \
        while (true) { \
            if (accept(std::istream::traits_type::eof())) { \
                error(location().front(), "unterminated comment"); \
                return {location(), Token::Eof}; \
            } \
            if (delim) break; \
            next(); /* eat up char in comment */\
        }
        if (accept('/')) {
            if (accept('='))
                return {location(), Token::DIV_ASGN};
            if (accept('*')) { // arbitrary comment
                IMPALA_WITHIN_COMMENT(accept('*') && accept('/'));
                continue;
            }
            if (accept('/')) { // end of line comment
                IMPALA_WITHIN_COMMENT(accept('\n'));
                continue;
            }
            return {location(), Token::DIV};
        }

        // &, &=, &&, |, |=, ||
#define IMPALA_LEX_AND_OR(op, tok_bit, tok_logic, tok_asgn) \
        if (accept( op )) { \
            if (accept('=')) \
                return {location(), Token:: tok_asgn}; \
            if (accept(op)) \
                return {location(), Token:: tok_logic}; \
            return {location(), Token:: tok_bit}; \
        }
        IMPALA_LEX_AND_OR('&', AND, ANDAND, AND_ASGN)
        IMPALA_LEX_AND_OR('|',  OR,   OROR,  OR_ASGN)

        if (accept(':')) {
            if (accept(':'))
                return {location(), Token::DOUBLE_COLON};
            return {location(), Token::COLON};
        }

        if (accept('@')) {
            if (accept('@'))
                return {location(), Token::RUNRUN};
            if (accept('?'))
                return {location(), Token::RUNKNOWN};
            return {location(), Token::RUN};
        }

        // single character tokens
        if (accept('(')) return {location(), Token::L_PAREN};
        if (accept(')')) return {location(), Token::R_PAREN};
        if (accept(',')) return {location(), Token::COMMA};
        if (accept(';')) return {location(), Token::SEMICOLON};
        if (accept('$')) return {location(), Token::HLT};
        if (accept('[')) return {location(), Token::L_BRACKET};
        if (accept(']')) return {location(), Token::R_BRACKET};
        if (accept('{')) return {location(), Token::L_BRACE};
        if (accept('}')) return {location(), Token::R_BRACE};
        if (accept('~')) return {location(), Token::TILDE};
        if (accept('?')) return {location(), Token::KNOWN};

        // '.', floats
        if (accept('.')) {
            if (accept(str, dec)) goto l_fractional_dot_rest;
            if (accept('.'))      return {location(), Token::DOTDOT};
            return {location(), Token::DOT};
        }

        // identifiers/keywords
        if (lex_identifier(str))
            return {location(), str};

        // char literal
        if (accept(str , '\'')) {
            while (!accept(str, '\'')) {
                accept(str, '\\');
                str += next();
                if (peek() == std::istream::traits_type::eof()) {
                    error(curr(), "missing terminating ' character");
                    str += '\''; // artificially append closing '
                    break;
                }
            }
            return {location(), Token::LIT_char, str};
        }

        // string literal
        if (accept(str , '"')) {
             while (!accept(str, '"')) {
                accept(str, '\\');
                str += next();
                if (peek() == std::istream::traits_type::eof()) {
                    error(curr(), "missing terminating \" character");
                    str += '\''; // artificially append closing "
                    break;
                }
            }
            return {location(), Token::LIT_str, str};
        }

        /*
         * literals
         */

        if (accept(str, dec_nonzero)) goto l_dec;
        if (accept(str, '0')) {
#define IMPALA_LEX_BASE_NUM(prefix, pred) \
            if (accept(str, (prefix))) { \
                while (accept(str, '_')) {} \
                if (accept(str, (pred))) { \
                    while (accept(str, (pred)) || accept(str, '_')) {} \
                    return lex_suffix(str, false); \
                } \
                return literal_error(str, false); \
            }

            IMPALA_LEX_BASE_NUM('b', bin)
            IMPALA_LEX_BASE_NUM('o', oct)
            IMPALA_LEX_BASE_NUM('x', hex)
            goto l_dec;
        }

        // invalid input char
        error(curr(), "invalid input character '{}'", (char) next());
        continue;

l_dec:                                      // [0-9_]*
        while (accept(str, dec) || accept(str, '_')) {}
        if (accept(str, '.')) {             // [0-9]
            if (accept(str, dec)) goto l_fractional_dot_rest;
            if (accept(str,  eE)) goto l_exp;
            return lex_suffix(str, true);
        }
        if (accept(str,  eE)) goto l_exp;
        return lex_suffix(str, false);

l_fractional_dot_rest:                      // [0-9_]*
        while (accept(str, dec) || accept(str, '_')) {}
        if (accept(str,  eE)) goto l_exp;
        return lex_suffix(str, true);

l_exp:                                      // [eE][+-]?[0-9_]+
        accept(str, sgn);
        if (accept(str, dec) || accept(str, '_')) {
            while (accept(str, dec) || accept(str, '_')) {}
            return lex_suffix(str, true);
        }
        return literal_error(str, true);
    }
}

bool Lexer::lex_identifier(std::string& str) {
    if (accept(str, sym)) {
        while (accept(str, sym) || accept(str, dec)) {}
        return true;
    }
    return false;
}

Token Lexer::lex_suffix(std::string& str, bool floating) {
    TokenTag tok = floating ? Token::LIT_f64 : Token::LIT_i32;
    std::string suffix_str;
    if (lex_identifier(suffix_str)) {
        Symbol suffix(suffix_str);
        if (floating) {
            auto lit = Token::sym2flit(suffix);
            if (lit == Token::Error) {
                error(location(), "invalid suffix on floating constant '{}'", suffix);
                return {location(), tok, str};
            }
            tok = lit;
        } else {
            auto lit = Token::sym2lit(suffix);
            if (lit == Token::Error) {
                error(location(), "invalid suffix on constant '{}'", suffix);
                return {location(), tok, str};
            }
            tok = lit;
        }
        str += suffix.str();
    }

    return {location(), tok, str};
}

Token Lexer::literal_error(std::string& str, bool floating) {
    error(location(), "invalid constant '{}'", str);
    return lex_suffix(str, floating);
}

}
