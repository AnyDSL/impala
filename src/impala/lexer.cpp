#include "impala/lexer.h"

#include <cctype>
#include <cstdio>
#include <stdexcept>

#include "thorin/util/symbol.h"

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

Lexer::Lexer(std::istream& stream, const std::string& filename)
    : stream_(stream)
    , pos_(filename, 1, 1)
    , loc_(pos_) 
    , result_(true)
{
    if (!stream_)
        throw std::runtime_error("stream is bad");

    stream_.exceptions(std::istream::badbit);
}

std::ostream& Lexer::error(const Location& loc) {
    result_ = false;
    return loc.error();
}

int Lexer::next() {
    int c = stream_.get();

    loc_.set_pos2(pos_);

    if (c == '\n') {
        pos_.inc_line();
        pos_.reset_col();
    } else if (c != std::istream::traits_type::eof())
        pos_.inc_col();

    return c;
}

Token Lexer::lex() {
    while (true) {
        std::string str; // the token string is concatenated here
        bool floating = false;
        loc_.set_pos1(pos_);

        // end of file
        if (accept(std::istream::traits_type::eof()))
            return Token(loc_, Token::END_OF_FILE);

        // skip whitespace
        if (accept(space)) {
            while (accept(space)) {}
            continue;
        }

        // +, ++, +=
        if (accept('+')) {
            if (accept('+')) return Token(loc_, Token::INC);
            if (accept('=')) return Token(loc_, Token::ADD_ASGN);
            return Token(loc_, Token::ADD);
        }

        // -, --, -=, ->
        if (accept('-')) {
            if (accept('-')) return Token(loc_, Token::DEC);
            if (accept('=')) return Token(loc_, Token::SUB_ASGN);
            if (accept('>')) return Token(loc_, Token::ARROW);
            return Token(loc_, Token::SUB);
        }

        // =, ==, =>
        if (accept('=')) {
            if (accept('=')) return Token(loc_, Token::EQ);
            if (accept('>')) return Token(loc_, Token::FAT_ARRROW);
            return Token(loc_, Token::ASGN);
        }

        // *, *=, %, %=, ^, ^=, !, !=, :, :=
#define IMPALA_LEX_OP(op, tok1, tok2) \
        if (accept( op )) { \
            if (accept('=')) return Token(loc_, Token:: tok2); \
            return Token(loc_, Token:: tok1); \
        }
        IMPALA_LEX_OP('*', MUL, MUL_ASGN)
        IMPALA_LEX_OP('%', REM, REM_ASGN)
        IMPALA_LEX_OP('^', XOR, XOR_ASGN)
        IMPALA_LEX_OP('!', NOT,  NE)

        // <, <=, <<, <<=, >, >=, >>, >>=
#define IMPALA_LEX_REL_SHIFT(op, tok_rel, tok_rel_eq, tok_shift, tok_shift_asgn) \
        if (accept( op )) { \
            if (accept('=')) return Token(loc_, Token:: tok_rel_eq); \
            if (accept(op)) {  \
                if (accept('=')) return Token(loc_, Token:: tok_shift_asgn); \
                return Token(loc_, Token:: tok_shift); \
            } \
            return Token(loc_, Token:: tok_rel); \
        }
        IMPALA_LEX_REL_SHIFT('<', LT, LE, SHL, SHL_ASGN)
        IMPALA_LEX_REL_SHIFT('>', GT, GE, SHR, SHR_ASGN)

        // /, /=, comments
#define IMPALA_WITHIN_COMMENT(delim) \
        while (true) { \
            if (accept(std::istream::traits_type::eof())) { \
                error(loc_.pos1()) << "unterminated comment\n"; \
                return Token(loc_, Token::END_OF_FILE); \
            } \
            if (delim) break; \
            next(); /* eat up char in comment */\
        }
        if (accept('/')) {
            if (accept('='))
                return Token(loc_, Token::DIV_ASGN);
            if (accept('*')) { // arbitrary comment
                IMPALA_WITHIN_COMMENT(accept('*') && accept('/'));
                continue;
            } 
            if (accept('/')) { // end of line comment
                IMPALA_WITHIN_COMMENT(accept('\n'));
                continue;
            }
            return Token(loc_, Token::DIV);
        }

        // &, &=, &&, |, |=, ||
#define IMPALA_LEX_AND_OR(op, tok_bit, tok_logic, tok_asgn) \
        if (accept( op )) { \
            if (accept('=')) \
                return Token(loc_, Token:: tok_asgn); \
            if (accept(op)) \
                return Token(loc_, Token:: tok_logic); \
            return Token(loc_, Token:: tok_bit); \
        }
        IMPALA_LEX_AND_OR('&', AND, ANDAND, AND_ASGN)
        IMPALA_LEX_AND_OR('|',  OR,   OROR,  OR_ASGN)

        if (accept(':')) {
            if (accept(':'))
                return Token(loc_, Token::DOUBLE_COLON);
            return Token(loc_, Token::COLON);
        }

        // single character tokens
        if (accept('(')) return Token(loc_, Token::L_PAREN);
        if (accept(')')) return Token(loc_, Token::R_PAREN);
        if (accept(',')) return Token(loc_, Token::COMMA);
        if (accept(';')) return Token(loc_, Token::SEMICOLON);
        if (accept('@')) return Token(loc_, Token::RUN);
        if (accept('$')) return Token(loc_, Token::HALT);
        if (accept('[')) return Token(loc_, Token::L_BRACKET);
        if (accept(']')) return Token(loc_, Token::R_BRACKET);
        if (accept('{')) return Token(loc_, Token::L_BRACE);
        if (accept('}')) return Token(loc_, Token::R_BRACE);
        if (accept('~')) return Token(loc_, Token::TILDE);

        // '.', floats
        if (accept('.')) {
            if (accept(str, dec)) goto l_fractional_dot_rest;
            if (accept('.'))      return Token(loc_, Token::DOTDOT);
            return Token(loc_, Token::DOT);
        }

        // identifiers/keywords
        if (lex_identifier(str))
            return Token(loc_, str);

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
        error(pos_) << "invalid input character '" << (char) next() << "'\n";
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
    TokenKind tok = floating ? Token::LIT_f64 : Token::LIT_i32;
    std::string suffix_str;
    if (lex_identifier(suffix_str)) {
        Symbol suffix(suffix_str);
        if (floating) {
            auto lit = Token::sym2flit(suffix);
            if (lit == Token::TYPE_error) {
                error(loc_) << "invalid suffix on floating constant '" << suffix << "'\n";
                return Token(loc_, tok, str);
            }
            tok = lit;
        } else {
            auto lit = Token::sym2lit(suffix);
            if (lit == Token::TYPE_error) {
                error(loc_) << "invalid suffix on constant '" << suffix << "'\n";
                return Token(loc_, tok, str);
            }
            tok = lit;
        }
        str += suffix;
    }
    
    return Token(loc_, tok, str);
}

Token Lexer::literal_error(std::string& str, bool floating) {
    error(pos_) << "invalid constant '" << str << "'\n";
    return lex_suffix(str, floating);
}

}
