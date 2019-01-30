#ifndef IMPALA_LEXER_H
#define IMPALA_LEXER_H

#include <istream>

#include "thorin/util/debug.h"

#include "impala/token.h"

namespace impala {

class Lexer {
public:
    Lexer(std::istream& stream, const char* filename);

    Token lex(); ///< Get next \p Token in stream.

private:
    bool lex_identifier(std::string&);
    Token lex_suffix(std::string&, bool floating);
    Token literal_error(std::string&, bool floating);
    int next();
    int peek() const { return stream_.peek(); }
    Loc loc() const { return {filename_, front_line_, front_col_, back_line_, back_col_}; }
    Loc curr() const { return loc().back(); }

    template<class Pred>
    bool accept(std::string& str, Pred pred) {
        if (pred(peek())) {
            str += next();
            return true;
        }
        return false;
    }

    template<class Pred>
    bool accept(Pred pred) {
        if (pred(peek())) {
            next();
            return true;
        }
        return false;
    }

    bool accept(int expect) { return accept([&] (int got) { return got == expect; }); }
    bool accept(std::string& str, int expect) { return accept(str, [&] (int got) { return got == expect; }); }
    bool accept(char c) { return accept((int) c); }
    bool accept(std::string& str, char c) { return accept(str, (int) c); }

    std::istream& stream_;
    const char* filename_;
    uint32_t front_line_ = 1, front_col_ = 1, back_line_ = 1, back_col_ = 1, peek_line_ = 1, peek_col_ = 1;
};

}

#endif
