#ifndef IMPALA_LEXER_H
#define IMPALA_LEXER_H

#include <istream>

#include "thorin/debug.h"

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
    Loc curr() const { return loc_.anew_finis(); }

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
    Loc loc_;
    Pos peek_;
};

}

#endif
