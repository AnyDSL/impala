#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <istream>
#include <string>

#include "thorin/world.h"

#include "impala/token.h"
#include "impala/sema/typetable.h"

namespace impala {

extern bool fancy;

class ASTNode;
class ModContents;

void init();
void destroy();

struct Init {
    Init(std::string module_name)
        : world(std::move(module_name))
    {
        init();
    }
    ~Init() { destroy(); }

    thorin::World world;
    thorin::AutoPtr<TypeTable> typetable;
};

void parse(ModContents*, std::istream&, const char*);
void name_analysis(const ModContents* mod);
void type_analysis(Init&, const ModContents*, bool nossa);
void check(Init&, const ModContents* mod, bool nossa);
void emit(thorin::World&, const ModContents*);

std::ostream& warn(const ASTNode* n);            ///< Emit warning while using \p n as \p Location.
std::ostream& warn(const thorin::Location& loc); ///< Emit warning at \p Location \p loc.
std::ostream& error(const ASTNode* n);           ///< Emit error while using \p n as \p Location.
std::ostream& error(const thorin::Location& loc);///< Emit error at \p Location \p loc.

int num_warnings();
int num_errors();

enum Prec {
    BOTTOM,
    ASGN,
    COND,
    OROR,
    ANDAND,
    OR,
    XOR,
    AND,
    EQ,
    REL,
    SHIFT,
    ADD,
    MUL,
    UNARY,
    EVAL,
    POSTFIX,
    TOP,
    NUM_PREC
};

typedef Prec Type2Prec[Token::NUM_TOKENS];

struct BinPrec {
    Prec l;
    Prec r;

    BinPrec() {}
    BinPrec(Prec l, Prec r) : l(l), r(r) {}
};

typedef BinPrec Type2BinPrec[Token::NUM_TOKENS];

struct PrecTable {
    static Type2Prec prefix_r; ///< Right precedence -- for unary prefix operators.
    static Type2Prec infix_l;  ///< Left precedences -- for binary operators.
    static Type2Prec infix_r;  ///< Right precedences -- for binary operators.
    static Type2Prec postfix_l;///< Left precedence -- for unary postfix operators.

private:
    static void init();

    friend void init();
};

}

#endif
