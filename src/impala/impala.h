#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <iostream>
#include <string>

#include "thorin/world.h"
#include "thorin/util/stream.h"

#include "impala/token.h"
#include "impala/sema/typetable.h"

namespace impala {

bool& fancy();

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
void name_analysis(const ModContents*);
void type_inference(Init&, const ModContents*);
void type_analysis(const ModContents*, bool nossa);
//void borrow_check(const ModContents*);
void check(Init&, const ModContents*, bool nossa);
void emit(thorin::World&, const ModContents*);

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

extern int global_num_warnings;
extern int global_num_errors;

int num_warnings();
int num_errors();

template<typename... Args>
std::ostream& warning(const thorin::Location& loc, const char* fmt, Args... args) {
    ++global_num_warnings;
    thorin::streamf(std::cerr, "%: warning: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

template<typename... Args>
std::ostream& error(const thorin::Location& loc, const char* fmt, Args... args) {
    ++global_num_errors;
    thorin::streamf(std::cerr, "%: error: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

}

#endif
