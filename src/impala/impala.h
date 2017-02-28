#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "thorin/world.h"
#include "thorin/util/stream.h"

#include "impala/token.h"
#include "impala/sema/typetable.h"

namespace impala {

bool& fancy();

class ASTNode;
class Item;
class Module;
typedef std::vector<std::unique_ptr<const Item>> Items;

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
    std::unique_ptr<TypeTable> typetable;
};

void parse(Items&, std::istream&, const char*);
void name_analysis(const Module*);
void type_inference(Init&, const Module*);
void type_analysis(const Module*, bool nossa);
//void borrow_check(const ModContents*);
void check(Init&, const Module*, bool nossa);
void emit(thorin::World&, const Module*);

enum Prec {
    Bottom,
    Assign = Bottom,
    OrOr, AndAnd,
    Eq, Rel,
    Or, Xor, And,
    Shift, Add, Mul,
    Unary,
    Eval,
    As,
    Postfix,
};

typedef Prec Type2Prec[Token::Num_Tokens];

struct BinPrec {
    Prec l;
    Prec r;

    BinPrec() {}
    BinPrec(Prec l, Prec r) : l(l), r(r) {}
};

typedef BinPrec Type2BinPrec[Token::Num_Tokens];

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
    thorin::streamf(std::cerr, "{}: warning: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

template<typename... Args>
std::ostream& error(const thorin::Location& loc, const char* fmt, Args... args) {
    ++global_num_errors;
    thorin::streamf(std::cerr, "{}: error: ", loc);
    return thorin::streamf(std::cerr, fmt, args...) << std::endl;;
}

}

#endif
