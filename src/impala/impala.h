#ifndef IMPALA_IMPALA_H
#define IMPALA_IMPALA_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "thorin/world.h"
#include "thorin/util/stream.h"

#include "impala/token.h"
#include "impala/sema/type.h"

namespace impala {

class ASTNode;
class Item;
class Module;
typedef std::vector<std::unique_ptr<const Item>> Items;

void init();
void parse(Items&, std::istream&, const char*);
void name_analysis(const Module*);
void type_inference(std::unique_ptr<TypeTable>& typetable, const Module*);
void type_analysis(const Module*);
//void borrow_check(const ModContents*);
void check(std::unique_ptr<TypeTable>& typetable, const Module*);
void emit(thorin::World&, const Module*);

enum class Prec {
    Bottom,
    Assign = Bottom,
    Hlt,
    OrOr, AndAnd,
    Rel,
    Or, Xor, And,
    Shift, Add, Mul,
    As,
    Unary,
    RunRun,
};

struct PrecTable {
    static Prec infix[Token::Num];
    static Prec infix_l(int tag) { return infix[tag]; }
    static Prec infix_r(int tag) { return Prec(int(infix[tag])+1); }

private:
    static void init();
    friend void impala::init();
};

int& num_warnings();
int& num_errors();
bool& fancy();

template<class... Args>
void warning(const Loc& loc, const char* fmt, Args... args) {
    ++num_warnings();
    Stream s(std::cerr);
    s.streamf("{}: warning: ", loc).streamf(fmt, std::forward<Args>(args)...).endl();
}

template<class... Args>
void error(const Loc& loc, const char* fmt, Args... args) {
    ++num_errors();
    Stream s(std::cerr);
    s.streamf("{}: error: ", loc).streamf(fmt, std::forward<Args>(args)...).endl();
}

}

#endif
