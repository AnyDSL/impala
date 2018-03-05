#include "impala/impala.h"

#include "thorin/util/symbol.h"

#include "impala/ast.h"
#include "impala/token.h"

namespace impala {

int global_num_warnings = 0;
int global_num_errors = 0;
bool fancy_output = false;

bool& fancy() { return fancy_output; }
int num_warnings() { return global_num_warnings; }
int num_errors() { return global_num_errors; }

void init() {
    PrecTable::init();
    Token::init();
}

void check(std::unique_ptr<TypeTable>& typetable, const Module* mod, bool nossa) {
    name_analysis(mod);
    type_inference(typetable, mod);
    type_analysis(mod, nossa);
    //borrow_check(mod);
}

Prec PrecTable::infix[Token::Num];

void PrecTable::init() {
#define IMPALA_INFIX(     tok, t_str, prec) PrecTable::infix[Token::tok] = Prec::prec;
#define IMPALA_INFIX_ASGN(tok, t_str)       PrecTable::infix[Token::tok] = Prec::Assign;
#include "impala/tokenlist.h"
}

}
