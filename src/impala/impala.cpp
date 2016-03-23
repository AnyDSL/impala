#include "impala/impala.h"

#include "impala/ast.h"
#include "impala/prec.h"
#include "impala/symbol.h"
#include "impala/token.h"

namespace impala {

bool fancy_output = false;

bool& fancy() { return fancy_output; }

void init() { PrecTable::init(); Token::init(); }
void destroy() { Symbol::destroy(); }
void check(Init& init, const ModContents* mod, bool) {
    name_analysis(mod);
    type_inference(init, mod);
    //type_analysis(mod, nossa);
    //borrow_check(mod);
}

int global_num_warnings = 0;
int global_num_errors = 0;

int num_warnings() { return global_num_warnings; }
int num_errors() { return global_num_errors; }

Type2Prec PrecTable::prefix_r;
Type2Prec PrecTable::infix_l;
Type2Prec PrecTable::infix_r;
Type2Prec PrecTable::postfix_l;

void PrecTable::init() {
#define IMPALA_PREFIX(    tok, t_str,    r)  PrecTable::prefix_r[Token:: tok] = r;
#define IMPALA_POSTFIX(   tok, t_str, l   ) PrecTable::postfix_l[Token:: tok] = l;
#define IMPALA_INFIX(     tok, t_str, l, r)   PrecTable::infix_l[Token:: tok] = l; PrecTable::infix_r[Token:: tok] = r;
#define IMPALA_INFIX_ASGN(tok, t_str, l, r)   PrecTable::infix_l[Token:: tok] = l; PrecTable::infix_r[Token:: tok] = r;
#include "impala/tokenlist.h"
}

}
