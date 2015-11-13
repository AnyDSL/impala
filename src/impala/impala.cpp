#include "impala/impala.h"

#include <iostream>

#include "thorin/util/stream.h"

#include "impala/ast.h"
#include "impala/prec.h"
#include "impala/symbol.h"
#include "impala/token.h"

namespace impala {

bool fancy = false;

void init() { PrecTable::init(); Token::init(); }
void destroy() { Symbol::destroy(); }
void check(Init& init, const ModContents* mod, bool nossa) {
    name_analysis(mod);
    type_inference(init, mod, nossa);
    type_analysis(mod);
    borrow_check(mod);
}

int warnings = 0;
int errors = 0;

std::ostream& warn (const ASTNode* n) { return warn (n->loc()); }
std::ostream& error(const ASTNode* n) { return error(n->loc()); }
std::ostream& warn (const thorin::Location& loc) { ++warnings; return streamf(std::cerr, "%: warning: ", loc); }
std::ostream& error(const thorin::Location& loc) { ++errors;   return streamf(std::cerr, "%: error: "  , loc); }

int num_warnings() { return warnings; }
int num_errors() { return errors; }

/*static*/ Type2Prec PrecTable::prefix_r;
/*static*/ Type2Prec PrecTable::infix_l;
/*static*/ Type2Prec PrecTable::infix_r;
/*static*/ Type2Prec PrecTable::postfix_l;

void PrecTable::init() {
#define IMPALA_PREFIX(    tok, t_str,    r)  PrecTable::prefix_r[Token:: tok] = r;
#define IMPALA_POSTFIX(   tok, t_str, l   ) PrecTable::postfix_l[Token:: tok] = l;
#define IMPALA_INFIX(     tok, t_str, l, r)   PrecTable::infix_l[Token:: tok] = l; PrecTable::infix_r[Token:: tok] = r;
#define IMPALA_INFIX_ASGN(tok, t_str, l, r)   PrecTable::infix_l[Token:: tok] = l; PrecTable::infix_r[Token:: tok] = r;
#include "impala/tokenlist.h"
}

}
