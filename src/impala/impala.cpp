#include "impala/impala.h"

#include <iostream>

#include "thorin/util/stream.h"

#include "impala/ast.h"
#include "impala/prec.h"
#include "impala/symbol.h"
#include "impala/token.h"

namespace impala {

void init() { PrecTable::init(); Token::init(); }
void destroy() { Symbol::destroy(); }
void check(Init& init, const ModContents* mod, bool nossa) { name_analysis(mod); type_analysis(init, mod, nossa); }

int warnings = 0;
int errors = 0;

std::ostream& warn (const ASTNode* n) { return warn (n->loc()); }
std::ostream& error(const ASTNode* n) { return error(n->loc()); }
std::ostream& warn (const thorin::Location& loc) { ++warnings; return streamf(std::cerr, "%: warning: ", loc); }
std::ostream& error(const thorin::Location& loc) { ++errors;   return streamf(std::cerr, "%: error: "  , loc); }

int num_warnings() { return warnings; }
int num_errors() { return errors; }

}
