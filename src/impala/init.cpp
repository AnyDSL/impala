#include "impala/init.h"

#include "impala/prec.h"
#include "impala/symbol.h"
#include "impala/token.h"

namespace impala {

void init() { PrecTable::init(); Token::init(); }
void destroy() { Symbol::destroy(); }

} // namespace impala

