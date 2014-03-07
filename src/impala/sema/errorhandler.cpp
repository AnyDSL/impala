#include "impala/sema/errorhandler.h"

#include "impala/ast.h"

namespace impala {

std::ostream& ErrorHandler::error(const ASTNode* n) { result_ = false; return n->error(); }
std::ostream& ErrorHandler::error(const Location& loc) { result_ = false; return loc.error(); }

}
