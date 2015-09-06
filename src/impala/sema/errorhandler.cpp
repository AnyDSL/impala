#include "impala/sema/errorhandler.h"

#include "impala/ast.h"

namespace impala {

std::ostream& ErrorHandler::warn(const ASTNode* n) { return n->warn(); }
std::ostream& ErrorHandler::warn(const thorin::Location& loc) { return loc.warn(); }
std::ostream& ErrorHandler::error(const ASTNode* n) { result_ = false; return n->error(); }
std::ostream& ErrorHandler::error(const thorin::Location& loc) { result_ = false; return loc.error(); }

}
