#ifndef IMPALA_SEMA_ERRORHANDLER_H
#define IMPALA_SEMA_ERRORHANDLER_H

#include <ostream>

namespace impala {

class ASTNode;
class Location;

class ErrorHandler {
public:
    ErrorHandler(const bool result)
        : result_(result)
    {}

    bool result() const { return result_; }   ///< Has an error occurred?
    std::ostream& error(const ASTNode* n);    ///< Emit an error while using \p n as \p Location.
    std::ostream& error(const Location& loc); ///< Emit an error at \p Location \p loc.

private:
    bool result_;
};

}

#endif
