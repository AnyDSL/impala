#ifndef IMPALA_SEMA_ERRORHANDLER_H
#define IMPALA_SEMA_ERRORHANDLER_H

#include <ostream>

namespace thorin {
class Location;
}

namespace impala {

class ASTNode;

class ErrorHandler {
public:
    ErrorHandler()
        : result_(true)
    {}
    virtual ~ErrorHandler() {}

    bool result() const { return result_; }          ///< Has an error occurred?
    std::ostream& warn(const ASTNode* n);            ///< Emit warning while using \p n as \p Location.
    std::ostream& warn(const thorin::Location& loc); ///< Emit warning at \p Location \p loc.
    std::ostream& error(const ASTNode* n);           ///< Emit error while using \p n as \p Location.
    std::ostream& error(const thorin::Location& loc);///< Emit error at \p Location \p loc.

private:
    bool result_;
};

}

#endif
