#ifndef IMPALA_DUMP_H
#define IMPALA_DUMP_H

#include "thorin/util/printer.h"

#include "impala/prec.h"
#include "impala/sema/unifiable.h"

#include <iostream>

namespace impala {

class Unifiable;                
class ASTNode;
class Expr;

class Printer : public thorin::Printer {
public:
    Printer(std::ostream& o, bool fancy)
        : thorin::Printer(o)
        , prec(BOTTOM)
        , fancy_(fancy)
    {}

    std::ostream& print(const Expr* expr);
    bool is_fancy() const { return fancy_; }

    Prec prec;

private:
    const bool fancy_;
};

/** 
 * @brief Dumps a human readable representation of the ASTNode \p n to output stream \p o.
 * 
 * @param n The \p ASTNode to dump.
 * @param fancy If set, the dumper makes minimal use of parenthesis for expressions.
 *  Otherwise, everything is put in parenthesis in order to fully debug the internal tree structure.
 * @param o The output stream where the dump is directed to.
 */
void dump(const ASTNode* n, bool fancy = false, std::ostream& o = std::cout);
std::ostream& operator << (std::ostream&, const ASTNode*);
template<class T> 
std::ostream& operator << (std::ostream& o, Proxy<T> u) { Printer p(o, true); return u->print(p); }

}

#endif
