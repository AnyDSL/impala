#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/scopetable.h"

namespace impala {

class Sema : public ScopeTable {
public:
    Sema(bool nossa)
        : cur_fn_(nullptr)
        , nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    const Fn* cur_fn() const { return cur_fn_; }

private:
    const Fn* cur_fn_;
    bool nossa_;
};

/*
 * expressions
 */

void EmptyExpr::check(Sema& sema) const {
}

void BlockExpr::check(Sema& sema) const {
}

void LiteralExpr::check(Sema& sema) const {
}

void FnExpr::check(Sema& sema) const {
}

void IdExpr::check(Sema& sema) const {
}

void PrefixExpr::check(Sema& sema) const {
}

void InfixExpr::check(Sema& sema) const {
}

void PostfixExpr::check(Sema& sema) const {
}

void FieldExpr::check(Sema& sema) const {
}

void CastExpr::check(Sema& sema) const {
}

void ArrayExpr::check(Sema& sema) const {
}

void RepeatArrayExpr::check(Sema& sema) const {
}

void TupleExpr::check(Sema& sema) const {
}

void StructExpr::check(Sema& sema) const {
}

void MapExpr::check(Sema& sema) const {
}

void IfExpr::check(Sema& sema) const {
}

void ForExpr::check(Sema& sema) const {
}


}
