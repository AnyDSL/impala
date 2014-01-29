#ifndef IMPALA_SEMA_CHECK_H
#define IMPALA_SEMA_HCHECK_

namespace impala {

class Scope;
class TypeTable;

bool check(TypeTable&, const Scope*, bool nossa = false);

}

#endif
