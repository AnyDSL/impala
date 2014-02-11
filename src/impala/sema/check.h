#ifndef IMPALA_SEMA_CHECK_H
#define IMPALA_SEMA_CHECK_H

namespace impala {

class Scope;

bool check(const ModContents*, bool nossa = false);

}

#endif
