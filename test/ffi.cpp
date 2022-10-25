#include "ffi.h"

void test(int& p, closure<void (int)> c, closure<int (int)> c2) {
    c(0);
    c(1);
    p = c2(1) + c2(2);
}
