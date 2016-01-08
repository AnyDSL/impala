#include <stdio.h>
#include <stdlib.h>

void* thorin_malloc(size_t size) {
    void* p;
    posix_memalign(&p, 64, size);
    return p;
}

void println(const char* s) {
    printf("%s\n", s);
}
