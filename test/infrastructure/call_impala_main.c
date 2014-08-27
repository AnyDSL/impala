#include <stdio.h>
#include <stdlib.h>

int main_impala();

void* thorin_malloc(size_t size) { 
    void* p;
    posix_memalign(&p, 64, size);
    return p;
}

void println(const char* s) {
    printf("%s\n", s);
}

int main() {
    // main_impala returns bool => 1 if everything ok, 0 if not
    return main_impala() ? EXIT_SUCCESS : EXIT_FAILURE;
}
