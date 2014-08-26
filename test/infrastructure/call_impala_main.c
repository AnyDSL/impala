#include <stdio.h>
#include <stdlib.h>

int main_impala();

void println(const char* s) {
    printf("%s\n", s);
}

int main() {
    // main_impala returns bool => 1 if everything ok, 0 if not
    return main_impala() ? EXIT_SUCCESS : EXIT_FAILURE;
}
