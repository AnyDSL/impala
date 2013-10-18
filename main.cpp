#include <iostream>

#include "type.h"

int main() {
    TypeTable tt;

    // create some test types
    Type* t1 = tt.tupletype( { tt.type_int(), tt.type_bool() }); // tuple(int, bool)
    Type* t2 = tt.tupletype( { tt.type_float(), tt.type_float(), t1 }); // tuple(float, float, tuple(int, bool))
    Type* t3 = tt.tupletype( { tt.type_float(), tt.type_float(), t1 }); // tuple(float, float, tuple(int, bool))

    // dump those types
    t1->dump();
    t2->dump();
    t3->dump();

    // check for equality
    std::cout << (t1 == t2) << std::endl; // 0
    std::cout << (t1 == t3) << std::endl; // 0
    std::cout << (t2 == t3) << std::endl; // 1

    // create an fn<a,b>(a, b)
    TypeVar* a = tt.typevar();
    TypeVar* b = tt.typevar();
    FnType* f = tt.fntype({a, b});
    const FnType* gen_f = tt.gentype({a, b}, f);
    gen_f->dump();
}
