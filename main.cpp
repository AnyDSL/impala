#include <iostream>

#include "type.h"

using namespace std;

void test_unification1() {
    TypeTable tt;

    TypeVar* A = tt.typevar();
    FnType* f = tt.fntype({A});         // fn(A)
    FnType* gf = tt.gentype({A}, f);    // fn<A>(A)

    // will fail because A is already bound!
    // TODO assert this failure
    FnType* gf2 = tt.gentype({A}, f);
}

void test_unification2() {
    TypeTable tt;

    TypeVar* A = tt.typevar();
    FnType* f = tt.fntype({A});         // fn(A)
    FnType* gf = tt.gentype({A}, f);    // fn<A>(A)

    TypeVar* B = tt.typevar();
    FnType* g = tt.fntype({B});         // fn(B)
    FnType* gg = tt.gentype({B}, g);    // fn<B>(B)

    gf->dump();
    gg->dump();

    assert(gg->equal(gf));

    cout << "test_unification2 [okay]" << endl;
}

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

    //test_unification1();
    test_unification2();
}
