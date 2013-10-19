#include <iostream>

#include "type.h"

using namespace std;

void simple_tests() {
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

    /*gf->dump();
    gf->get_representative()->dump();
    g->dump();
    g->get_representative()->dump();*/

    assert(gf == f);
    assert(gf == gg);

    assert(g != gg);
    assert(g->equal(gg));
    assert(g->get_representative() == gg->get_representative());

    assert(A->get_representative() == B->get_representative());

    cout << "test_unification2 [okay]" << endl;
}

void test_unification3() {
    TypeTable tt;

    TypeVar* A = tt.typevar();
    TypeVar* B = tt.typevar();
    FnType* f1 = tt.fntype({A, B});     // fn(A, B)
    FnType* gf1 = tt.gentype({B}, f1);  // fn<B>(A, B)
    FnType* f2 = tt.fntype({A, gf1});   // fn(A, fn<B>(A, B))
    FnType* gf2 = tt.gentype({A}, f2);  // fn<A>(A, fn<B>(A, B))

    TypeVar* C = tt.typevar();
    TypeVar* D = tt.typevar();
    FnType* g1 = tt.fntype({C, D});     // fn(C, D)
    FnType* gg1 = tt.gentype({D}, g1);  // fn<D>(C, D)
    FnType* g2 = tt.fntype({C, gg1});   // fn(C, fn<D>(C, D))
    FnType* gg2 = tt.gentype({C}, g2);  // fn<C>(C, fn<D>(C, D))

    //gf2->dump();
    //gg2->dump();

    assert(gf2 == gg2);
    assert(gf1 != gg1);
    assert(gf1->equal(gg1));
    assert(gf1->get_representative() == gg1->get_representative());

    TypeVar* E = tt.typevar();
    TypeVar* F = tt.typevar();
    FnType* h1 = tt.fntype({F, E});     // fn(F, E)
    FnType* gh1 = tt.gentype({F}, h1);  // fn<F>(F, E)
    FnType* h2 = tt.fntype({E, gh1});   // fn(E, fn<F>(F, E))
    FnType* gh2 = tt.gentype({E}, h2);  // fn<E>(E, fn<F>(F, E))

    //gh2->dump();

    assert(gf2 != gh2);
    assert(!gf2->equal(gh2));
    assert(gf2->get_representative() != gh2->get_representative());

    assert(gf1 != gh1);
    assert(!gf1->equal(gh1));
    assert(gf1->get_representative() != gh1->get_representative());

    TypeVar* G = tt.typevar();
    TypeVar* H = tt.typevar();
    FnType* k1 = tt.fntype({G, H});         // fn(G, H)
    FnType* k2 = tt.fntype({G, k1});        // fn(G, fn(G, H))
    FnType* gk2 = tt.gentype({G, H}, k2);   // fn<G,H>(G, fn(G, H))

    gk2->dump();

    assert(gf2 != gk2);
    assert(!gf2->equal(gk2));
    assert(gf2->get_representative() != gk2->get_representative());

    assert(gf1 != k1);
    assert(!gf1->equal(k1));
    assert(gf1->get_representative() != k1->get_representative());

    cout << "test_unification3 [okay]" << endl;
}

int main() {
    //simple_tests();
    //test_unification1();
    //test_unification2();
    test_unification3();
}
