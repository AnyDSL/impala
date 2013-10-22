#include <iostream>

#include "type.h"

using namespace std;

void simple_tests() {
    TypeTable tt;

    // create some test types
    const Type* t1 = tt.tupletype( { tt.type_int(), tt.type_bool() }); // tuple(int, bool)
    const Type* t2 = tt.tupletype( { tt.type_float(), tt.type_float(), t1 }); // tuple(float, float, tuple(int, bool))
    const Type* t3 = tt.tupletype( { tt.type_float(), tt.type_float(), t1 }); // tuple(float, float, tuple(int, bool))

    // dump those types
    t1->dump();
    t2->dump();
    t3->dump();

    // check for equality
    std::cout << (t1 == t2) << std::endl; // 0
    std::cout << (t1 == t3) << std::endl; // 0
    std::cout << (t2 == t3) << std::endl; // 1

    // create an fn<a,b>(a, b)
    const TypeVar* a = tt.typevar();
    const TypeVar* b = tt.typevar();
    const FnType* f = tt.fntype({a, b});
    const FnType* gen_f = tt.gentype({a, b}, f);
    gen_f->dump();

    tt.check_sanity();

    cout << "simple_tests [okay]" << endl;
}

void test_unification2() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const FnType* f = tt.fntype({A});         // fn(A)
    const FnType* gf = tt.gentype({A}, f);    // fn<A>(A)

    const TypeVar* B = tt.typevar();
    const FnType* g = tt.fntype({B});         // fn(B)
    const FnType* gg = tt.gentype({B}, g);    // fn<B>(B)

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

    tt.check_sanity();
    check_sanity({A, f, gf, B, g, gg});

    cout << "test_unification2 [okay]" << endl;
}

void test_unification3() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const TypeVar* B = tt.typevar();
    const FnType* f1 = tt.fntype({A, B});     // fn(A, B)
    const FnType* gf1 = tt.gentype({B}, f1);  // fn<B>(A, B)
    const FnType* f2 = tt.fntype({A, gf1});   // fn(A, fn<B>(A, B))
    const FnType* gf2 = tt.gentype({A}, f2);  // fn<A>(A, fn<B>(A, B))

    const TypeVar* C = tt.typevar();
    const TypeVar* D = tt.typevar();
    const FnType* g1 = tt.fntype({C, D});     // fn(C, D)
    const FnType* gg1 = tt.gentype({D}, g1);  // fn<D>(C, D)
    const FnType* g2 = tt.fntype({C, gg1});   // fn(C, fn<D>(C, D))
    const FnType* gg2 = tt.gentype({C}, g2);  // fn<C>(C, fn<D>(C, D))

    //gf2->dump();
    //gg2->dump();

    assert(gf2 == gg2);
    assert(gf1 != gg1);
    assert(gf1->equal(gg1));
    assert(gf1->get_representative() == gg1->get_representative());

    const TypeVar* E = tt.typevar();
    const TypeVar* F = tt.typevar();
    const FnType* h1 = tt.fntype({F, E});     // fn(F, E)
    const FnType* gh1 = tt.gentype({F}, h1);  // fn<F>(F, E)
    const FnType* h2 = tt.fntype({E, gh1});   // fn(E, fn<F>(F, E))
    const FnType* gh2 = tt.gentype({E}, h2);  // fn<E>(E, fn<F>(F, E))

    //gh2->dump();

    assert(gf2 != gh2);
    assert(!gf2->equal(gh2));
    assert(gf2->get_representative() != gh2->get_representative());

    assert(gf1 != gh1);
    assert(!gf1->equal(gh1));
    assert(gf1->get_representative() != gh1->get_representative());

    const TypeVar* G = tt.typevar();
    const TypeVar* H = tt.typevar();
    const FnType* k1 = tt.fntype({G, H});         // fn(G, H)
    const FnType* k2 = tt.fntype({G, k1});        // fn(G, fn(G, H))
    const FnType* gk2 = tt.gentype({G, H}, k2);   // fn<G,H>(G, fn(G, H))

    //gk2->dump();

    assert(gf2 != gk2);
    assert(!gf2->equal(gk2));
    assert(gf2->get_representative() != gk2->get_representative());

    assert(gf1 != k1);
    assert(!gf1->equal(k1));
    assert(gf1->get_representative() != k1->get_representative());

    tt.check_sanity();
    check_sanity({A, B, f1, gf1, f2, gf2, C, D, g1, gg1, g2, gg2, E, F, h1, gh1, h2, gh2, G, H, k1, k2, gk2});

    cout << "test_unification3 [okay]" << endl;
}

void test_type_sanity1() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const FnType* g = tt.fntype({tt.type_int()}); // fn(int)

    try {
        // illegal
        const FnType* gg = tt.gentype({A}, g);    // fn<A>(int)

        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
        assert(g->is_closed());
        assert(g->is_unified());
        assert(!A->is_unified());
        assert(A->bound_at() == nullptr);
    }

    tt.check_sanity();

    cout << "test_type_sanity1 [okay]" << endl;
}

void test_type_sanity2() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();

    try {
        tt.gentype({A}, A);         // forall A, A
        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
        assert(!A->is_closed());
        assert(!A->is_unified());
        assert(A->bound_at() == nullptr);
    }

    const TypeVar* B = tt.typevar();
    const TypeVar* C = tt.typevar();
    try {
        tt.gentype({B}, C); // forall B, C
        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
        assert(B->bound_at() == nullptr);
    }


    tt.check_sanity();

    cout << "test_type_sanity2 [okay]" << endl;
}

void test_type_sanity3() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const FnType* f = tt.fntype({A});       // fn(A)
    const FnType* gf = tt.gentype({A}, f);  // fn<A>(A)

    const FnType* g = tt.fntype({A});       // fn(A)

    assert(g->is_unified());
    assert(g != gf);
    assert(g->get_representative() != gf->get_representative());
    assert(!g->equal(gf));

    try {
        const FnType* h = tt.fntype({gf, g});     // fn(fn<A>(A), fn(A)) -> INVALID

        h->dump();

        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
    }

    tt.check_sanity();
    check_sanity({A, f, gf, g});

    cout << "test_type_sanity3 [okay]" << endl;
}

void test_type_sanity4() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const FnType* f = tt.fntype({A});         // fn(A)
    const FnType* gf = tt.gentype({A}, f);    // fn<A>(A)

    const TypeVar* B = tt.typevar();
    const FnType* g = tt.fntype({A, B});   // fn(A, B)

    try {
        // must fail because A is already bound!
        const FnType* gf2 = tt.gentype({A}, g);

        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
    }

    tt.check_sanity();

    cout << "test_type_sanity4 [okay]" << endl;
}

void test_type_sanity5() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const TypeVar* B = tt.typevar();
    const FnType* g = tt.fntype({B});   // fn(B)

    try {
        // must fail because A is not a subtype of g
        const FnType* gf2 = tt.gentype({A}, g);

        assert(false && "Previous statement should have failed!");
    } catch (IllegalTypeException& e) {
    }

    tt.check_sanity();

    cout << "test_type_sanity5 [okay]" << endl;
}

void test_type_sanity6() {
    TypeTable tt;

    const TypeVar* A = tt.typevar();
    const FnType* f = tt.fntype({A});         // fn(A)
    const FnType* g = tt.fntype({A});         // fn(A)
    const FnType* gg = tt.gentype({A}, g);    // fn<A>(A)
    // TODO f is now a type that is sane but not unified!

    /*tt.check_sanity();

    cout << "test_type_sanity6 [okay]" << endl;*/
}

int main() {
    //simple_tests();
    test_unification2();
    test_unification3();
    test_type_sanity1();
    test_type_sanity2();
    test_type_sanity3();
    test_type_sanity4();
    test_type_sanity5();
    test_type_sanity6();
}
