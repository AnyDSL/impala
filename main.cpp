#include <iostream>

#include "typetable.h"

using namespace std;

void simple_tests() {
    TypeTable tt;

    /*
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
    std::cout << (t2 == t3) << std::endl; // 1*/

    // create an fn<A,B>(A, B)
    TypeVar A = tt.typevar();
    TypeVar B = tt.typevar();
    FnType f = tt.fntype({A, B});
    f->add_bound_var(A);
    f->add_bound_var(B);
    tt.unify(f);
    f->dump();

    // create an fn<C:Clonable+Equality, D>(C, D)
    const TypeTrait* clonable = tt.typetrait(std::string("Clonable"));
    const TypeTrait* eq = tt.typetrait(std::string("Equality"));

    TypeVar C = tt.typevar();
    TypeVar D = tt.typevar();

    TypeTraitInstance clonableInst = tt.instantiate_trait(clonable, {});
    TypeTraitInstance eqInst = tt.instantiate_trait(eq, {});

    C->add_restriction(clonableInst);
    C->add_restriction(eqInst);

    FnType g = tt.fntype({C, D});
    g->add_bound_var(C);
    g->add_bound_var(D);

    tt.unify(g);

    g->dump();

    tt.check_sanity();

    cout << "simple_tests [okay]" << endl;
}

void test_unification1() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType f = tt.fntype({A});       // fn(A)
    FnType g = tt.fntype({A});       // fn(A)
    FnType h = tt.fntype({f, g});    // fn(fn(A), fn(A))
    h->add_bound_var(A);              // fn<A>(fn(A), fn(A))
    tt.unify(h);

    //gh->dump();
    //gh.representative()->dump();
    //h->dump();
    //h.representative()->dump();

    assert(f.is_unified());
    assert(g.is_unified());
    assert(h.is_unified());

    assert(f == g);

    tt.check_sanity();
    check_sanity({A, f, g, h});

    cout << "test_unification1 [okay]" << endl;
}


void test_unification2() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType f = tt.fntype({A});     // fn(A)
    f->add_bound_var(A);            // fn<A>(A)
    tt.unify(f);

    TypeVar B = tt.typevar();
    FnType g = tt.fntype({B});     // fn(B)
    g->add_bound_var(B);            // fn<B>(B)
    tt.unify(g);

    //f->dump();
    //f.representative()->dump();
    //g->dump();
    //g.representative()->dump();

    assert(g.representative() == f.representative());
    assert(A.representative() == B.representative());

    tt.check_sanity();
    check_sanity({A, f, B, g});

    cout << "test_unification2 [okay]" << endl;
}

void test_unification3() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    TypeVar B = tt.typevar();
    FnType f1 = tt.fntype({A, B});  // fn(A, B)
    f1->add_bound_var(B);           // fn<B>(A, B)
    FnType f2 = tt.fntype({A, f1}); // fn(A, fn<B>(A, B))
    f2->add_bound_var(A);           // fn<A>(A, fn<B>(A, B))
    tt.unify(f2);

    TypeVar C = tt.typevar();
    TypeVar D = tt.typevar();
    FnType g1 = tt.fntype({C, D});  // fn(C, D)
    g1->add_bound_var(D);           // fn<D>(C, D)
    FnType g2 = tt.fntype({C, g1}); // fn(C, fn<D>(C, D))
    g2->add_bound_var(C);           // fn<C>(C, fn<D>(C, D))
    tt.unify(g2);

    //uf2->dump();
    //ug2->dump();

    assert(f1.is_unified());
    assert(g1.is_unified());
    assert(f2.is_unified());
    assert(g2.is_unified());

    assert(f2.representative() == g2.representative());

    assert(f1->equal(g1));
    assert(f1.representative() == g1.representative());

    TypeVar E = tt.typevar();
    TypeVar F = tt.typevar();
    FnType h1 = tt.fntype({F, E});     // fn(F, E)
    h1->add_bound_var(F);               // fn<F>(F, E)
    FnType h2 = tt.fntype({E, h1});    // fn(E, fn<F>(F, E))
    h2->add_bound_var(E);               // fn<E>(E, fn<F>(F, E))
    tt.unify(h2);

    //uh2->dump();

    assert(h1.is_unified());

    assert(f2.representative() != h2.representative());
    assert(!f2->equal(h2));

    assert(f1 != h1);
    assert(!f1->equal(h1));
    assert(f1.representative() != h1.representative());

    TypeVar G = tt.typevar();
    TypeVar H = tt.typevar();
    FnType k1 = tt.fntype({G, H});     // fn(G, H)
    FnType k2 = tt.fntype({G, k1});    // fn(G, fn(G, H))
    k2->add_bound_var(G);               // fn<G>(G, fn(G, H))
    k2->add_bound_var(H);               // fn<G,H>(G, fn(G, H))
    tt.unify(k2);

    //uk2->dump();

    assert(k1.is_unified());

    assert(!f2->equal(k2));
    assert(f2.representative() != k2.representative());

    assert(f1 != k1);
    assert(!f1->equal(k1));
    assert(f1.representative() != k1.representative());

    tt.check_sanity();
    check_sanity({A, B, f1, f2,
        C, D, g1, g2,
        E, F, h1, h2,
        G, H, k1, k2});

    cout << "test_unification3 [okay]" << endl;
}

/// test trait instance unification
void test_unification4() {
    TypeTable tt;

    const TypeTrait* clonable = tt.typetrait(std::string("Clonable"));
    TypeTraitInstance inst = tt.instantiate_trait(clonable, {});
    tt.unify(inst);

    assert(inst.is_unified());

    TypeTraitInstance inst2 = tt.instantiate_trait(clonable, {});
    tt.unify(inst2);

    assert(inst2.is_unified());

    assert(inst->equal(inst2));
    assert(inst.representative() == inst2.representative());

    TypeTrait* A = tt.typetrait(std::string("A"));
    TypeVar X = tt.typevar();
    A->add_bound_var(X);

    tt.check_sanity();

    cout << "test_unification4 [okay]" << endl;
}

void test_unification5() {
    TypeTable tt;

    const TypeTrait* clonable = tt.typetrait(std::string("Clonable"));

    TypeTraitInstance clonableInst = tt.instantiate_trait(clonable, {});
    TypeVar A = tt.typevar();
    A->add_restriction(clonableInst);
    FnType f = tt.fntype({A});
    f->add_bound_var(A); // fn<A:Clonable>(A)
    tt.unify(f);

    assert(f.is_unified());
    assert(clonableInst.is_unified());

    TypeTraitInstance clonableInst2 = tt.instantiate_trait(clonable, {});
    TypeVar B = tt.typevar();
    B->add_restriction(clonableInst2);
    FnType g = tt.fntype({B});
    g->add_bound_var(B); // fn<B:Clonable>(B)
    tt.unify(g);

    assert(B.is_unified());
    assert(g.is_unified());
    assert(clonableInst2.is_unified());

    assert(clonableInst->equal(clonableInst2));
    assert(clonableInst.representative() == clonableInst2.representative());

    assert(f.representative() == g.representative());

    const TypeTrait* st = tt.typetrait(std::string("SomeTrait"));
    TypeTraitInstance stInst = tt.instantiate_trait(st, {});
    TypeVar C = tt.typevar();
    C->add_restriction(stInst);
    FnType h = tt.fntype({C});
    h->add_bound_var(C); // fn<B:SomeTrait>(B)
    tt.unify(h);

    assert(h.is_unified());
    assert(stInst.is_unified());

    assert(!st->equal(clonable));
    assert(!stInst->equal(clonableInst));
    assert(!stInst->equal(clonableInst2));
    assert(stInst.representative() != clonableInst.representative());

    assert(h.representative() != g.representative());

    tt.check_sanity();
    check_sanity({A, f, B, g, C, h});

    cout << "test_unification5 [okay]" << endl;
}

/// fn<A:S<B>, B:S<A>>(A, B)
void test_unification6() {
    TypeTable tt;

    TypeVar X = tt.typevar();
    TypeTrait* S = tt.typetrait(std::string("S"));  // trait S
    S->add_bound_var(X);                            // trait S<X>

    TypeVar A = tt.typevar();
    TypeVar B = tt.typevar();

    TypeTraitInstance SA = tt.instantiate_trait(S, {A}); // S<A>
    TypeTraitInstance SB = tt.instantiate_trait(S, {B}); // S<B>

    A->add_restriction(SB);
    B->add_restriction(SA);

    /// fn<A:S<B>, B:S<A>>(A, B)
    FnType f = tt.fntype({A, B});
    f->add_bound_var(A);
    f->add_bound_var(B);

    assert(!SB->equal(SA));
    assert(!SA->equal(SB));

    tt.unify(f);

    //f->dump();

    assert(f.is_unified());
    assert(A.is_unified());
    assert(B.is_unified());
    assert(SA.is_unified());
    assert(SB.is_unified());

    assert(A.representative() != B.representative());
    assert(SA.representative() != SB.representative());


    TypeVar C = tt.typevar();
    TypeVar D = tt.typevar();

    TypeTraitInstance SC = tt.instantiate_trait(S, {C}); // S<C>
    TypeTraitInstance SD = tt.instantiate_trait(S, {D}); // S<D>

    C->add_restriction(SD);
    D->add_restriction(SC);

    /// fn<C:S<D>, D:S<C>>(C, D)
    FnType g = tt.fntype({C, D});
    g->add_bound_var(C);
    g->add_bound_var(D);

    tt.unify(g);

    assert(g.is_unified());
    assert(C.is_unified());
    assert(D.is_unified());
    assert(SC.is_unified());
    assert(SD.is_unified());

    assert(C.representative() != D.representative());
    assert(SC.representative() != SD.representative());

    assert(g.representative() == f.representative());
    assert(C.representative() == A.representative());
    assert(D.representative() == B.representative());
    assert(SC.representative() == SA.representative());
    assert(SD.representative() == SB.representative());

    tt.check_sanity();
    check_sanity({A, B, C, D, f, g});

    cout << "test_unification6 [okay]" << endl;
}

/// fn<A:S<A>>(A)
void test_unification7() {
    TypeTable tt;

    TypeVar X = tt.typevar();
    TypeTrait* S = tt.typetrait(std::string("S"));  // trait S
    S->add_bound_var(X);                            // trait S<X>

    TypeVar A = tt.typevar();

    TypeTraitInstance SA = tt.instantiate_trait(S, {A}); // S<A>
    A->add_restriction(SA);

    /// fn<A:S<A>>(A)
    FnType f = tt.fntype({A});
    f->add_bound_var(A);
    tt.unify(f);

    //f->dump();

    assert(f.is_unified());
    assert(A.is_unified());
    assert(SA.is_unified());

    TypeVar B = tt.typevar();

    TypeTraitInstance SB = tt.instantiate_trait(S, {B}); // S<C>
    B->add_restriction(SB);

    /// fn<C:S<C>>(C)
    FnType g = tt.fntype({B});
    g->add_bound_var(B);
    tt.unify(g);

    assert(g.is_unified());
    assert(B.is_unified());
    assert(SB.is_unified());

    assert(g.representative() == f.representative());
    assert(B.representative() == A.representative());
    assert(SB.representative() == SA.representative());

    tt.check_sanity();
    check_sanity({A, B, f, g});

    cout << "test_unification7 [okay]" << endl;
}

void test_trait_instatiation1() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    TypeTrait* T = tt.typetrait(std::string("T"));  // trait T
    T->add_bound_var(A);                            // trait T<A>

    tt.check_sanity();
    cout << "test_type_sanity1 [okay]" << endl;
}

void test_type_sanity1() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType g = tt.fntype({tt.type_int()}); // fn(int)

    tt.check_sanity();
    cout << "test_type_sanity1 [okay]" << endl;
}

void test_type_sanity2() {
    TypeTable tt;
    TypeVar A = tt.typevar();
    TypeVar B = tt.typevar();
    TypeVar C = tt.typevar();

    tt.check_sanity();
    cout << "test_type_sanity2 [okay]" << endl;
}

void test_type_sanity3() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType f = tt.fntype({A}); // fn(A)
    f->add_bound_var(A);        // fn<A>(A)
    tt.unify(f);

    FnType g = tt.fntype({A}); // fn(A)
    tt.unify(g);

    assert(g.is_unified());
    assert(g.representative() != f.representative());
    assert(!g->equal(f));

    tt.check_sanity();
    check_sanity({A, f, g});
    cout << "test_type_sanity3 [okay]" << endl;
}

void test_type_sanity4() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType f = tt.fntype({A}); // fn(A)
    f->add_bound_var(A);        // fn<A>(A)
    tt.unify(f);

    TypeVar B = tt.typevar();
    FnType g = tt.fntype({A, B});   // fn(A, B)

    tt.check_sanity();
    cout << "test_type_sanity4 [okay]" << endl;
}

void test_type_sanity5() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    TypeVar B = tt.typevar();
    FnType g = tt.fntype({B});   // fn(B)

    tt.check_sanity();
    cout << "test_type_sanity5 [okay]" << endl;
}

void test_type_sanity6() {
    TypeTable tt;

    TypeVar A = tt.typevar();
    FnType f = tt.fntype({A});         // fn(A)
    FnType g = tt.fntype({A});         // fn(A)
    g->add_bound_var(A);
    tt.unify(g);
    // TODO f is now a type that is sane but not unified!

    //tt.check_sanity();

    //cout << "test_type_sanity6 [okay]" << endl;
}

int main() {
    //simple_tests();
    //return 0;

    test_unification1();
    test_unification2();
    test_unification3();
    test_unification4();
    test_unification5();
    test_unification6();
    test_unification7();
    //test_type_sanity1();
    //test_type_sanity2();
    //test_type_sanity3();
    test_type_sanity4();
    //test_type_sanity5();
    //test_type_sanity6();

    return 0;
}
