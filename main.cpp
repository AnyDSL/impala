#include <iostream>

#include "type.h"

int main() {
    TypeTable tt;

    // create some test types
    const Type* t1 = tt.tupletype2(tt.type_int(), tt.type_bool());       // tuple(int, bool)
    const Type* t2 = tt.tupletype3(tt.type_float(), tt.type_float(), t1);// tuple(float, float, tuple(int, bool))
    const Type* t3 = tt.tupletype3(tt.type_float(), tt.type_float(), t1);// tuple(float, float, tuple(int, bool))

    // dump those types
    std::cout << t1->to_string() << std::endl;
    std::cout << t2->to_string() << std::endl;
    std::cout << t3->to_string() << std::endl;

    // check for equality
    std::cout << (t1 == t2) << std::endl; // 0
    std::cout << (t1 == t3) << std::endl; // 0
    std::cout << (t2 == t3) << std::endl; // 1
}
