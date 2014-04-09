#include <iostream>

#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "impala/impala.h"
#include "impala/dump.h"
#include "impala/sema/type.h"

impala::Type llvm2impala(impala::TypeTable&, llvm::Type*);

int main() {
    impala::Init init("dummy");
    auto& context = llvm::getGlobalContext();
    int num = llvm::Intrinsic::num_intrinsics - 1;
    impala::Printer printer(std::cout, false);

    for (int i = 1; i != num; ++i) {
        auto id = (llvm::Intrinsic::ID) i;
        
        if (!llvm::Intrinsic::isOverloaded(id)) {
            auto type = llvm::Intrinsic::getType(context, id);
            std::string name = llvm::Intrinsic::getName(id);
            assert(name.substr(0, 5) == "llvm.");
            name = name.substr(5); // remove 'llvm.' prefix
            // replace '.' with '_'
            std::transform(name.begin(), name.end(), name.begin(), [] (char c) { return c == '.' ? '_' : c; });
            if (auto itype = llvm2impala(*init.typetable, type)) {
                auto fn = itype.as<impala::FnType>();
                printer.stream() << "intrinsic " << name;
                printer.dump_list([&] (impala::Type type) { printer.stream() << type->to_string(); }, fn->elems().slice_to_end(fn->size()-1), "(", ")");
                printer.stream() << " -> ";
                printer.stream() << fn->return_type()->to_string() << ';';
                printer.newline();
            }
        }
    }
}

impala::Type llvm2impala(impala::TypeTable& tt, llvm::Type* type) {
    if (auto int_type = llvm::dyn_cast<llvm::IntegerType>(type)) {
        switch (int_type->getBitWidth()) {
            case  1: return tt.type_bool();
            case  8: return tt.type_i8();
            case 16: return tt.type_i16();
            case 32: return tt.type_i32();
            case 64: return tt.type_i64();
            default: assert(false);
        }
    }

    if (type->isFloatTy())  return tt.type_f32();
    if (type->isDoubleTy()) return tt.type_f64();

    if (auto fn = llvm::dyn_cast<llvm::FunctionType>(type)) {
        std::vector<impala::Type> param_types(fn->getNumParams()+1);
        bool valid = true;
        for (size_t i = 0, e = fn->getNumParams(); i != e; ++i) {
            param_types[i] = llvm2impala(tt, fn->getParamType(i));
            valid &= param_types[i];
        }

        auto ret = llvm2impala(tt, fn->getReturnType());
        valid &= ret;
        if (valid) {
            param_types.back() = tt.fntype({ret});
            return tt.fntype(param_types);
        }
    }

    return impala::Type();
}
