#include <iostream>

#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/dump.h"
#include "impala/sema/typetable.h"
#include "impala/sema/unifiable.h"

impala::Type llvm2impala(impala::TypeTable&, llvm::Type*);

int main() {
    impala::Init init("dummy");
    thorin::AutoPtr<impala::ModContents> prg = new impala::ModContents();
    prg->set_loc(thorin::Location("dummy", 1, 1, 1, 1));
    check(init, prg, false);

    auto& context = llvm::getGlobalContext();
    int num = llvm::Intrinsic::num_intrinsics - 1;
    impala::Printer printer(std::cout, false);

    printer.stream() << "extern \"device\" {";
    ++printer.indent;
    for (int i = 1; i != num; ++i) {
        auto id = (llvm::Intrinsic::ID) i;
        std::string llvm_name = llvm::Intrinsic::getName(id);
        // skip "experimental" intrinsics
        if (llvm_name.find("experimental")!=std::string::npos) continue;
        assert(llvm_name.substr(0, 5) == "llvm.");
        std::string name = llvm_name.substr(5); // remove 'llvm.' prefix
        // replace '.' with '_'
        std::transform(name.begin(), name.end(), name.begin(), [] (char c) { return c == '.' ? '_' : c; });

        if (llvm::Intrinsic::isOverloaded(id)) {
            printer.newline();
            printer.stream() << "// fn \"" << llvm_name << "\" " << name;
            printer.stream() << " (...) -> (...); // is overloaded";
        } else {
            if (auto itype = llvm2impala(*init.typetable, llvm::Intrinsic::getType(context, id))) {
                printer.newline();
                auto fn = itype.as<impala::FnType>();
                printer.stream() << "fn \"" << llvm_name << "\" " << name;
                printer.dump_list([&] (impala::Type type) { printer.stream() << type; }, fn->args().slice_to_end(fn->num_args()-1), "(", ")");
                printer.stream() << " -> ";
                if (fn->return_type()->is_noret())
                    printer.stream() << "();";
                else
                    printer.stream() << fn->return_type() << ';';
            }
        }
    }
    --printer.indent;
    printer.newline() << "}";
    printer.newline();
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
            auto t = llvm2impala(tt, fn->getParamType(i));
            valid &= t;
            if (valid) param_types[i] = t;
        }

        auto ret = fn->getReturnType()->isVoidTy() ? (impala::Type)tt.tuple_type({}) : llvm2impala(tt, fn->getReturnType());
        valid &= ret;
        if (valid) {
            param_types.back() = fn->getReturnType()->isVoidTy() ? tt.tuple_type({}) : tt.tuple_type({ret});
            return tt.fn_type(param_types);
        }
    }

    return impala::Type();
}
