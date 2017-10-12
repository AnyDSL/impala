#include <iostream>

#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "impala/ast.h"
#include "impala/impala.h"

// extract intrinsic names for overloaded intrinsics
static const char * const IntrinsicNameTable[] = {
  "not_intrinsic",
#define GET_INTRINSIC_NAME_TABLE
#include <llvm/IR/Intrinsics.gen>
#undef GET_INTRINSIC_NAME_TABLE
};

const impala::Type* llvm2impala(impala::TypeTable&, llvm::Type*);

int main() {
    impala::Init init("dummy");
    auto module = std::make_unique<impala::Module>("dummy.impala");
    check(init, module.get(), false);

    llvm::LLVMContext context;
    int num = llvm::Intrinsic::num_intrinsics - 1;

    std::cout << "extern \"device\" {" << thorin::up;
    for (int i = 1; i != num; ++i) {
        auto id = (llvm::Intrinsic::ID) i;
        std::string llvm_name;
        if (llvm::Intrinsic::isOverloaded(id))
            llvm_name = IntrinsicNameTable[i];
        else
            llvm_name = llvm::Intrinsic::getName(id);

        // skip "experimental" intrinsics
        if (llvm_name.find("experimental")!=std::string::npos)
            continue;
        assert(llvm_name.substr(0, 5) == "llvm.");
        std::string name = llvm_name.substr(5); // remove 'llvm.' prefix
        // replace '.' with '_'
        std::transform(name.begin(), name.end(), name.begin(), [] (char c) { return c == '.' ? '_' : c; });

        if (llvm::Intrinsic::isOverloaded(id)) {
            std::cout << thorin::endl;
            std::cout << "// fn \"" << llvm_name << "\" " << name;
            std::cout << " (...) -> (...); // is overloaded";
        } else {
            if (auto itype = llvm2impala(*init.typetable, llvm::Intrinsic::getType(context, id))) {
                std::cout << thorin::endl;
                auto fn = itype->as<impala::FnType>();
                std::cout << "fn \"" << llvm_name << "\" " << name;
                stream_list(std::cout, fn->ops().skip_back(), [&](const impala::Type* type) { std::cout << type; }, "(", ")");
                if (fn->return_type()->isa<impala::NoRetType>())
                    std::cout << " -> !;";
                else
                    std::cout << " -> " << fn->return_type() << ';';
            }
        }
    }
    std::cout << thorin::down << thorin::endl << '}' << thorin::endl;
}

const impala::Type* llvm2impala(impala::TypeTable& tt, llvm::Type* type) {
    if (auto int_type = llvm::dyn_cast<llvm::IntegerType>(type)) {
        switch (int_type->getBitWidth()) {
            case  1: return tt.type_bool();
            case  8: return tt.type_i8();
            case 16: return tt.type_i16();
            case 32: return tt.type_i32();
            case 64: return tt.type_i64();
            default: return nullptr;
        }
    }

    if (type->isHalfTy())   return tt.type_f16();
    if (type->isFloatTy())  return tt.type_f32();
    if (type->isDoubleTy()) return tt.type_f64();

    if (auto ptr = llvm::dyn_cast<llvm::PointerType>(type)) {
        auto elem = llvm2impala(tt, ptr->getElementType());
        return elem == nullptr ? nullptr : tt.borrowed_ptr_type(elem, false, ptr->getAddressSpace());
    }

    if (auto vector_type = llvm::dyn_cast<llvm::VectorType>(type)) {
        auto elem = llvm2impala(tt, vector_type->getElementType());
        return elem == nullptr ? nullptr : tt.simd_type(elem, vector_type->getNumElements());
    }

    if (auto struct_type = llvm::dyn_cast<llvm::StructType>(type)) {
        auto elements = struct_type->elements();
        impala::ASTTypeParams ast_type_params;
        impala::FieldDecls field_decls;
        auto ret = tt.struct_type(new impala::StructDecl(impala::Location(), impala::Visibility(impala::Visibility::Pub), new impala::Identifier(thorin::Location(), struct_type->isLiteral() ? "" : struct_type->getName().str().c_str()), std::move(ast_type_params), std::move(field_decls)), struct_type->getNumElements());
        for (size_t i = 0, e = struct_type->getNumElements(); i != e; ++i) {
            auto elem = llvm2impala(tt, elements[i]);
            if (elem == nullptr)
                return nullptr;
            ret->set(i, elem);
        }
        return ret;
    }

    if (auto fn = llvm::dyn_cast<llvm::FunctionType>(type)) {
        std::vector<const impala::Type*> param_types(fn->getNumParams()+1);
        bool valid = true;
        for (size_t i = 0, e = fn->getNumParams(); i != e; ++i) {
            auto t = llvm2impala(tt, fn->getParamType(i));
            valid &= bool(t);
            if (valid) param_types[i] = t;
        }

        auto ret = fn->getReturnType()->isVoidTy() ? (const impala::Type*)tt.tuple_type({}) : llvm2impala(tt, fn->getReturnType());
        valid &= bool(ret);
        if (valid) {
            param_types.back() = fn->getReturnType()->isVoidTy() ? tt.fn_type({}) : tt.fn_type({ret});
            return tt.fn_type(param_types);
        }
    }

    return nullptr;
}
