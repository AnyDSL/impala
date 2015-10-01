#include <fstream>
#include <string>
#include <cassert>

#include "thorin/util/hash.h"

#include "impala/ast.h"
#include "impala/cgen.h"
#include "impala/impala.h"
#include "impala/location.h"

namespace impala {

class CGen {
private:
    // Analyses a type to see if it mentions a structure somewhere
    template <typename F>
    void struct_from_type(const Type type, const F& f) {
        // If the type mentions a vector, then we need to include the intrinsics header
        if (type.isa<SimdType>())
            needs_vectors = true;

        // Is the value a structure ?
        if (auto app_type = type.isa<StructAppType>()) {
            f(app_type->struct_abs_type()->struct_decl());
            return;
        }

        // Is the value a pointer ?
        if (auto ptr_type = type.isa<PtrType>()) {
            struct_from_type(ptr_type->referenced_type().node(), f);
            return;
        }

        // Is the value an array ?
        if (auto array_type = type.isa<ArrayType>()) {
            struct_from_type(array_type->elem_type().node(), f);
            return;
        }
    }

    static std::ostream& cgen_error(const ASTNode* node) {
        return node->loc().error() << "cannot generate C interface : ";
    }

    // Generates a C type from an Impala type
    static bool ctype_from_impala(const Type type, std::string& ctype_prefix, std::string& ctype_suffix) {
        if (auto prim_type = type.isa<PrimType>()) {
            switch (prim_type->primtype_kind()) {
                case PrimType_i8:
                    ctype_prefix = "char"; ctype_suffix = "";
                    return true;
                case PrimType_i16:
                    ctype_prefix = "short"; ctype_suffix = "";
                    return true;
                case PrimType_i32:
                    ctype_prefix = "int"; ctype_suffix = "";
                    return true;
                case PrimType_i64:
                    ctype_prefix = "long long"; ctype_suffix = "";
                    return true;
                case PrimType_u8:
                    ctype_prefix = "unsigned char"; ctype_suffix = "";
                    return true;
                case PrimType_u16:
                    ctype_prefix = "unsigned short"; ctype_suffix = "";
                    return true;
                case PrimType_u32:
                    ctype_prefix = "unsigned int"; ctype_suffix = "";
                    return true;
                case PrimType_u64:
                    ctype_prefix = "unsigned long long"; ctype_suffix = "";
                    return true;
                case PrimType_f32:
                    ctype_prefix = "float"; ctype_suffix = "";
                    return true;
                case PrimType_f64:
                    ctype_prefix = "double"; ctype_suffix = "";
                    return true;
                case PrimType_bool:
                    ctype_prefix = "int"; ctype_suffix = "";
                    return true;
            }
        }

        if (auto vector_type = type.isa<SimdType>()) {
            auto prim = vector_type->elem_type().as<PrimType>();

            ctype_suffix = "";
            switch (prim->primtype_kind()) {
                case PrimType_i32:
                    if (vector_type->size() == 4) ctype_prefix = "__m128i";
                    else if (vector_type->size() == 8) ctype_prefix = "__m258i";
                    else return false;
                    break;
                case PrimType_f32:
                    if (vector_type->size() == 4) ctype_prefix = "__m128";
                    else if (vector_type->size() == 8) ctype_prefix = "__m258";
                    else return false;
                    break;
                case PrimType_f64:
                    if (vector_type->size() == 4) ctype_prefix = "__m128d";
                    else if (vector_type->size() == 8) ctype_prefix = "__m258d";
                    else return false;
                    break;
                default:
                    return false;
            }
            return true;
        }

        // Structure types
        if (auto struct_type = type.isa<StructAppType>()) {
            const StructDecl* decl = struct_type->struct_abs_type()->struct_decl();
            ctype_prefix = "struct " + std::string(decl->item_symbol().str());
            ctype_suffix = "";
            return true;
        }

        // C void type is represented as an empty tuple (other tuples are not supported for interface generation)
        if (auto tuple_type = type.isa<TupleType>()) {
            ctype_prefix = "void";
            ctype_suffix = "";
            return true;
        }

        // Pointer types are defined recursively
        if (auto ptr_type = type.isa<PtrType>()) {
            // Rules :
            // &[T] -> T*
            // &[T * N] -> T*
            // &T -> T*

            if (auto array_type = ptr_type->referenced_type().isa<ArrayType>()) {
                if (!ctype_from_impala(array_type->elem_type(), ctype_prefix, ctype_suffix))
                    return false;
            } else {
                if (!ctype_from_impala(ptr_type->referenced_type(), ctype_prefix, ctype_suffix))
                    return false;
            }

            ctype_prefix += "*";
            ctype_suffix = "";
            return true;
        }

        if (auto darray_type = type.isa<DefiniteArrayType>()) {
            if (!ctype_from_impala(darray_type->elem_type(), ctype_prefix, ctype_suffix))
                return false;
            ctype_suffix = "[" + std::to_string(darray_type->dim()) + "]";
            return true;
        }

        if (auto iarray_type = type.isa<IndefiniteArrayType>()) {
            if (!ctype_from_impala(iarray_type->elem_type(), ctype_prefix, ctype_suffix))
                return false;
            ctype_suffix = "[]";
            return true;
        }

        return false;
    }

    enum GenState {
        NOT_GEN,
        CUR_GEN,
        GEN
    };

    // Computes the order of generation of C structures
    void compute_struct_order(thorin::HashMap<const StructDecl*, GenState>& struct_decls,
                              std::vector<const StructDecl*>& order,
                              const StructDecl* cur_gen) {
        struct_decls[cur_gen] = CUR_GEN;

        // Go through each dependency and generate it
        for (auto field : cur_gen->field_decls()) {
            struct_from_type(field->type().node(), [&] (const StructDecl* decl) {
                auto it = struct_decls.find(decl);
                if (it != struct_decls.end()) {
                    if (it->second == NOT_GEN) {
                        compute_struct_order(struct_decls, order, decl);
                    }
                }
            });
        }

        order.push_back(cur_gen);

        struct_decls[cur_gen] = GEN;
    }

    void process_struct_decl(const StructDecl* struct_decl) {
        // Add all the structures that are referenced in the fields
        for (auto field : struct_decl->field_decls()) {
            struct_from_type(field->type().node(), [this] (const StructDecl* decl) {
                export_structs.insert(decl);
            });
        }
    }

    void process_fn_decl(const FnDecl* fn_decl) {
        // If the function is not extern, skip it
        if (!fn_decl->is_extern())
            return;

        // Read each argument in turn and record the structures that have to be exported
        FnType fn_type = fn_decl->fn_type();
        for (auto arg : fn_type->args()) {
            struct_from_type(arg, [this] (const StructDecl* decl) {
                export_structs.insert(decl);
            });
        }

        export_fns.push_back(fn_decl);
    }

    thorin::HashSet<const StructDecl*> export_structs;
    std::vector<const FnDecl*> export_fns;

public:
    bool needs_vectors = false;

    void process_module(const ModContents* mod) {
        for (auto item : mod->items()) {
            if (auto decl = item->isa<FnDecl>())
                process_fn_decl(decl);
        }
    }

    void add_dependencies() {
        // Add all the structures references in the structure fields
        std::vector<const StructDecl*> exports;
        do {
            exports.resize(export_structs.size());
            std::copy(export_structs.begin(), export_structs.end(), exports.begin());

            for (auto st : exports) {
                process_struct_decl(st);
            }
        } while (exports.size() != export_structs.size());
    }

    bool generate_structs(std::ostream& o) {
        if (export_structs.empty())
            return true;

        // We have to make sure every structure is generated after each
        // of its dependencies has already been generated (otherwise the C
        // compiler will complain)
        thorin::HashMap<const StructDecl*, GenState> struct_decls;
        std::vector<const StructDecl*> order;

        for (auto st : export_structs)
            struct_decls.insert(std::make_pair(st, NOT_GEN));

        int prev_id = 0;
        do {
            compute_struct_order(struct_decls, order, struct_decls.begin()->first);

            // Remove structures that have been generated
            for (size_t i = prev_id, e = order.size(); i != e; i++)
                struct_decls.erase(order[i]);

            prev_id = order.size();
        } while (!struct_decls.empty());

        assert(order.size() == export_structs.size());

        for (auto st : order) {
            o << "struct " << st->item_symbol().str() << " {\n";
            for (auto field : st->field_decls()) {
                Type type = field->type();

                std::string ctype_pref, ctype_suf;
                if (!ctype_from_impala(type.node(), ctype_pref, ctype_suf)) {
                    cgen_error(field) << "structure field type not exportable\n";
                    return false;
                }

                o << "    " << ctype_pref << ' ' << field->symbol().str() << ctype_suf << ";\n";
            }
            o << "};\n" << std::endl;
        }

        return true;
    }

    bool generate_functions(std::ostream& o) const {
        for (auto fn : export_fns) {
            const FnType fn_type = fn->fn_type();

            std::string return_pref, return_suf;
            if (!ctype_from_impala(fn_type->return_type().node(), return_pref, return_suf)) {
                cgen_error(fn) << "function return type not exportable\n";
                return false;
            }

            // We cannot return definite or indefinite arrays from Impala
            if (return_suf != "") {
                cgen_error(fn) << "function returning an array\n";
                return false;
            }

            o << return_pref << ' ' << fn->item_symbol().str() << '(';

            // Generate all arguments except the last one which is the implicit continuation
            for (size_t i = 0, e = fn_type->num_args() - 1; i != e; ++i) {
                std::string ctype_pref, ctype_suf;
                if (!ctype_from_impala(fn_type->arg(i).node(), ctype_pref, ctype_suf)) {
                    cgen_error(fn) << "function argument type not exportable\n";
                    return false;
                }

                o << ctype_pref << ' ' << fn->param(i)->symbol().str() << ctype_suf;

                if (i < fn_type->num_args() - 2)
                    o << ", ";
            }

            // Generate void functions when the function takes no argument to be C89 compatible
            if (fn_type->num_args() == 1) {
                o << "void";
            }

            o << ");" << std::endl;
        }

        return true;
    }
};

bool generate_c_interface(const ModContents* mod, const CGenOptions& opts, std::ostream& o) {
    if (opts.fns_only && opts.structs_only)
        return false;

    // Process the ast to find which functions & structures to export
    CGen cgen;
    cgen.process_module(mod);
    cgen.add_dependencies();

    // Generate C interface
    o << "/* " << opts.file_name << " : Impala interface file generated by impala */\n"
      << "#ifndef " << opts.guard << "\n"
      << "#define " << opts.guard << "\n\n"
      << "#ifdef __cplusplus\n"
      << "extern \"C\" {\n"
      << "#endif\n"
      << std::endl;

    if (cgen.needs_vectors) {
        o << "#include <immintrin.h>\n" << std::endl;
    }

    // Export structures
    if (!opts.fns_only && !cgen.generate_structs(o)) {
        return false;
    }

    // Export functions
    if (!opts.structs_only && !cgen.generate_functions(o)) {
        return false;
    }

    o << "\n#ifdef __cplusplus\n"
      << "}\n"
      << "#endif\n\n"
      << "#endif /* " << opts.guard << " */\n" << std::endl;

    return true;
}

}
