#include <sstream>

#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

class TypeSema {
public:
    TypeSema(bool nossa)
        : nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }

    // error handling

    thorin::u8 char_value(const Location& loc, const char*& p);
    bool expect_lvalue(const Expr* expr, const char* context = nullptr) {
        if (!expr->is_lvalue()) {
            error(expr) << "lvalue required " << (context ? context : "in assignment") << '\n';
            return  false;
        }
        return true;
    }
    Type scalar_type(const Expr*);
    bool is_int(Type t);
    bool is_float(Type t);
    bool expect_int(const Expr*);
    bool expect_int_or_bool(const Expr*);
    void expect_num(const Expr*);
    Type expect_type(const Expr* expr, Type found, Type expected, const char* context = nullptr);
    Type expect_type(const Expr* expr, Type expected, const char* context = nullptr) { return expect_type(expr, expr->type(), expected, context); }

    // check wrappers

    TypeVar check(const TypeParam* type_param) { type_param->check(*this); return type_param->type_var(); }
    void check(const ModContents* n) { n->check(*this); }
    Type check(const FieldDecl* n) { n->check(*this); return n->type(); }
    Type check(const LocalDecl* local) { local->check(*this); return local->type(); }
    Type check(const ASTType* ast_type) { ast_type->check(*this); return ast_type->type(); }
    void check(const Item* n) { n->check(*this); }
    Type check(const Expr* expr) { expr->check(*this); return expr->type(); }
    void check(const Stmt* n) { n->check(*this); }
    Type check_call(const MapExpr* expr, FnType fn_poly, const std::vector<Type>& type_args, ArrayRef<const Expr*> args);
    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types);

    static Type turn_cast_inside_out(const Expr* expr) {
        assert(expr->needs_cast());
        expr->type_.clear();
        expr->type_ = expr->actual_type_;
        expr->actual_type_.clear();
        return expr->type();
    }

private:
    bool nossa_;

public:
    const BlockExprBase* cur_block_ = nullptr;
    const Expr* cur_fn_ = nullptr;
};

void type_analysis(const ModContents* mod, bool nossa) {
    TypeSema sema(nossa);
    sema.check(mod);
}

//------------------------------------------------------------------------------

Type TypeSema::scalar_type(const Expr* e) {
    Type t = e->type();
    if (auto simd = t.isa<SimdType>()) {
        return simd->elem_type();
    }
    return t;
}

bool TypeSema::is_int(Type t) {
    return t->is_i8() || t->is_i16() || t->is_i32() || t->is_i64() ||
           t->is_u8() || t->is_u16() || t->is_u32() || t->is_u64();
}

bool TypeSema::is_float(Type t) {
    return t->is_f32() || t->is_f64();
}

bool TypeSema::expect_int(const Expr* expr) {
    auto t = scalar_type(expr);

    if (!t->is_error() && !is_int(t)) {
        error(expr) << "expected integer type but found " << t << "\n";
        return false;
    }
    return true;
}

bool TypeSema::expect_int_or_bool(const Expr* expr) {
    auto t = scalar_type(expr);

    if (!t->is_error() && !t->is_bool() && !is_int(t)) {
        error(expr) << "expected integer or boolean type but found " << t << "\n";
        return false;
    }
    return true;
}

void TypeSema::expect_num(const Expr* expr) {
    auto t = scalar_type(expr);

    if (!t->is_error() && !t->is_bool() && !is_int(t) && !is_float(t))
        error(expr) << "expected number type but found " << t << "\n";
}

Type TypeSema::expect_type(const Expr* expr, Type found_type, Type expected, const char* context) {
    if (found_type == expected)
        return found_type;
    if (found_type <= expected) {
        expr->actual_type_ = found_type;
        return expected;
    }

    // TODO noret
    //if (expected.noret() && (found_type == type_noret()))
        //return found_type;

    if (found_type->is_polymorphic()) { // try to infer instantiations for this polymorphic type
        std::vector<Type> type_args;
        Type inst = instantiate_unknown(found_type, type_args);
        if (inst == expected) {
            check_bounds(expr->loc(), *found_type, type_args);
            return expected;
        }
    }

    error(expr->loc()) << "mismatched types: expected '" << expected << "' but found '" << found_type << (context ? std::string("' as ") + context : "'" ) << "\n";
    return expected;
}

bool TypeSema::check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> type_args) {
    SpecializeMap map = specialize_map(unifiable, type_args);
    assert(map.size() == type_args.size());
    bool result = true;

    for (size_t i = 0, e = type_args.size(); i != e; ++i) {
        auto type_var = unifiable->type_var(i);
        Type arg = type_args[i];
        assert(map.contains(*type_var));
        assert(map.find(*type_var)->second == *arg);

        for (auto bound : type_var->bounds()) {
            // TODO do we need this copy?
            SpecializeMap bound_map(map); // copy the map per type var
            auto spec_bound = bound->specialize(bound_map);
            if (!arg->is_error() && !spec_bound->is_error()) {

                // TODO
                //check_impls(); // first we need to check all implementations to be up-to-date
                if (!arg->implements(spec_bound, bound_map)) {
                    error(loc) << "'" << arg << "' (instance for '" << type_var << "') does not implement bound '" << spec_bound << "'\n";
                    result = false;
                }
            }
        }
    }

    return result;
}

//------------------------------------------------------------------------------

/*
 * misc
 */

TypeVar TypeParam::check(TypeSema&) const { return type_var(); }

void TypeParamList::check_type_params(TypeSema& sema) const {
    for (auto type_param : type_params()) {
        /*auto type_var = */sema.check(type_param);
        for (auto bound : type_param->bounds()) {
            if (/*auto type_app = */bound->isa<ASTTypeApp>()) {
                //type_var->add_bound(type_app->trait_app(sema, type_var));
            } else {
                error(type_param) << "bounds must be trait instances, not types\n";
            }
        }
    }
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

void ErrorASTType::check(TypeSema& ) const {}
void PrimASTType::check(TypeSema&) const {}
void PtrASTType::check(TypeSema& sema) const { sema.check(referenced_ast_type()); }
void IndefiniteArrayASTType::check(TypeSema& sema) const { sema.check(elem_ast_type()); }
void   DefiniteArrayASTType::check(TypeSema& sema) const { sema.check(elem_ast_type()); }

void SimdASTType::check(TypeSema& sema) const {
    if (!sema.check(elem_ast_type()).isa<PrimType>())
        error(this) << "non primitive types forbidden in simd type\n";
}

void TupleASTType::check(TypeSema& sema) const {
    for (auto arg : args())
        sema.check(arg);
}

void FnASTType::check(TypeSema& sema) const {
    check_type_params(sema);
    for (auto arg : args())
        sema.check(arg);
}

void ASTTypeApp::check(TypeSema&) const {
    if (!decl() || !decl()->isa<TypeDecl>())
        error(identifier()) << '\'' << symbol() << "' does not name a type\n";
}

void Typeof::check(TypeSema& sema) const { sema.check(expr()); }

#if 0
TraitApp ASTTypeApp::trait_app(TypeSema& sema, Type self) const {
    if (decl()) {
        if (!decl()->isa<TraitDecl>())
            error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
}
#endif

//------------------------------------------------------------------------------

void LocalDecl::check(TypeSema& sema) const {
    if (ast_type())
        sema.check(ast_type());
}

Type Fn::check_body(TypeSema& sema) const {
    sema.check(body());

    for (auto param : params()) {
        if (param->is_mut() && !param->is_written())
            warn(param) << "parameter '" << param->symbol() << "' declared mutable but parameter is never written to\n";
    }

    return body()->type();
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        sema.check(mod_contents());
}

void ModContents::check(TypeSema& sema) const {
    for (auto item : items())
        sema.check(item);
}

void ExternBlock::check(TypeSema& sema) const {
    if (!abi().empty()) {
        if (abi() != "\"C\"" && abi() != "\"device\"" && abi() != "\"thorin\"")
            error(this) << "unknown extern specification\n";  // TODO: better location
    }

    for (auto fn : fns())
        sema.check(fn);
}

void Typedef::check(TypeSema& sema) const {
    check_type_params(sema);
    sema.check(ast_type());
}

void EnumDecl::check(TypeSema&) const { /*TODO*/ }

void StructDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    for (auto field_decl : field_decls())
        sema.check(field_decl);
}

void FieldDecl::check(TypeSema& sema) const { sema.check(ast_type()); }

void FnDecl::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, body());
    check_type_params(sema);
    for (auto param : params())
        sema.check(param);

    if (body() != nullptr)
        check_body(sema);
}

void StaticItem::check(TypeSema& sema) const {
    if (init())
        sema.check(init());
}

void TraitDecl::check(TypeSema& sema) const {
    sema.check(self_param());
    check_type_params(sema);

#if 0
    for (auto type_app : super_traits()) {
        if (!trait_abs_->add_super_trait(type_app->trait_app(sema, self_var)))
            error(type_app) << "duplicate super trait '" << type_app << "' for trait '" << symbol() << "'\n";
    }
#endif

    for (auto method : methods())
        sema.check(method);
}

void ImplItem::check(TypeSema& /*sema*/) const {
#if 0
    check_type_params(sema);
    Type for_type = sema.check(this->ast_type());

    TraitApp trait_app;
    if (trait() != nullptr) {
        if (auto type_app = trait()->isa<ASTTypeApp>()) {
            trait_app = type_app->trait_app(sema, for_type);
            auto impl = sema.impl(this, trait_app, for_type);
            for (auto type_param : type_params())
                impl->bind(sema.check(type_param));

            if (!for_type->is_error() && !trait_app->is_error()) {
                for_type.as<KnownType>()->add_impl(impl);
                trait_app->trait()->add_impl(impl);
            }
        } else
            error(trait()) << "expected trait instance\n";
    }

    thorin::HashSet<Symbol> implemented_methods;
    for (auto method : methods()) {
        sema.check(method);
        Type fn_type = method->type();

        if (trait() != nullptr) {
            assert(!trait_app.empty());

            Symbol meth_name = method->symbol();
            if (auto method_type = trait_app->find_method(meth_name)) {
                // remember name for check if all methods were implemented
                const auto& p = implemented_methods.insert(meth_name);
                assert_unused(p.second && "there should be no such name in the set"); // else name analysis failed

                // check that the types match
                if (fn_type != method_type)
                    error(method) << "method '" << trait() << "." << meth_name << "' should have type '" << method_type << "', but implementation has type '" << fn_type << "'\n";
            }
        }
    }

    // TODO: check that all methods are implemented
#endif
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

void EmptyExpr::check(TypeSema&) const {}
void LiteralExpr::check(TypeSema&) const {}

thorin::u8 TypeSema::char_value(const Location& loc, const char*& p) {
    thorin::u8 value = 0;
    if (*p++ == '\\') {
        switch (*p++) {
            case '0':  value = '\0'; break;
            case 'n':  value = '\n'; break;
            case 't':  value = '\t'; break;
            case '\'': value = '\''; break;
            case '\"': value = '\"'; break;
            case '\\': value = '\\'; break;
            default:
                error(loc) << "unknown escape sequence '\\" << *(p-1) << "'\n";
        }
    } else
        value = thorin::u8(*(p-1));

    return value;
}

void CharExpr::check(TypeSema& sema) const {
    const char* p = symbol().str();
    assert(*p == '\'');
    ++p;
    if (*p != '\'') {
        value_ = sema.char_value(loc(), p);

        if (*p++ != '\'')
            error(this) << "multi-character character constant\n";
        else
            assert(*p == '\0');
    } else
        error(this) << "empty character constant\n";
}

void StrExpr::check(TypeSema& sema) const {
    for (auto symbol : symbols()) {
        const char* p = symbol.str();
        assert(*p == '"');
        ++p;
        while (*p != '"')
            values_.push_back(sema.char_value(loc(), p));
        assert(p[1] == '\0');
    }
    values_.push_back('\0');
}

void FnExpr::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, body());
    assert(type_params().empty());

    for (size_t i = 0, e = num_params(); i != e; ++i)
        sema.check(param(i));

    assert(body() != nullptr);
    check_body(sema);
}

void PathExpr::check(TypeSema& sema) const {
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            // if local lies in an outer function go through memory to implement closure
            if (local->is_mut() && (sema.nossa() || local->fn() != sema.cur_fn_))
                local->take_address();
        }
    }
}

void PrefixExpr::check(TypeSema& sema) const {
    auto rtype = sema.check(rhs());

    switch (kind()) {
        case AND: {
            sema.expect_lvalue(rhs(), "as unary '&' operand");
            if (rhs()->needs_cast()) {
                rtype.clear();
                //rtype = TypeSema::turn_cast_inside_out(rhs()); // TODO reference?
            }

            return;
        }
        case MUL:
            // TODO
#if 0
            auto type = sema.check(rhs());
            // 'type' must be a pointer type (with any address space)
            // and must reference the expected type.
            if (auto ptr = type.isa<PtrType>()) {
                sema.expect_type(rhs(), ptr->referenced_type(), expected);
                return ptr->referenced_type();
            } else {
                auto ptr_type = sema.borrowd_ptr_type(expected);
                sema.expect_type(rhs(), type));
                return sema.type_error();
            }
#endif
            return;
        case INC:
        case DEC: {
            sema.expect_num(rhs());
            sema.expect_lvalue(rhs());
            return;
        }
        case ADD:
        case SUB: {
            sema.expect_num(rhs());
            return;
        }
        case NOT: {
            sema.expect_int(rhs()); // TODO or bool or simd type
            return;
        }
        default:
            return;
    }

    THORIN_UNREACHABLE;
}

void InfixExpr::check(TypeSema& sema) const {
    auto ltype = sema.check(lhs());
    //auto rtype = sema.check(rhs());

    switch (kind()) {
        case EQ:
        case NE:
            if (!ltype.isa<PtrType>() && !ltype.isa<PrimType>() && !ltype.isa<SimdType>())
                error(this) << "expected primitive type, pointer type or SIMD type for equality operator\n";
            return;
        case LT:
        case LE:
        case GT:
        case GE:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM: {
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return;
        }
        case OROR:
        case ANDAND:
            // TODO
#if 0
            sema.check(lhs(), sema.type_bool(), "left-hand side of logical boolean expression");
            sema.check(rhs(), sema.type_bool(), "right-hand side of logical boolean expression");
            return sema.type_bool();
#endif
        case SHL:
        case SHR: {
            sema.expect_int(lhs());
            sema.expect_int(rhs());
            return;
        }
        case OR:
        case XOR:
        case AND: {
            sema.expect_int_or_bool(lhs());
            sema.expect_int_or_bool(rhs());
            return;
        }
        case ASGN:
            sema.expect_lvalue(lhs());
            return;
        case ADD_ASGN:
        case SUB_ASGN:
        case MUL_ASGN:
        case DIV_ASGN:
        case REM_ASGN:
            if (sema.expect_lvalue(lhs())) {
                sema.expect_num(lhs());
                sema.expect_num(rhs());
            }
            return;
        case AND_ASGN:
        case  OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN:  {
            // TODO handle floats etc
            if (sema.expect_lvalue(lhs())) {
                sema.expect_int_or_bool(lhs());
                sema.expect_int_or_bool(rhs());
            }
            return;
        }
        default: THORIN_UNREACHABLE;
    }
}

void PostfixExpr::check(TypeSema& sema) const {
    // TODO check if operator supports the type
    sema.check(lhs());
    sema.expect_lvalue(lhs());
}

template <typename F, typename T>
bool symmetric(F f, T a, T b) {
    return f(a, b) || f(b, a);
}

void CastExpr::check(TypeSema& sema) const {
    auto src_type = sema.check(lhs());
    auto dst_type = sema.check(ast_type());

    auto ptr_to_ptr     = [&] (Type a, Type b) { return a.isa<PtrType>() && b.isa<PtrType>(); };
    auto int_to_int     = [&] (Type a, Type b) { return sema.is_int(a)   && sema.is_int(b);   };
    auto float_to_float = [&] (Type a, Type b) { return sema.is_float(a) && sema.is_float(b); };
    auto int_to_ptr     = [&] (Type a, Type b) { return sema.is_int(a)   && b.isa<PtrType>(); };
    auto int_to_float   = [&] (Type a, Type b) { return sema.is_int(a)   && sema.is_float(b); };
    auto int_to_bool    = [&] (Type a, Type b) { return sema.is_int(a)   && b->is_bool();     };
    auto float_to_bool  = [&] (Type a, Type b) { return sema.is_float(a) && b->is_bool();     };

    bool valid_cast =
        ptr_to_ptr(src_type, dst_type) ||
        float_to_float(src_type, dst_type) ||
        int_to_int(src_type, dst_type) ||
        symmetric(int_to_ptr, src_type, dst_type) ||
        symmetric(int_to_float, src_type, dst_type) ||
        symmetric(int_to_bool, src_type, dst_type) ||
        symmetric(float_to_bool, src_type, dst_type);

    if (!valid_cast) {
        error(this) << "invalid source and destination types for cast operator, got '"
                    << src_type << "' and '" << dst_type << "'\n";
    }
}

void DefiniteArrayExpr::check(TypeSema& /*sema*/) const {
#if 0
    for (auto arg : args())
        sema.check(arg, elem_type, "element of definite array expression");
#endif
}

void RepeatedDefiniteArrayExpr::check(TypeSema& sema) const {
    sema.check(value());
}

void IndefiniteArrayExpr::check(TypeSema& sema) const {
    sema.check(dim());
    sema.expect_int(dim());
    sema.check(elem_ast_type());
}

void TupleExpr::check(TypeSema& /*sema*/) const {
#if 0
    std::vector<Type> types;
    if (auto exp_tup = expected.isa<TupleType>()) {
        if (exp_tup->num_args() != num_args())
            error(this) << "expected tuple with " << exp_tup->num_args() << " elements, but found tuple expression with " << num_args() << " elements\n";

        size_t i = 0;
        for (auto arg : args()) {
            sema.check(arg, exp_tup->arg(i++));
            types.push_back(arg->type());
        }
    } else {
        for (auto arg : args()) {
            sema.check(arg);
            types.push_back(arg->type());
        }
    }
    return sema.tuple_type(types);
#endif
}

void SimdExpr::check(TypeSema& sema) const {
    Type elem_type;
    for (auto arg : args()) {
        auto arg_type = sema.check(arg);
        if (elem_type) {
            if (elem_type != arg_type)
                sema.expect_type(arg, arg_type, elem_type, "element of simd expression");
        } else
            elem_type = arg_type;
    }
}

void StructExpr::check(TypeSema& /*sema*/) const {
#if 0
    if (auto decl = path()->decl()) {
        StructAppType struct_app;

        if (auto typeable_decl = decl->isa<TypeableDecl>()) {
            if (auto decl_type = typeable_decl->type()) {
                if (num_ast_type_args() <= decl_type->num_type_vars()) {
                    StructAppType exp_type = expected.isa<StructAppType>();

                    // use the expected type if there is any
                    if (exp_type && (decl_type == exp_type->struct_abs_type())) {
                        for (size_t i = 0; i < exp_type->num_args(); ++i) {
                            if ((i < num_ast_type_args()) && (exp_type->arg(i) != sema.check(type_arg(i))))
                                error(type_arg(i)) << "expected different argument for type parameter '" << decl_type->type_var(i) << "': expected '" << exp_type->arg(i) << "' but found '" << type_arg(i)->type() << "'\n";
                            type_args_.push_back(exp_type->arg(i));
                        }

                        assert(type_args_.size() == decl_type->num_type_vars());
                        struct_app = exp_type;
                    } else { // if no expected type was given fill type arguments with unknowns
                        for (auto type_arg : ast_type_args())
                            type_args_.push_back(sema.check(type_arg));

                        for (size_t i = num_ast_type_args(), e = decl_type->num_type_vars(); i != e; ++i)
                            type_args_.push_back(sema.unknown_type());

                        assert(type_args_.size() == decl_type->num_type_vars());
                        auto instantiated_decl_type = decl_type->instantiate(type_args_);

                        if (instantiated_decl_type.isa<StructAppType>())
                            struct_app = instantiated_decl_type.as<StructAppType>();
                        else
                            error(path()) << '\'' << decl->symbol() << '\'' << " does not name a structure\n";
                    }
                } else
                    error(this) << "too many type arguments to structure: " << num_ast_type_args() << " for " << decl_type->num_type_vars() << "\n";

                if (struct_app) {
                    auto struct_abs  = struct_app->struct_abs_type();
                    auto struct_decl = struct_abs->struct_decl();
                    thorin::HashSet<const FieldDecl*> done;
                    for (const auto& elem : elems()) {
                        if (auto field_decl = struct_decl->field_decl(elem.symbol())) {
                            elem.field_decl_ = field_decl;
                            if (!thorin::visit(done, field_decl)) {
                                std::ostringstream oss;
                                oss << "initialization type for field '" << elem.symbol() << '\'';
                                sema.check(elem.expr(), struct_app->elem(field_decl->index()), oss.str().c_str());
                            } else
                                error(elem.expr()) << "field '" << elem.symbol() << "' specified more than once\n";
                        } else
                            error(elem.expr()) << "structure '" << struct_decl->symbol() << "' has no field named '" << elem.symbol() << "'\n";
                    }

                    if (done.size() != struct_decl->field_table().size()) {
                        for (const auto& p : struct_decl->field_table()) {
                            if (!done.contains(p.second))
                                error(this) << "missing field '" << p.first << "'\n";
                        }
                    }

                    return struct_app;
                }
            } else
                return sema.unknown_type();
        } else
            error(path()) << '\'' << decl->symbol() << '\'' << " does not name a structure\n";
    }
    return sema.type_error();
#endif
}

Type TypeSema::check_call(const MapExpr* /*expr*/, FnType /*fn_poly*/, const std::vector<Type>& /*type_args*/, ArrayRef<const Expr*> /*args*/) {
#if 0
    size_t num_ast_type_args = ast_type_args.size();
    size_t num_args = args.size();

    if (num_ast_type_args <= fn_poly->num_type_vars()) {
        for (auto type_arg : ast_type_args)
            type_args.push_back(check(type_arg));

        for (size_t i = num_ast_type_args, e = fn_poly->num_type_vars(); i != e; ++i)
            type_args.push_back(unknown_type());

        assert(type_args.size() == fn_poly->num_type_vars());
        expr->fn_mono_ = fn_poly->instantiate(type_args).as<FnType>();

        bool is_contuation = num_args == expr->fn_mono()->num_args();
        if (is_contuation || num_args+1 == expr->fn_mono()->num_args()) {
            for (size_t i = 0; i != num_args; ++i)
                check(args[i], expr->fn_mono()->arg(i), "argument type");

            // note: the order is important because of the unifying side-effects of ==
            if (is_contuation || expr->fn_mono->return_type() == expected) { // TODO this looks overly complicated
                // check if all type variables could be inferred
                bool is_known = true;
                for (size_t i = 0, e = type_args.size(); i != e; ++i) {
                    if (!type_args[i]->is_known()) {
                        is_known = false;
                        error(expr->loc()) << "could not find instance for type variable '" << fn_poly->type_var(i) << "' of function '" << expr->lhs() << "'\n";
                    }
                }

                if (is_known) {
                    check_bounds(expr->loc(), fn_poly, type_args);
                    if (is_contuation)
                        return type_noret();
                    if (!expr->fn_mono()->return_type()->is_noret())
                        return expect_type(expr, expr->fn_mono()->return_type(), expected);
                    error(expr) << "missing last argument to call continuation\n";
                }
            } else
                error(expr->loc()) << "return type '" << expr->fn_mono->return_type() << "' does not match expected type '" << expected << "'\n";
        } else {
            std::string rela = (num_args+1 < expr->fn_mono()->num_args()) ? "few" : "many";
            size_t exp_args = expr->fn_mono()->num_args() > 0 ? expr->fn_mono()->num_args()-1 : 0;
            error(expr->loc()) << "too " << rela << " arguments: " << num_args << " for " << exp_args << "\n";
        }
    } else
        error(expr->loc()) << "too many type arguments to function: " << num_ast_type_args << " for " << fn_poly->num_type_vars() << "\n";

    return type_error();
#endif
    return Type();
}

void FieldExpr::check(TypeSema& /*sema*/) const {
#if 0
    if (auto type = check_as_struct(sema, expected))
        return type;

    if (!lhs()->type()->is_error())
        error(lhs()) << "attempted access of field '" << symbol() << "' on type '" << lhs()->type() << "', but no field with that name was found\n";
    return sema.type_error();
#endif
}

Type FieldExpr::check_as_struct(TypeSema& /*sema*/) const {
#if 0
    auto ltype = sema.check(lhs());
    if (ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto struct_app = ltype.isa<StructAppType>()) {
        if (auto field_decl = struct_app->struct_abs_type()->struct_decl()->field_decl(symbol())) {
            index_ = field_decl->index();
            // a struct cannot have fields of type noret, so we can check against expected (noret defaults to false)
            sema.expect_type(this, struct_app->elem(index_), expected, "field expression type");
            return expected;
        }
    }
#endif
    return Type();
}

void MapExpr::check(TypeSema& /*sema*/) const {
#if 0
    if (auto field_expr = lhs()->isa<FieldExpr>()) {
        if (field_expr->check_as_struct(sema, sema.unknown_type()))
            return check_as_map(sema, expected);
        return check_as_method_call(sema, expected);
    }

    return check_as_map(sema, expected);
#endif
}

Type MapExpr::check_as_map(TypeSema& /*sema*/) const {
#if 0
    auto ltype = sema.check(lhs());
    if (ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto fn_poly = ltype.isa<FnType>()) {
        return sema.check_call(this, fn_poly, ast_type_args(), type_args_, args(), expected);
    } else if (auto array = ltype.isa<ArrayType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0)))
                return array->elem_type();
            else
                error(this) << "require integer as array subscript\n";
        } else
            error(this) << "too many array subscripts\n";
    } else if (auto exp_tup = ltype.isa<TupleType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0))) {
                if (auto lit = arg(0)->isa<LiteralExpr>())
                    return exp_tup->arg(lit->get_u64());
                else
                    error(this) << "require literal as tuple subscript\n";
            } else
                error(this) << "require integer as tuple subscript\n";
        } else
            error(this) << "too many tuple subscripts\n";
    } else if(auto simd = ltype.isa<SimdType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (!sema.expect_int(arg(0)))
                error(this) << "require integer as vector subscript\n";
            return simd->elem_type();
        } else
            error(this) << "too many simd vector subscripts\n";
    } else
        error(this) << "incorrect type for map expression\n";

    return sema.type_error();
#endif
    return Type();
}

Type MapExpr::check_as_method_call(TypeSema& /*sema*/) const {
#if 0
    auto field_expr = lhs()->as<FieldExpr>();
    if (auto fn_method = sema.check(field_expr->lhs())->find_method(field_expr->symbol())) {
        Array<const Expr*> nargs(num_args() + 1);
        nargs[0] = field_expr->lhs();
        std::copy(args().begin(), args().end(), nargs.begin()+1);
        return field_expr->type_ = sema.check_call(this, fn_method, ast_type_args(), type_args_, nargs, expected);
    } else
        error(this) << "no declaration for method '" << field_expr->symbol() << "' found\n";
    return sema.type_error();
#endif
    return Type();
}

void BlockExprBase::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_block_, this);
    for (auto stmt : stmts())
        sema.check(stmt);

    sema.check(expr());

    for (auto local : locals_) {
        if (local->is_mut() && !local->is_written())
            warn(local) << "variable '" << local->symbol() << "' declared mutable but variable is never written to\n";
    }
}

void RunBlockExpr::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    return BlockExprBase::check(sema);
}

void IfExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.check(then_expr());
    sema.check(else_expr());

#if 0
    // if there is an expected type, we want to pipe it down to enable type inference
    // otherwise we cannot do so because if then_type is noret, else type still can be anything
    if (expected.isa<UnknownType>()) {
        Type then_type = sema.check(then_expr(), sema.unknown_type());
        Type else_type = sema.check(else_expr(), sema.unknown_type());

        if (then_type->is_noret() && else_type->is_noret())
            return sema.type_noret();
        if (then_type->is_noret())
            return sema.expect_type(else_expr(), expected, "if expression type");
        if (else_type->is_noret())
            return sema.expect_type(then_expr(), expected, "if expression type");
        if (then_type == else_type) {
            assert(!then_expr()->needs_cast());
            assert(!else_expr()->needs_cast());
            return sema.expect_type(this, then_type, expected, "if expression type");
        }
        if (then_type <= else_type) {
            assert(!then_expr()->needs_cast());
            then_expr()->actual_type_ = then_type;
            then_expr()->type_.clear();
            then_expr()->type_ = else_type;
            return sema.expect_type(this, else_type, expected, "if expression type");
        }
        if (else_type <= then_type) {
            assert(!else_expr()->needs_cast());
            else_expr()->actual_type_ = else_type;
            else_expr()->type_.clear();
            else_expr()->type_ = then_type;
            return sema.expect_type(this, then_type, expected, "if expression type");
        }

        error(this) << "different types in arms of an if expression\n";
        error(then_expr()) << "type of the consequence is '" << then_type << "'\n";
        error(else_expr()) << "type of the alternative is '" << else_type << "'\n";
        return sema.type_error();
    } else {
        // we always allow noret in one of the branches as long
        Type then_type = sema.check(then_expr(), expected, "type of then branch");
        Type else_type = sema.check(else_expr(), expected, "type of else branch");
        return (then_type->is_noret()) ? else_type : then_type;
    }
#endif
}

void WhileExpr::check(TypeSema& sema) const {
    sema.check(cond());
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body());
}

void ForExpr::check(TypeSema& /*sema*/) const {
#if 0
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();
    if (auto map = forexpr->isa<MapExpr>()) {
        Type lhst = sema.check(map->lhs());

        if (auto fn_for = lhst.isa<FnType>()) {
            if (fn_for->num_args() != 0) {
                if (auto fn_ret = fn_for->args().back().isa<FnType>()) {
                    break_decl_->type_ = fn_ret; // inherit the type for break

                    // copy over args and check call
                    Array<const Expr*> args(map->args().size()+1);
                    *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
                    return sema.check_call(map, fn_for, map->ast_type_args(), map->type_args_, args, expected);
                }
            }
        }
    } else if (auto field_expr = forexpr->isa<FieldExpr>()) {
        assert_unused(false && field_expr && "TODO");
    }

    error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
#endif
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(TypeSema& sema) const {
    if (sema.check(expr())->is_noret())
        error(expr()) << "expression does not return; subsequent statements are unreachable\n";
    if (!expr()->has_side_effect())
        warn(expr()) << "statement with no effect\n";
}

void ItemStmt::check(TypeSema& sema) const {
    sema.check(item());
}

void LetStmt::check(TypeSema& sema) const {
    sema.cur_block_->add_local(local());
    sema.check(local());
    if (init())
        sema.check(init());
        //sema.check(init(), expected, "initialization type");
}

//------------------------------------------------------------------------------

}
