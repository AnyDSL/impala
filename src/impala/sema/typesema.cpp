#include <sstream>

#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

class CheckBoundsData {
public:
    CheckBoundsData(const Location& l, Uni u, ArrayRef<Type> ts)
        : loc(l), unifiable(u), types(ts.size())
    {
        for (size_t i = 0, e = ts.size(); i != e; ++i) {
            if (auto t = ts[i])
                types[i] = t;
        }
    }

    const Location& loc;
    Uni unifiable;
    std::vector<Type> types;
};

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    void verfiy() const {
        assert(check_bounds_stash_.empty());
        TypeTable::verify();
    }

    bool nossa() const { return nossa_; }
    void push_impl(const ImplItem* i) { impls_.push_back(i); }
    void check_impls() {
        while (!impls_.empty()) {
            const ImplItem* i = impls_.back();
            impls_.pop_back();
            i->check_item(*this);
        }
    }

    thorin::u8 char_value(const Location& loc, const char*& p);

    // error handling

    bool expect_lvalue(const Expr* expr, const std::string& what = std::string()) {
        if (!expr->is_lvalue()) {
            error(expr) << "lvalue required " << (what.empty() ? "in assignment" : what.c_str()) << '\n';
            return  false;
        }
        return true;
    }
    Type scalar_type(Type);
    bool is_int(Type t);
    bool is_float(Type t);
    bool expect_int(const Expr*);
    bool expect_int_or_bool(const Expr*);
    bool expect_num(Type, const Expr*);
    void expect_num(const Expr*);
    Type expect_type(const Expr* expr, Type found, TypeExpectation expected);
    Type expect_type(const Expr* expr, TypeExpectation expected) { return expect_type(expr, expr->type(), expected); }

    Type comparison_result(const Expr* expr) {
        if (auto simd = expr->type().isa<SimdType>())
            return simd_type(type_bool(), simd->size());
        return type_bool();
    }

    TraitApp instantiate(const Location& loc, TraitAbs trait, Type self, ArrayRef<const ASTType*> args);
    Type instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args);
    Type check_call(const MapExpr* expr, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, TypeExpectation expected);

    void stash_bound_check(const Location& loc, Uni unifiable, ArrayRef<Type> types) {
        CheckBoundsData cbs(loc, unifiable, types);
        check_bounds_stash_.push_back(cbs);
    }

    void treat_bound_check_stash() {
        for (CheckBoundsData cbs : check_bounds_stash_)
            check_bounds(cbs.loc, cbs.unifiable, cbs.types);
        check_bounds_stash_.clear();
    }

    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types);

    // check wrappers

    Type check(const TypeableDecl* decl) {
        if (!decl->type_)
            decl->type_ = decl->check(*this);
        return decl->type();
    }
    Type check(const ValueDecl* decl, Type expected) {
        if (!decl->type_)
            decl->type_ = decl->check(*this, expected);
        return decl->type();
    }
    Type check(const Expr* expr, TypeExpectation expected) {
        if (!expr->type_.empty())
            return expr->type_;
        return expr->type_ = expect_type(expr, expr->check(*this, expected), expected);
    }
    Type check(const Expr* expr, Type expected, const std::string& what) { return check(expr, TypeExpectation(expected, what)); }
    Type check(const Expr* expr, Type expected) { return check(expr, expected, ""); }
    /// a check that does not expect any type (i.e. any type is allowed)
    Type check(const Expr* expr) { return check(expr, unknown_type()); }
    Type check(const ASTType* ast_type) { return ast_type->type_ = ast_type->check(*this); }

    static Type turn_cast_inside_out(const Expr* expr) {
        assert(expr->needs_cast());
        expr->type_.clear();
        expr->type_ = expr->actual_type_;
        expr->actual_type_.clear();
        return expr->type();
    }

private:
    bool nossa_;
    std::vector<const ImplItem*> impls_;
    std::vector<CheckBoundsData> check_bounds_stash_;

public:
    const BlockExprBase* cur_block_ = nullptr;
    const Fn* cur_fn_ = nullptr;
};

//------------------------------------------------------------------------------

Type TypeSema::scalar_type(Type t) {
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
    return t->is_f16() || t->is_f32() || t->is_f64();
}

bool TypeSema::expect_int(const Expr* expr) {
    auto t = scalar_type(expr->type());
    if (!t->is_error() && !is_int(t)) {
        error(expr) << "expected integer type but found " << t << "\n";
        return false;
    }
    return true;
}

bool TypeSema::expect_int_or_bool(const Expr* expr) {
    auto t = scalar_type(expr->type());
    if (!t->is_error() && !t->is_bool() && !is_int(t)) {
        error(expr) << "expected integer or boolean type but found " << t << "\n";
        return false;
    }
    return true;
}

bool TypeSema::expect_num(Type t, const Expr* expr) {
    auto scalar = scalar_type(t);
    if (!scalar->is_error() && !scalar->is_bool() && !is_int(scalar) && !is_float(scalar)) {
        error(expr) << "expected number type but found " << t << "\n";
        return false;
    }
    return true;
}

void TypeSema::expect_num(const Expr* expr) {
    expect_num(expr->type(), expr);
}

Type TypeSema::expect_type(const Expr* expr, Type found_type, TypeExpectation expected) {
    if (found_type == expected.type())
        return expected.type();
    if (found_type <= expected.type()) {
        expr->actual_type_ = found_type;
        return expected.type();
    }

    if (expected.noret() && (found_type == type_noret()))
        return found_type;

    if (found_type->is_polymorphic()) { // try to infer instantiations for this polymorphic type
        std::vector<Type> type_args;
        Type inst = instantiate_unknown(found_type, type_args);
        if (inst == expected.type()) {
            check_bounds(expr->loc(), *found_type, type_args);
            return expected.type();
        }
    }

    error(expr->loc()) << "mismatched types: expected '" << expected.type() << "' but found '" << found_type
               << (expected.what().empty() ? "'" :  "' as " + expected.what()) << "\n";
    return expected.type();
}

TraitApp TypeSema::instantiate(const Location& loc, TraitAbs trait_abs, Type self, ArrayRef<const ASTType*> args) {
    if ((args.size()+1) == trait_abs->num_type_vars()) {
        std::vector<Type> type_args;
        type_args.push_back(self);
        for (auto t : args)
            type_args.push_back(check(t));
        stash_bound_check(loc, *trait_abs, type_args);
        return trait_abs->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables of trait '" << trait_abs << "': " << args.size() << " for " << (trait_abs->num_type_vars()-1) << "\n";

    return trait_app_error();
}

Type TypeSema::instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args) {
    if (args.size() == type->num_type_vars()) {
        std::vector<Type> type_args;
        for (auto t : args)
            type_args.push_back(check(t));
        stash_bound_check(loc, *type, type_args);
        return type->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << args.size() << " for " << type->num_type_vars() << "\n";

    return type_error();
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
                check_impls(); // first we need to check all implementations to be up-to-date
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
 * AST types
 */

void TypeParamList::check_type_params(TypeSema& sema) const {
    for (auto type_param : type_params()) {
        auto type_var = type_param->type_var(sema);
        for (auto bound : type_param->bounds()) {
            if (auto type_app = bound->isa<ASTTypeApp>()) {
                type_var->add_bound(type_app->trait_app(sema, type_var));
            } else {
                error(type_param) << "bounds must be trait instances, not types\n";
            }
        }
    }
}

Type TypeParam::check(TypeSema& sema) const { return sema.type_var(symbol()); }
TypeVar TypeParam::type_var(TypeSema& sema) const { return sema.check(this).as<TypeVar>(); }
Type ErrorASTType::check(TypeSema& sema) const { return sema.type_error(); }

Type PrimASTType::check(TypeSema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.type(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::check(TypeSema& sema) const {
    auto type = sema.check(referenced_type());
    switch (kind()) {
        case Borrowed: return sema.borrowd_ptr_type(type, addr_space());
        case Mut:      return sema.mut_ptr_type(type, addr_space());
        case Owned:    return sema.owned_ptr_type(type, addr_space());
    }
    THORIN_UNREACHABLE;
}

Type IndefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type DefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.definite_array_type(sema.check(elem_type()), dim());
}

Type TupleASTType::check(TypeSema& sema) const {
    std::vector<Type> types;
    for (auto arg : args())
        types.push_back(sema.check(arg));

    return sema.tuple_type(types);
}

Type FnASTType::check(TypeSema& sema) const {
    check_type_params(sema);

    std::vector<Type> params;
    for (auto arg : args())
        params.push_back(sema.check(arg));

    FnType fn_type = sema.fn_type(params);
    for (auto type_param : type_params())
        fn_type->bind(type_param->type_var(sema));

    return fn_type;
}

Type ASTTypeApp::check(TypeSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>())
            return sema.instantiate(loc(), sema.check(type_decl), args());
    }

    error(identifier()) << '\'' << symbol() << "' does not name a type\n";
    return sema.type_error();
}

Type Typeof::check(TypeSema& sema) const {
    return sema.check(expr());
}

TraitApp ASTTypeApp::trait_app(TypeSema& sema, Type self) const {
    if (decl()) {
        if (auto trait_decl = decl()->isa<TraitDecl>()) {
            trait_decl->check_item(sema);
            return sema.instantiate(this->loc(), trait_decl->trait_abs(), self, args());
        } else
            error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
    return sema.trait_app_error();
}

Type SimdASTType::check(TypeSema& sema) const {
    auto type = sema.check(elem_type());
    if (type.isa<PrimType>())
        return sema.simd_type(type, size());
    else {
        error(this) << "non primitive types forbidden in simd type\n";
        return sema.type_error();
    }
}

Type MatrixASTType::check(TypeSema& sema) const {
    auto type = sema.check(elem_type());
    auto scalar = sema.scalar_type(type);

    if (!sema.is_int(scalar) && !sema.is_float(scalar)) {
        error(this) << "only floating point and integer types are supported for "
                    << (is_vector() ? "vectors" : "matrices") << "\n";
        return sema.type_error();
    }

    return sema.matrix_type(type, rows(), cols());
}

//------------------------------------------------------------------------------

Type ValueDecl::check(TypeSema& sema) const { return check(sema, Type()); }

Type ValueDecl::check(TypeSema& sema, Type expected) const {
    if (auto local = this->isa<LocalDecl>())
        local->fn_ = sema.cur_fn_;

    if (ast_type()) {
        Type t = sema.check(ast_type());
        if (expected.empty() || expected == t) {
            return t;
        } else {
            error(this) << "could not infer types: expected '" << expected << "' but found '" << t << "'\n";
            return sema.type_error();
        }
    } else if (expected.empty()) {
        error(this) << "could not infer parameter type for " << this << "\n";
        return sema.type_error();
    } else {
        return expected;
    }
}

void Fn::check_body(TypeSema& sema, FnType fn_type) const {
    auto return_type = fn_type->return_type();
    sema.check(body(), TypeExpectation(return_type, true, "return type"));

    for (auto param : params()) {
        if (param->is_mut() && !param->is_written())
            warn(param) << "parameter '" << param->symbol() << "' declared mutable but parameter is never written to\n";
    }
}

//------------------------------------------------------------------------------

/*
 * items
 */

void TypeDeclItem::check_item(TypeSema& sema) const { sema.check(static_cast<const TypeDecl*>(this)); }

Type ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
    return Type();
}

void ModContents::check(TypeSema& sema) const {
    std::vector<const Item*> non_impls;
    for (auto item : items()) {
        if (auto impl = item->isa<const ImplItem>()) {
            sema.push_impl(impl);
        } else
            non_impls.push_back(item);
    }

    sema.check_impls();
    for (auto item : non_impls)
        item->check_item(sema);
}

void ExternBlock::check_item(TypeSema& sema) const {
    if (!abi().empty())
        if (abi() != Symbol("\"C\"") && abi() != Symbol("\"device\"") && abi() != Symbol("\"thorin\""))
            error(this) << "unknown extern specification\n";  // TODO: better location
    for (auto fn : fns())
        sema.check(fn);
}

Type Typedef::check(TypeSema& sema) const {
    check_type_params(sema);
    Type type = sema.check(ast_type());
    if (type_params().size() > 0) {
        Type abs = sema.typedef_abs(type);
        for (auto type_param : type_params())
            abs->bind(type_param->type_var(sema));
        return abs;
    } else {
        return type;
    }
}

Type EnumDecl::check(TypeSema&) const { return Type(); }

Type StructDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    auto struct_type = sema.struct_abs_type(this);
    type_ = struct_type;    // break cycle to allow recursive types
    for (auto field : field_decls())
        struct_type->set(field->index(), sema.check(field));
    for (auto type_param : type_params())
        struct_type->bind(type_param->type_var(sema));

    sema.treat_bound_check_stash();

    type_.clear();          // will be set again by TypeSema's wrapper
    return struct_type;
}

Type FieldDecl::check(TypeSema& sema) const {
    return sema.check(ast_type());
}

void FnDecl::check_item(TypeSema& sema) const {
    sema.check(static_cast<const ValueDecl*>(this));
}

Type FnDecl::check(TypeSema& sema) const {
    THORIN_PUSH(sema.cur_fn_, this);
    check_type_params(sema);
    std::vector<Type> types;
    for (auto param : params())
        types.push_back(sema.check(param));

    // create FnType
    FnType fn_type = sema.fn_type(types);
    for (auto tp : type_params())
        fn_type->bind(tp->type_var(sema));
    type_ = fn_type;

    sema.treat_bound_check_stash();

    if (body() != nullptr)
        check_body(sema, fn_type);

    type_.clear(); // will be set again by TypeSema's wrapper
    return fn_type;
}

void StaticItem::check_item(TypeSema& sema) const {
    Type init_type;
    if (init())
        init_type = sema.check(init());
    sema.check(static_cast<const ValueDecl*>(this), init_type);
}

Type StaticItem::check(TypeSema& sema) const {
    Type init_type;
    if (init())
        init_type = sema.check(init());

    Type ret_type;
    if (init_type)
        ret_type = sema.check(static_cast<const ValueDecl*>(this), init_type);
    else
        ret_type = sema.check(static_cast<const ValueDecl*>(this), sema.unknown_type());

    type_.clear(); // will be set again by TypeSema's wrapper
    return ret_type;
}

void TraitDecl::check_item(TypeSema& sema) const {
    // did we already check this trait?
    if (!trait_abs().empty())
        return;

    TypeVar self_var = self_param()->type_var(sema);
    trait_abs_ = sema.trait_abs(this);
    trait_abs_->bind(self_var);

    check_type_params(sema);
    for (auto tp : type_params())
        trait_abs_->bind(tp->type_var(sema));

    for (auto type_app : super_traits()) {
        if (!trait_abs_->add_super_trait(type_app->trait_app(sema, self_var)))
            error(type_app) << "duplicate super trait '" << type_app << "' for trait '" << symbol() << "'\n";
    }

    sema.treat_bound_check_stash();

    for (auto m : methods())
        sema.check(m);
}

void ImplItem::check_item(TypeSema& sema) const {
    check_type_params(sema);
    Type for_type = sema.check(this->ast_type());

    TraitApp trait_app;
    if (trait() != nullptr) {
        if (auto type_app = trait()->isa<ASTTypeApp>()) {
            trait_app = type_app->trait_app(sema, for_type);
            auto impl = sema.impl(this, trait_app, for_type);
            for (auto tp : type_params())
                impl->bind(tp->type_var(sema));

            if (!for_type->is_error() && !trait_app->is_error()) {
                for_type.as<KnownType>()->add_impl(impl);
                trait_app->trait()->add_impl(impl);
            }
        } else
            error(trait()) << "expected trait instance\n";
    }

    sema.treat_bound_check_stash();

    thorin::HashSet<Symbol> implemented_methods;
    for (auto fn : methods()) {
        Type fn_type = sema.check(fn);

        if (trait() != nullptr) {
            assert(!trait_app.empty());

            Symbol meth_name = fn->symbol();
            if (auto method_type = trait_app->find_method(meth_name)) {
                // remember name for check if all methods were implemented
                const auto& p = implemented_methods.insert(meth_name);
                assert_unused(p.second && "there should be no such name in the set"); // else name analysis failed

                // check that the types match
                if (fn_type != method_type)
                    error(fn) << "method '" << trait() << "." << meth_name << "' should have type '" << method_type << "', but implementation has type '" << fn_type << "'\n";
            }
        }
    }

    // TODO
#if 0
    // check that all methods are implemented
    if (!bound.empty()) {
        if (implemented_methods.size() != bound->num_methods()) {
            assert(implemented_methods.size() < bound->num_methods());
            for (const auto& p : bound->all_methods()) {
                if (!implemented_methods.contains(p.first))
                    error(this) << "must implement method '" << p.first << "'\n";
            }
        }
    }
#endif
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

Type EmptyExpr::check(TypeSema& sema, TypeExpectation) const { return sema.unit(); }

Type LiteralExpr::check(TypeSema& sema, TypeExpectation) const {
    // FEATURE we could enhance this using the expected type (e.g. 4 could be interpreted as int8 if needed)
    return sema.type(literal2type());
}

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

Type CharExpr::check(TypeSema& sema, TypeExpectation) const {
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

    return sema.type_u8();
}

Type StrExpr::check(TypeSema& sema, TypeExpectation expected) const {
    for (auto symbol : symbols()) {
        const char* p = symbol.str();
        assert(*p == '"');
        ++p;
        while (*p != '"')
            values_.push_back(sema.char_value(loc(), p));
        assert(p[1] == '\0');
    }
    values_.push_back('\0');

    auto result = sema.definite_array_type(sema.type_u8(), values_.size());
    if (auto ptr = expected.type().isa<BorrowedPtrType>()) {
        if (auto array = ptr->referenced_type().isa<ArrayType>()) {
            if (array->elem_type()->is_u8()) {
                is_used_as_global_ = true;
                return sema.borrowd_ptr_type(result);
            }
        }
    }
    return result;
}

Type FnExpr::check(TypeSema& sema, TypeExpectation expected) const {
    THORIN_PUSH(sema.cur_fn_, this);
    assert(type_params().empty());

    FnType fn_type;
    if (FnType exp_fn = expected.type().isa<FnType>()) {
        if (!is_continuation() && exp_fn->num_args() == num_params()+1) { // add return param to infer type
            const Location& loc = body()->loc().begin();
            const_cast<FnExpr*>(this)->params_.push_back(Param::create(ret_var_handle_, new Identifier("return", loc), loc, nullptr));
        } else if (exp_fn->num_args() != num_params())
            error(this) << "expected function with " << exp_fn->num_args() << " parameters, but found lambda expression with " << num_params() << " parameters\n";

        for (size_t i = 0; i < num_params() && i < exp_fn->num_args(); ++i)
            sema.check(param(i), exp_fn->arg(i));

        fn_type = exp_fn;
    } else {
        std::vector<Type> par_types;
        for (auto param : params())
            par_types.push_back(sema.check(param));

        fn_type = sema.fn_type(par_types);
    }

    assert(body() != nullptr);
    check_body(sema, fn_type);

    return fn_type;
}

Type PathExpr::check(TypeSema& sema, TypeExpectation) const {
    // FEATURE consider longer paths
    //auto* last = path()->path_args().back();
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            // if local lies in an outer function go through memory to implement closure
            if (local->is_mut() && (sema.nossa() || local->fn() != sema.cur_fn_))
                local->take_address();
        }
        return sema.check(value_decl());
    }
    return sema.type_error();
}

static int take_addr_space(TypeSema& sema, const PrefixExpr* prefix) {
    if (prefix->kind() == PrefixExpr::MUL) {
        auto type = sema.check(prefix->rhs());
        if (auto ptr = type.isa<PtrType>()) {
            return ptr->addr_space();
        }
    }
    return 0;
}

Type PrefixExpr::check(TypeSema& sema, TypeExpectation expected) const {
    switch (kind()) {
        case AND: {
            Type rtype;
            if (auto ptr = expected.type().isa<PtrType>()) {
                rtype = sema.check(rhs(), ptr->referenced_type());
            } else
                rtype = sema.check(rhs());
            sema.expect_lvalue(rhs(), "as unary '&' operand");
            rhs()->take_address();
            if (rhs()->needs_cast()) {
                rtype.clear();
                rtype = TypeSema::turn_cast_inside_out(rhs());
            }

            // Keep the address space of the original pointer, if possible
            int addr_space = 0;
            if (auto map = rhs()->isa<MapExpr>()) {
                if (auto prefix = map->lhs()->isa<PrefixExpr>())
                    addr_space = take_addr_space(sema, prefix);
            } else if (auto field = rhs()->isa<FieldExpr>()) {
                if (auto prefix = field->lhs()->isa<PrefixExpr>())
                    addr_space = take_addr_space(sema, prefix);
            } else if (auto prefix = rhs()->isa<PrefixExpr>()) {
                addr_space = take_addr_space(sema, prefix);
            }

            return sema.borrowd_ptr_type(rtype, addr_space);
        }
        case TILDE:
            if (auto pty = expected.type().isa<PtrType>()) {
                return sema.owned_ptr_type(sema.check(rhs(), pty->referenced_type()));
            } else {
                return sema.owned_ptr_type(sema.check(rhs()));
            }
        case MUL: {
            auto type = sema.check(rhs());
            // 'type' must be a pointer type (with any address space)
            // and must reference the expected type.
            if (auto ptr = type.isa<PtrType>()) {
                sema.expect_type(rhs(), ptr->referenced_type(), expected.type());
                return ptr->referenced_type();
            } else {
                auto ptr_type = sema.borrowd_ptr_type(expected.type());
                sema.expect_type(rhs(), type, TypeExpectation(ptr_type));
                return sema.type_error();
            }
        }
        case INC:
        case DEC: {
            auto rtype = sema.check(rhs(), expected);
            sema.expect_num(rhs());
            sema.expect_lvalue(rhs());
            return rtype;
        }
        case ADD:
        case SUB: {
            auto rtype = sema.check(rhs(), expected);
            sema.expect_num(rhs());
            return rtype;
        }
        case NOT: {
            auto rtype = sema.check(rhs(), expected);
            if (auto simd = rtype.isa<SimdType>()) {
                if (simd->elem_type()->is_bool())
                    return rtype;
            }
            if (rtype->is_bool() || sema.expect_int(rhs()))
                return rtype;
            return sema.type_error();
        }
        case RUN:
        case HLT:
            return sema.check(rhs()); // TODO can we propagate expected here?
        default:
            THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

Type InfixExpr::check(TypeSema& sema, TypeExpectation expected) const {
    switch (kind()) {
        case EQ:
        case NE:
            sema.check(rhs(), sema.check(lhs()));
            if (!lhs()->type().isa<PtrType>() && !lhs()->type().isa<PrimType>() && !lhs()->type().isa<SimdType>()) {
                error(this) << "expected primitive type, pointer type or SIMD type for equality operator\n";
                return sema.type_error();
            }
            return sema.comparison_result(lhs());
        case LT:
        case LE:
        case GT:
        case GE:
            sema.check(rhs(), sema.check(lhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return sema.comparison_result(lhs());
        case OROR:
        case ANDAND:
            sema.check(lhs(), sema.type_bool(), "left-hand side of logical boolean expression");
            sema.check(rhs(), sema.type_bool(), "right-hand side of logical boolean expression");
            return sema.type_bool();
        case ADD:
        case SUB:
        case DIV:
        case MUL:
        case REM: {
            return check_arith_op(sema);
        }
        case SHL:
        case SHR: {
            auto type = sema.check(lhs(), sema.check(rhs(), expected));
            sema.expect_int(lhs());
            sema.expect_int(rhs());
            return type;
        }
        case OR:
        case XOR:
        case AND: {
            auto type = sema.check(lhs(), sema.check(rhs(), expected));
            sema.expect_int_or_bool(lhs());
            sema.expect_int_or_bool(rhs());
            return type;
        }
        case ASGN:
            sema.check(rhs(), sema.check(lhs()));
            if (sema.expect_lvalue(lhs()))
                return sema.unit();
            break;
        case ADD_ASGN:
        case SUB_ASGN:
        case MUL_ASGN:
        case DIV_ASGN:
        case REM_ASGN: {
            check_arith_op(sema);
            if (sema.expect_lvalue(lhs()))
                return sema.unit();
            break;
        }
        case AND_ASGN:
        case  OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN:  {
            // TODO handle floats etc
            sema.check(rhs(), sema.check(lhs()));
            if (sema.expect_lvalue(lhs())) {
                sema.expect_int_or_bool(lhs());
                sema.expect_int_or_bool(rhs());
                return sema.unit();
            }
            break;
        }
        default: THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

inline Type matrix_elem_type(Type t) {
    if (auto mat = t.isa<MatrixType>())
        return mat->elem_type();
    return t;
}

Type InfixExpr::check_arith_op(TypeSema& sema) const {
    auto ltype = sema.check(lhs());
    auto rtype = sema.check(rhs());

    if (ltype->is_error() || rtype->is_error()) return sema.type_error();

    auto lelem = matrix_elem_type(ltype);
    auto relem = matrix_elem_type(rtype);
    bool lscalar = ltype == lelem;
    bool rscalar = rtype == relem;

    lvec_ = lscalar ? SCALAR : (ltype.as<MatrixType>()->is_vector() ? VECTOR : MATRIX);
    rvec_ = rscalar ? SCALAR : (ltype.as<MatrixType>()->is_vector() ? VECTOR : MATRIX);

    if (kind() == REM) {
        // For REM, the types must be integers
        if (!sema.is_int(sema.scalar_type(lelem)) || !sema.is_int(sema.scalar_type(relem))) {
            error(this) << "the modulus '%' operator is only valid on integers\n";
            return sema.type_error();
        }
    } else {
        sema.expect_num(lelem, lhs());
        sema.expect_num(relem, rhs());
    }

    // if operands are both scalars or both vectors, types must be equal
    if ((lscalar & rscalar) && ltype == rtype) return ltype;
    if ((lscalar ^ rscalar) && lelem == relem) { return lscalar ? rtype : ltype; }

    if (!lscalar && !rscalar && lelem == relem) {
        auto lmat = ltype.as<MatrixType>();
        auto rmat = rtype.as<MatrixType>();

        if (kind() == MUL) {
            // vector * matrix, matrix * vector, or matrix * matrix multiplication            
            if (!lmat->is_vector()) {
                if (lmat->cols() == rmat->rows()) return rmat;
            } else {
                if (lmat->rows() == rmat->rows()) return lmat;
            }
        } else if (lmat->rows() == rmat->rows()) {
            // addition, subtraction, division, ...
            return lmat;
        }
    }            

    if (!ltype->is_error() && !rtype->is_error()) {
        error(this) << "types do not match for operator '" << Token(loc(), (Token::Kind)kind())
                    << "', got " << ltype << " and " << rtype << " \n";
    }
    return sema.type_error();
}

Type PostfixExpr::check(TypeSema& sema, TypeExpectation expected) const {
    // TODO check if operator supports the type
    sema.check(lhs(), expected);
    sema.expect_lvalue(lhs());
    return lhs()->type();
}

template <typename F, typename T>
bool symmetric(F f, T a, T b) {
    return f(a, b) || f(b, a);
}

Type CastExpr::check(TypeSema& sema, TypeExpectation) const {
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

    return dst_type;
}

Type DefiniteArrayExpr::check(TypeSema& sema, TypeExpectation) const {
    Type elem_type = sema.unknown_type();
    for (auto arg : args())
        sema.check(arg, TypeExpectation(elem_type, "element of definite array expression"));
    return sema.definite_array_type(elem_type, num_args());
}

Type RepeatedDefiniteArrayExpr::check(TypeSema& sema, TypeExpectation) const {
    return sema.definite_array_type(sema.check(value()), count());
}

Type IndefiniteArrayExpr::check(TypeSema& sema, TypeExpectation) const {
    sema.check(dim());
    sema.expect_int(dim());
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type TupleExpr::check(TypeSema& sema, TypeExpectation expected) const {
    std::vector<Type> types;
    if (auto exp_tup = expected.type().isa<TupleType>()) {
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
}

Type SimdExpr::check(TypeSema& sema, TypeExpectation) const {
    Type elem_type = sema.unknown_type();
    for (auto arg : args())
        sema.check(arg, TypeExpectation(elem_type, "element of simd expression"));
    return sema.simd_type(elem_type, num_args());
}

Type StructExpr::check(TypeSema& sema, TypeExpectation expected) const {
    if (auto decl = path()->decl()) {
        StructAppType struct_app;

        if (auto td = decl->isa<TypeableDecl>()) {
            Type decl_type = sema.check(td);

            if (num_type_args() <= decl_type->num_type_vars()) {
                StructAppType exp_type = expected.type().isa<StructAppType>();

                // use the expected type if there is any
                if (exp_type && (decl_type == exp_type->struct_abs_type())) {
                    for (size_t i = 0; i < exp_type->num_args(); ++i) {
                        if ((i < num_type_args()) && (exp_type->arg(i) != sema.check(type_arg(i))))
                            error(type_arg(i)) << "expected different argument for type parameter '" << decl_type->type_var(i) << "': expected '" << exp_type->arg(i) << "' but found '" << type_arg(i)->type() << "'\n";
                        inferred_args_.push_back(exp_type->arg(i));
                    }

                    assert(inferred_args_.size() == decl_type->num_type_vars());
                    struct_app = exp_type;
                } else { // if no expected type was given fill type arguments with unknowns
                    for (auto type_arg : type_args())
                        inferred_args_.push_back(sema.check(type_arg));

                    for (size_t i = num_type_args(), e = decl_type->num_type_vars(); i != e; ++i)
                        inferred_args_.push_back(sema.unknown_type());

                    assert(inferred_args_.size() == decl_type->num_type_vars());
                    auto instantiated_decl_type = decl_type->instantiate(inferred_args_);

                    if (instantiated_decl_type.isa<StructAppType>())
                        struct_app = instantiated_decl_type.as<StructAppType>();
                    else
                        error(path()) << '\'' << decl->symbol() << '\'' << " does not name a structure\n";
                }
            } else
                error(this) << "too many type arguments to structure: " << num_type_args() << " for " << decl_type->num_type_vars() << "\n";

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
                            sema.check(elem.expr(), TypeExpectation(struct_app->elem(field_decl->index()), oss.str()));
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
            error(path()) << '\'' << decl->symbol() << '\'' << " does not name a structure\n";
    }
    return sema.type_error();
}

Type TypeSema::check_call(const MapExpr* expr, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, TypeExpectation expected) {
    size_t num_type_args = type_args.size();
    size_t num_args = args.size();

    if (num_type_args <= fn_poly->num_type_vars()) {
        for (auto type_arg : type_args)
            inferred_args.push_back(check(type_arg));

        for (size_t i = num_type_args, e = fn_poly->num_type_vars(); i != e; ++i)
            inferred_args.push_back(unknown_type());

        assert(inferred_args.size() == fn_poly->num_type_vars());
        expr->fn_mono_ = fn_poly->instantiate(inferred_args).as<FnType>();

        bool is_contuation = num_args == expr->fn_mono()->num_args();
        if (is_contuation || num_args+1 == expr->fn_mono()->num_args()) {
            for (size_t i = 0; i != num_args; ++i)
                check(args[i], expr->fn_mono()->arg(i), "argument type");

            // note: the order is important because of the unifying side-effects of ==
            if (is_contuation || expr->fn_mono()->return_type() == expected.type()) { // TODO this looks overly complicated
                // check if all type variables could be inferred
                bool is_known = true;
                for (size_t i = 0, e = inferred_args.size(); i != e; ++i) {
                    if (!inferred_args[i]->is_known()) {
                        is_known = false;
                        error(expr->loc()) << "could not find instance for type variable '" << fn_poly->type_var(i) << "' of function '" << expr->lhs() << "'\n";
                    }
                }

                if (is_known) {
                    check_bounds(expr->loc(), fn_poly, inferred_args);
                    if (is_contuation)
                        return type_noret();
                    if (!expr->fn_mono()->return_type()->is_noret())
                        return expect_type(expr, expr->fn_mono()->return_type(), expected);
                    error(expr) << "missing last argument to call continuation\n";
                }
            } else
                error(expr->loc()) << "return type '" << expr->fn_mono()->return_type() << "' does not match expected type '" << expected.type() << "'\n";
        } else {
            std::string rela = (num_args+1 < expr->fn_mono()->num_args()) ? "few" : "many";
            size_t exp_args = expr->fn_mono()->num_args() > 0 ? expr->fn_mono()->num_args()-1 : 0;
            error(expr->loc()) << "too " << rela << " arguments: " << num_args << " for " << exp_args << "\n";
        }
    } else
        error(expr->loc()) << "too many type arguments to function: " << num_type_args << " for " << fn_poly->num_type_vars() << "\n";

    return type_error();
}

Type FieldExpr::check(TypeSema& sema, TypeExpectation expected) const {
    auto ltype = sema.check(lhs());
    if (ltype.isa<MatrixType>())
        return check_as_matrix(sema, expected.type());

    if (auto type = check_as_struct(sema, expected.type()))
        return type;

    if (!lhs()->type()->is_error())
        error(lhs()) << "attempted access of field '" << symbol() << "' on type '" << lhs()->type() << "', but no field with that name was found\n";
    return sema.type_error();
}

Type FieldExpr::check_as_struct(TypeSema& sema, Type expected) const {
    auto ltype = sema.check(lhs());
    if (ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto mat = ltype.isa<MatrixType>())
        return check_as_matrix(sema, expected);

    if (auto struct_app = ltype.isa<StructAppType>()) {
        if (auto field_decl = struct_app->struct_abs_type()->struct_decl()->field_decl(symbol())) {
            index_ = field_decl->index();
            // a struct cannot have fields of type noret, so we can check against expected.type() (noret defaults to false)
            sema.expect_type(this, struct_app->elem(index_), TypeExpectation(expected, "field expression type"));
            return expected;
        }
    }

    return Type();
}

Type FieldExpr::check_as_matrix(TypeSema& sema, Type expected) const {
    auto ltype = sema.check(lhs()).as<MatrixType>();
    if (ltype->is_vector()) {
        const char* str = symbol().str();
        uint32_t len_dst = strlen(str);
        if (len_dst > 4) {
            error(this) << "too many components in swizzle operation\n";
            return sema.type_error();
        }

        uint32_t len_src = 0;
        for (auto* p = str; *p; p++) {
            uint32_t l;
            switch (*p) {
                case 'x': l = 0; break;
                case 'y': l = 1; break;
                case 'z': l = 2; break;
                case 'w': l = 3; break;
                default:
                    error(this) << "incorrect character in swizzle operation, only 'x', 'y', 'z', and 'w' are allowed\n";
                    return sema.type_error();
            }
            swizzle_.push_back(l);
            len_src = std::max(len_src, l);
        }
        
        if (len_src >= ltype->rows()) {
            const char xyzw[] = "xyzw";
            error(this) << "vector component '" << xyzw[len_src] << "' is out of bounds\n";
            return sema.type_error();
        }

        if (len_dst == 1) index_ = len_src;

        return len_dst > 1 ? static_cast<Type>(sema.matrix_type(ltype->elem_type(), len_dst)) : ltype->elem_type();
    }

    error(this) << "matrix components cannot be accessed by field names, use the array syntax instead\n";
    return sema.type_error();
}

Type MapExpr::check(TypeSema& sema, TypeExpectation expected) const {
    if (auto field_expr = lhs()->isa<FieldExpr>()) {
        if (field_expr->check_as_struct(sema, sema.unknown_type()))
            return check_as_map(sema, expected);
        return check_as_method_call(sema, expected);
    }

    return check_as_map(sema, expected);
}

Type MapExpr::check_as_map(TypeSema& sema, TypeExpectation expected) const {
    auto ltype = sema.check(lhs());
    if (ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto fn_poly = ltype.isa<FnType>()) {
        return sema.check_call(this, fn_poly, type_args(), inferred_args_, args(), expected);
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
}

Type MapExpr::check_as_method_call(TypeSema& sema, TypeExpectation expected) const {
    auto field_expr = lhs()->as<FieldExpr>();
    sema.check_impls();
    if (auto fn_method = sema.check(field_expr->lhs())->find_method(field_expr->symbol())) {
        Array<const Expr*> nargs(num_args() + 1);
        nargs[0] = field_expr->lhs();
        std::copy(args().begin(), args().end(), nargs.begin()+1);
        return field_expr->type_ = sema.check_call(this, fn_method, type_args(), inferred_args_, nargs, expected);
    } else
        error(this) << "no declaration for method '" << field_expr->symbol() << "' found\n";
    return sema.type_error();
}

Type MatrixExpr::check(TypeSema& sema, TypeExpectation expected) const {
    if (!num_args())
        error(this) << "arguments expected\n";

    for (auto arg : args()) {
        // prevent error message explosion
        if (sema.check(arg)->is_error()) return sema.type_error();
    }

    uint32_t rows, cols;
    switch (kind()) {
#define IMPALA_MAT_KEY(tok, str, r, c) case Token:: tok : rows = r; cols = c; break;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }

    if (rows > 0) {
        if (cols > 1) { return check_matrix_args(sema, rows, cols); }
        else          { return check_vector_args(sema, rows);       }
    } else {
        // special functions (e.g. dot, cross, ...) have rows = cols = 0
        switch (kind()) {
            case Token::MAT_INVERSE:     return check_inverse(sema);
            case Token::MAT_DETERMINANT: return check_determinant(sema);
            case Token::VEC_DOT:         return check_dot(sema);
            case Token::VEC_CROSS:       return check_cross(sema);
            default: break;
        }
    }

    THORIN_UNREACHABLE;
}

Type MatrixExpr::check_vector_args(TypeSema& sema, uint32_t rows) const {
    // vectors can be constructed by concatenating scalars with smaller vectors
    auto type = arg(0)->type();

    Type elem_type;
    if (auto mat = type.isa<MatrixType>())
        elem_type = mat->elem_type();
    else
        elem_type = type;

    auto scalar = sema.scalar_type(elem_type);
    if (!sema.is_int(scalar) && !sema.is_float(scalar)) {
        error(this) << "only floating point and integer types are supported for vectors\n";
        return sema.type_error();
    }

    uint32_t count = 0;
    for (auto arg : args()) {
        if (arg->type().isa<PrimType>() || arg->type().isa<SimdType>()) {
            if (arg->type() != elem_type) {
                error(this) << "mismatching types in vector initializers\n";
                return sema.type_error();
            }
            count += 1;
        } else if (auto mat = arg->type().isa<MatrixType>()) {
            if (mat->elem_type() != elem_type) {
                error(this) << "mismatching types in vector initializers: got "
                            << mat->elem_type() << ", expected " << elem_type << "\n";
                return sema.type_error();            
            }

            if (!mat->is_vector()) {
                error(this) << "matrices are not allowed in vector initializers\n";
                return sema.type_error();
            }
            count += mat->rows();
        } else {
            error(this) << "incorrect type for vector initializer\n";
            return sema.type_error();
        }
    }

    if (count != rows && count > 1) {
        error(this) << "incorrect number of initializers for vector: got "
                    << count << ", expected " << rows << "\n";
        return sema.type_error();
    }

    return sema.matrix_type(elem_type, rows);
}

Type MatrixExpr::check_matrix_args(TypeSema& sema, uint32_t rows, uint32_t cols) const {
    // matrices can be constructed by providing the columns as vectors, or by listing all the components one by one.
    auto type = arg(0)->type();
    for (auto arg : args()) {
        if (arg->type() != type) {
            error(this) << "mismatching types in matrix initializers\n";
            return sema.type_error();
        }
    }

    if (auto mat = type.isa<MatrixType>()) {
        if (!mat->is_vector()) {
            error(this) << "matrices cannot be initialized with matrices\n";
            return sema.type_error();
        }

        if (num_args() != cols || mat->rows() != rows) {
            error(this) << "matrix initializer sizes do not match matrix dimensions\n";
            return sema.type_error();
        }

        return sema.matrix_type(mat->elem_type(), rows, cols);
    }

    auto scalar = sema.scalar_type(type);
    if (!sema.is_int(scalar) && !sema.is_float(scalar)) {
        error(this) << "only floating point and integer types are supported for matrices\n";
        return sema.type_error();
    }

    if (num_args() > 1 && num_args() != rows * cols) {
        error(this) << "incorrect number of initializers for matrix: got "
                    << num_args() << ", expected " << rows * cols << "\n";
    }

    return sema.matrix_type(type, rows, cols);
}

Type MatrixExpr::check_inverse(TypeSema& sema) const {
    if (num_args() != 1) {
        error(this) << "incorrect number of arguments for the inverse function\n";
        return sema.type_error();
    }

    auto mat = arg(0)->type().isa<MatrixType>();

    if (!mat || mat->is_vector() || mat->rows() != mat->cols()) {
        error(this) << "invalid operand type for the inverse function: got "
                    << arg(0)->type() << ", expected square matrix\n";
        return sema.type_error();
    }

    if (!sema.is_float(sema.scalar_type(mat->elem_type()))) {
        error(this) << "the inverse operation is only available for floating point matrices\n";
        return sema.type_error();
    } 

    return mat;
}

Type MatrixExpr::check_determinant(TypeSema& sema) const {
    if (num_args() != 1) {
        error(this) << "incorrect number of arguments for the determinant function\n";
        return sema.type_error();
    }

    auto mat = arg(0)->type().isa<MatrixType>();

    if (!mat || mat->is_vector() || mat->rows() != mat->cols()) {
        error(this) << "invalid operand type for the determinant function\n";
        return sema.type_error();
    }

    return mat->elem_type();
}

Type MatrixExpr::check_cross(TypeSema& sema) const {
    if (num_args() != 2) {
        error(this) << "incorrect number of arguments for the cross function\n";
        return sema.type_error();
    }

    auto lmat = arg(0)->type().isa<MatrixType>();
    auto rmat = arg(1)->type().isa<MatrixType>();
    
    if (lmat != rmat || !lmat || !lmat->is_vector() || lmat->rows() != 3) {
        error(this) << "invalid operand types for the cross function\n";
        return sema.type_error();
    }

    return sema.matrix_type(lmat->elem_type(), 3);
}

Type MatrixExpr::check_dot(TypeSema& sema) const {
    if (num_args() != 2) {
        error(this) << "incorrect number of arguments for the dot function\n";
        return sema.type_error();
    }

    auto lmat = arg(0)->type().isa<MatrixType>();
    auto rmat = arg(1)->type().isa<MatrixType>();
    
    if (lmat != rmat || !lmat || !lmat->is_vector()) {
        error(this) << "invalid operand types for the dot function\n";
        return sema.type_error();
    }

    return lmat->elem_type();
}

Type BlockExprBase::check(TypeSema& sema, TypeExpectation expected) const {
    THORIN_PUSH(sema.cur_block_, this);
    for (auto stmt : stmts())
        stmt->check(sema);

    sema.check(expr(), expected);

    for (auto local : locals_) {
        if (local->is_mut() && !local->is_written())
            warn(local) << "variable '" << local->symbol() << "' declared mutable but variable is never written to\n";
    }

    return expr() ? expr()->type() : sema.unit().as<Type>();
}

Type IfExpr::check(TypeSema& sema, TypeExpectation expected) const {
    sema.check(cond(), sema.type_bool(), "condition type");

    // if there is an expected type, we want to pipe it down to enable type inference
    // otherwise we cannot do so because if then_type is noret, else type still can be anything
    if (expected.type().isa<UnknownType>()) {
        Type then_type = sema.check(then_expr(), sema.unknown_type());
        Type else_type = sema.check(else_expr(), sema.unknown_type());

        if (then_type->is_noret() && else_type->is_noret())
            return sema.type_noret();
        if (then_type->is_noret())
            return sema.expect_type(else_expr(), TypeExpectation(expected, "if expression type"));
        if (else_type->is_noret())
            return sema.expect_type(then_expr(), TypeExpectation(expected, "if expression type"));
        if (then_type == else_type) {
            assert(!then_expr()->needs_cast());
            assert(!else_expr()->needs_cast());
            return sema.expect_type(this, then_type, TypeExpectation(expected, "if expression type"));
        }
        if (then_type <= else_type) {
            assert(!then_expr()->needs_cast());
            then_expr()->actual_type_ = then_type;
            then_expr()->type_.clear();
            then_expr()->type_ = else_type;
            return sema.expect_type(this, else_type, TypeExpectation(expected, "if expression type"));
        }
        if (else_type <= then_type) {
            assert(!else_expr()->needs_cast());
            else_expr()->actual_type_ = else_type;
            else_expr()->type_.clear();
            else_expr()->type_ = then_type;
            return sema.expect_type(this, then_type, TypeExpectation(expected, "if expression type"));
        }

        error(this) << "different types in arms of an if expression\n";
        error(then_expr()) << "type of the consequence is '" << then_type << "'\n";
        error(else_expr()) << "type of the alternative is '" << else_type << "'\n";
        return sema.type_error();
    } else {
        // we always allow noret in one of the branches as long
        Type then_type = sema.check(then_expr(), TypeExpectation(expected.type(), true, "type of then branch"));
        Type else_type = sema.check(else_expr(), TypeExpectation(expected.type(), true, "type of else branch"));
        return (then_type->is_noret()) ? else_type : then_type;
    }
}

Type WhileExpr::check(TypeSema& sema, TypeExpectation) const {
    sema.check(cond(), sema.type_bool(), "condition type");
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body(), sema.unit(), "body type of while loop");
    return sema.unit();
}

Type ForExpr::check(TypeSema& sema, TypeExpectation expected) const {
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
                    return sema.check_call(map, fn_for, map->type_args(), map->inferred_args_, args, expected);
                }
            }
        }
    } else if (auto field_expr = forexpr->isa<FieldExpr>()) {
        assert_unused(false && field_expr && "TODO");
    }

    error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
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
    item()->check_item(sema);
}

void LetStmt::check(TypeSema& sema) const {
    sema.cur_block_->add_local(local());
    Type expected = sema.check(local(), sema.unknown_type());
    if (init())
        sema.check(init(), expected, "initialization type");
    else if (!local()->is_mut())
        error(this) << "non-mutable let statement lacks initialization\n";
}

void check_correct_asm_type(const Type t, const Expr *expr) {
    if (!t.isa<PrimType>() && !t.isa<PtrType>() && !t.isa<SimdType>())
        error(expr) << "Asm operand must have primitive, pointer or SIMD type, but it is " << t << "\n";
}

void AsmStmt::check(TypeSema& sema) const {
    for (const auto& output : outputs()) {
        if (!output.expr()->is_lvalue())
            error(output.expr()) << "output expression of an asm statement must be an lvalue\n";
        check_correct_asm_type(sema.check(output.expr()), output.expr());
    }

    for (const auto& input : inputs())
        check_correct_asm_type(sema.check(input.expr()), input.expr());

    for (const auto& option : options()) {
        if (option != "volatile" && option != "alignstack" && option != "intel") {
            error(this) << "unsupported inline assembly option '"
                        << option << "', only 'volatile', 'alignstack' and 'intel' supported\n";
        }
    }
}

//------------------------------------------------------------------------------

void type_analysis(Init& init, const ModContents* mod, bool nossa) {
    auto sema = new TypeSema(nossa);
    init.typetable = sema;
    mod->check(*sema);
#ifndef NDEBUG
    sema->verify();
#endif
}

//------------------------------------------------------------------------------

}
