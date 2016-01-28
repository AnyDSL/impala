#include <sstream>

#include "thorin/util/array.h"
#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

class InferSema : public TypeTable {
public:
    // helpers

    TraitApp instantiate(const Location& loc, TraitAbs trait, Type self, ArrayRef<const ASTType*> args);
    Type instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args);

    int take_addr_space(const PrefixExpr* prefix) {
        check(prefix->rhs());
        if (prefix->kind() == PrefixExpr::MUL) {
            auto type = prefix->rhs()->type();
            if (auto ptr = type.isa<PtrType>())
                return ptr->addr_space();
        }
        return 0;
    }

    Type constrain(const Typeable* typeable, Type type) {
        todo_ |= typeable->type_ -= type;
        return typeable->type();
    }

    Type safe_get_arg(Type type, size_t i) {
        return type && i < type->num_args() ? type->arg(i) : unknown_type().as<Type>();
    }

    // check wrappers

    void check(const ModContents* n) { n->check(*this); }
    Type check(const LocalDecl* local) { return constrain(local, local->check(*this)); }
    void check(const Item* n) { n->check(*this); }
    Type check(const Expr* expr) { return check(expr, unknown_type()); }
    Type check(const Expr* expr, Type expected) { return constrain(expr, expr->check(*this, expected)); }
    void check(const Stmt* n) { n->check(*this); }

    TypeVar check(const TypeParam* type_param) {
        if (type_param->type())
            return type_param->type_var();
        todo_ = true;
        return (type_param->type_ = type_param->check(*this)).as<TypeVar>();
    }

    Type check(const ASTType* ast_type) {
        if (ast_type->type())
            return ast_type->type();

        if (auto type = ast_type->check(*this)) {
            if (type->is_known()) {
                todo_ = true;
                return ast_type->type_ = type;
            }
        }

        return Type();
    }

    Type check_call(const MapExpr* expr, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected);
    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types);

private:
    bool todo_ = true;

    friend void type_inference(Init&, const ModContents*);
};

void type_inference(Init& init, const ModContents* mod) {
    auto sema = new InferSema();
    init.typetable = sema;

    while (sema->todo_) {
        sema->todo_ = false;
        sema->check(mod);
    }

#ifndef NDEBUG
    sema->verify();
#endif
}

//------------------------------------------------------------------------------

#if 0
TraitApp InferSema::instantiate(const Location& loc, TraitAbs trait_abs, Type self, ArrayRef<const ASTType*> args) {
    if ((args.size()+1) == trait_abs->num_type_vars()) {
        std::vector<Type> type_args;
        type_args.push_back(self);
        for (auto t : args)
            type_args.push_back(check(t));

        // TODO
        //stash_bound_check(loc, *trait_abs, type_args);
        return trait_abs->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables of trait '" << trait_abs << "': " << args.size() << " for " << (trait_abs->num_type_vars()-1) << "\n";

    return trait_app_error();
}

Type InferSema::instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args) {
    if (args.size() == type->num_type_vars()) {
        std::vector<Type> type_args;
        for (auto t : args)
            type_args.push_back(check(t));

        // TODO
        //stash_bound_check(loc, *type, type_args);
        return type->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << args.size() << " for " << type->num_type_vars() << "\n";

    return type_error();
}

bool InferSema::check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> type_args) {
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
#endif

//------------------------------------------------------------------------------

/*
 * misc
 */

TypeVar TypeParam::check(InferSema& sema) const { return sema.type_var(symbol()); }

void TypeParamList::check_type_params(InferSema& sema) const {
    for (auto type_param : type_params()) {
        auto type_var = sema.check(type_param);
        for (auto bound : type_param->bounds()) {
            if (auto type_app = bound->isa<ASTTypeApp>())
                type_var->add_bound(type_app->trait_app(sema, type_var));
        }
    }
}

Type LocalDecl::check(InferSema& sema) const { return sema.check(ast_type()); }

Type Fn::check_body(InferSema& sema, FnType fn_type) const {
    return sema.check(body(), fn_type->return_type());
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

Type ErrorASTType::check(InferSema& sema) const { return sema.type_error(); }

Type PrimASTType::check(InferSema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.type(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Type PtrASTType::check(InferSema& sema) const {
    auto referenced_type = sema.check(referenced_ast_type());

    if (is_owned())
        return sema.owned_ptr_type(referenced_type, addr_space());
    else {
        assert(is_borrowed() && "only owned and borrowed ptrs are supported");
        return sema.borrowd_ptr_type(referenced_type, addr_space());
    }
}

Type IndefiniteArrayASTType::check(InferSema& sema) const { return sema.indefinite_array_type(sema.check(elem_ast_type())); }
Type DefiniteArrayASTType::check(InferSema& sema) const { return sema.definite_array_type(sema.check(elem_ast_type()), dim()); }

Type SimdASTType::check(InferSema& sema) const {
    auto elem_type = sema.check(elem_ast_type());
    if (elem_type.isa<PrimType>())
        return sema.simd_type(elem_type, size());
    return Type();
}

Type TupleASTType::check(InferSema& sema) const {
    Array<Type> types(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        types[i] = sema.check(arg(i));

    return sema.tuple_type(types);
}

Type FnASTType::check(InferSema& sema) const {
    check_type_params(sema);

    Array<Type> types(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        types[i] = sema.check(arg(i));

    auto fn_type = sema.fn_type(types);
    for (auto type_param : type_params()) {
        sema.check(type_param);
        fn_type->bind(type_param->type_var());
    }

    return fn_type;
}

Type Typeof::check(InferSema& sema) const { return sema.check(expr()); }

Type ASTTypeApp::check(InferSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>()) {
            if (auto type = type_decl->type())
                return sema.instantiate(loc(), type, args());
            else
                return sema.unknown_type();
        }
    }

    return sema.type_error();
}

TraitApp ASTTypeApp::trait_app(InferSema& sema, Type self) const {
    if (decl()) {
        if (auto trait_decl = decl()->isa<TraitDecl>()) {
            if (auto trait_abs = trait_decl->trait_abs())
                return sema.instantiate(this->loc(), trait_abs, self, args());
            else
                //return sema.unknown_type();
                return sema.trait_app_error(); // TODO we need an unknown_trait here
        } else
            error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
    return sema.trait_app_error();
}

//------------------------------------------------------------------------------

/*
 * items
 */

void ModDecl::check(InferSema& sema) const {
    if (mod_contents())
        sema.check(mod_contents());
}

void ModContents::check(InferSema& sema) const {
    for (auto item : items())
        sema.check(item);
}

void ExternBlock::check(InferSema& sema) const {
    for (auto fn : fns())
        sema.check(fn);
}

void Typedef::check(InferSema& sema) const {
    // TODO this is broken
    check_type_params(sema);
    sema.check(ast_type());

    if (type_params().size() > 0) {
        Type abs = sema.typedef_abs(ast_type()->type());
        for (auto type_param : type_params())
            abs->bind(type_param->type_var());
    } else
        sema.constrain(this, ast_type()->type());
}

void EnumDecl::check(InferSema&) const { /*TODO*/ }

void StructDecl::check(InferSema& sema) const {
    check_type_params(sema);

    for (auto field : field_decls()) {
        if (auto field_type = field->type()) {
            if (!field_type || !field_type->is_known())
                return; // bail out for now if we don't yet know all field types
        }
    }

    auto struct_type = sema.struct_abs_type(this);

    for (auto field : field_decls())
        struct_type->set(field->index(), field->type());

    for (auto type_param : type_params())
        struct_type->bind(type_param->type_var());

    type_ = struct_type;
}

void FieldDecl::check(InferSema& sema) const { sema.check(ast_type()); }

void FnDecl::check(InferSema& sema) const {
    check_type_params(sema);

    Array<Type> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.check(param(i));

    sema.constrain(this, sema.fn_type(param_types));

    for (auto type_param : type_params())
        fn_type()->bind(type_param->type_var());

    if (body() != nullptr)
        check_body(sema, fn_type());
}

void StaticItem::check(InferSema& sema) const {
    if (init())
        sema.constrain(this, init()->type());
}

#if 0

void TraitDecl::check(InferSema& sema) const {
    todo_ |= sema.check(self_param());
    TypeVar self_var = self_param()->type_var();
    trait_abs_ = sema.trait_abs(this);
    trait_abs_->bind(self_var);

    check_type_params(sema);
    for (auto type_param : type_params()) {
        todo_ |= sema.check(type_param);
        trait_abs_->bind(type_param->type_var());
    }

    for (auto type_app : super_traits()) {
        if (!trait_abs_->add_super_trait(type_app->trait_app(sema, self_var)))
            error(type_app) << "duplicate super trait '" << type_app << "' for trait '" << symbol() << "'\n";
    }

    for (auto method : methods())
        todo_ |= sema.check(method);
}

void ImplItem::check(InferSema& sema) const {
    check_type_params(sema);
    todo_ |= sema.check(ast_type());
    Type for_type = ast_type()->type();

    TraitApp trait_app;
    if (trait() != nullptr) {
        if (auto type_app = trait()->isa<ASTTypeApp>()) {
            trait_app = type_app->trait_app(sema, for_type);
            auto impl = sema.impl(this, trait_app, for_type);
            for (auto type_param : type_params()) {
                todo_ |= sema.check(type_param);
                impl->bind(type_param->type_var());
            }

            if (!for_type->is_error() && !trait_app->is_error()) {
                for_type.as<KnownType>()->add_impl(impl);
                trait_app->trait()->add_impl(impl);
            }
        } else
            error(trait()) << "expected trait instance\n";
    }

    thorin::HashSet<Symbol> implemented_methods;
    for (auto method : methods()) {
        todo_ |= sema.check(method);
        Type fn_type = method->type();

        if (trait() != nullptr) {
            assert(!trait_app.empty());

            Symbol meth_name = method->symbol();
            if (auto method_type = trait_app->find_method(meth_name)) {
                // remember name for check if all methods were implemented
                const auto& p = implemented_methods.insert(meth_name);
                assert(p.second && "there should be no such name in the set"); // else name analysis failed

                // check that the types match
                if (fn_type != method_type)
                    error(method) << "method '" << trait() << "." << meth_name << "' should have type '" << method_type << "', but implementation has type '" << fn_type << "'\n";
            }
        }
    }
}

#endif


//------------------------------------------------------------------------------

/*
 * expressions
 */

Type EmptyExpr::check(InferSema& sema, Type) const { return sema.unit(); }
Type LiteralExpr::check(InferSema& sema, Type) const { return sema.type(literal2type()); }
Type CharExpr::check(InferSema& sema, Type) const { return sema.type_u8(); }
Type StrExpr::check(InferSema& sema, Type) const { return sema.definite_array_type(sema.type_u8(), values_.size()); }

Type FnExpr::check(InferSema& sema, Type expected) const {
    assert(type_params().empty());

    Array<Type> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.constrain(param(i), sema.safe_get_arg(expected, i));

    auto fn_type = sema.fn_type(param_types);
    assert(body() != nullptr);
    check_body(sema, fn_type);
    return fn_type;
}

Type PathExpr::check(InferSema& sema, Type expected) const {
    if (value_decl())
        return sema.constrain(value_decl(), expected);
    return sema.type_error();
}

Type PrefixExpr::check(InferSema& sema, Type expected) const {
    switch (kind()) {
        case AND: {
            Type expected_referenced_type = sema.safe_get_arg(expected, 0);
            auto rtype = sema.check(rhs(), expected_referenced_type);
            int addr_space = 0;
#if 0
            // keep the address space of the original pointer, if possible
            if (auto map = rhs()->isa<MapExpr>()) {
                if (auto prefix = map->lhs()->isa<PrefixExpr>())
                    addr_space = sema.take_addr_space(prefix);
            } else if (auto field = rhs()->isa<FieldExpr>()) {
                if (auto prefix = field->lhs()->isa<PrefixExpr>())
                    addr_space = sema.take_addr_space(prefix);
            } else if (auto prefix = rhs()->isa<PrefixExpr>()) {
                addr_space = sema.take_addr_space(prefix);
            }
#endif

            return sema.borrowd_ptr_type(rtype, addr_space);

        }
        case TILDE:
            return sema.owned_ptr_type(sema.check(rhs(), sema.safe_get_arg(expected, 0)));
        case MUL:
            return sema.check(rhs(), sema.borrowd_ptr_type(expected));
        case INC: case DEC:
        case ADD: case SUB:
        case NOT:
        case RUN: case HLT:
            return sema.check(rhs(), expected);
        case OR:  case OROR: // Lambda
            THORIN_UNREACHABLE;
    }
    THORIN_UNREACHABLE;
}

Type InfixExpr::check(InferSema& sema, Type expected) const {
    switch (kind()) {
        case EQ: case NE:
        case LT: case LE:
        case GT: case GE:
            sema.check(lhs(), expected - rhs()->type());
            sema.check(rhs(), expected - lhs()->type());
            return sema.type_bool();
        case OROR:
        case ANDAND:
            sema.check(lhs(), sema.type_bool());
            sema.check(rhs(), sema.type_bool());
            return sema.type_bool();
        case ADD: case SUB:
        case MUL: case DIV: case REM:
        case SHL: case SHR:
        case AND: case OR:  case XOR:
            sema.check(lhs(), expected - rhs()->type());
            sema.check(rhs(), expected - lhs()->type());
            return lhs()->type() - rhs()->type();
        case ASGN:
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
        case SHL_ASGN: case SHR_ASGN:
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN:
            sema.check(lhs(), rhs()->type());
            sema.check(rhs(), lhs()->type());
            return sema.unit();
    }

    THORIN_UNREACHABLE;
}

Type PostfixExpr::check(InferSema& sema, Type expected) const {
    return sema.check(lhs(), expected);
}

Type CastExpr::check(InferSema& sema, Type) const {
    sema.check(lhs());
    return sema.check(ast_type());
}

Type DefiniteArrayExpr::check(InferSema& sema, Type expected) const {
    Type expected_elem_type = sema.safe_get_arg(expected, 0);

    for (auto arg : args())
        expected_elem_type -= arg->type();

    for (auto arg : args())
        sema.check(arg, expected_elem_type);

    return sema.definite_array_type(expected_elem_type, num_args());
}

Type SimdExpr::check(InferSema& sema, Type expected) const {
    Type expected_elem_type = sema.safe_get_arg(expected, 0);

    for (auto arg : args())
        expected_elem_type -= arg->type();

    for (auto arg : args())
        expected_elem_type -= sema.check(arg, expected_elem_type);

    return sema.simd_type(expected_elem_type, num_args());
}

Type RepeatedDefiniteArrayExpr::check(InferSema& sema, Type expected) const {
    Type expected_elem_type = sema.safe_get_arg(expected, 0);
    return sema.definite_array_type(sema.check(value(), expected_elem_type), count());
}

Type IndefiniteArrayExpr::check(InferSema& sema, Type) const {
    sema.check(dim());
    return sema.indefinite_array_type(sema.check(elem_ast_type()));
}

Type TupleExpr::check(InferSema& sema, Type expected) const {
    Array<Type> types(num_args());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.check(arg(i), sema.safe_get_arg(expected, i));

    return sema.tuple_type(types);
}

#if 0

Type StructExpr::check(InferSema& sema, Type expected) const {
    if (auto decl = path()->decl()) {
        if (auto typeable_decl = decl->isa<TypeableDecl>()) {
            if (auto decl_type = typeable_decl->type()) {
                if (num_type_args() <= decl_type->num_type_vars()) {
                    StructAppType exp_type = expected.isa<StructAppType>();

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

                if (struct_app)
                    return struct_app;
            } else
                return sema.unknown_type();
        }
    }
    return sema.type_error();
}

Type InferSema::check_call(const MapExpr* expr, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected) {
    size_t num_type_args = type_args.size();
    size_t num_args = args.size();

    if (num_type_args <= fn_poly->num_type_vars()) {
        for (auto type_arg : type_args)
            inferred_args.push_back(check(type_arg));

        for (size_t i = num_type_args, e = fn_poly->num_type_vars(); i != e; ++i)
            inferred_args.push_back(unknown_type());

        assert(inferred_args.size() == fn_poly->num_type_vars());
        auto fn_mono = fn_poly->instantiate(inferred_args).as<FnType>();

        bool is_contuation = num_args == fn_mono->num_args();
        if (is_contuation || num_args+1 == fn_mono->num_args()) {
            for (size_t i = 0; i != num_args; ++i)
                check(args[i], fn_mono->arg(i), "argument type");

            // note: the order is important because of the unifying side-effects of ==
            if (is_contuation || fn_mono->return_type() == expected) { // TODO this looks overly complicated
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
                    if (!fn_mono->return_type()->is_noret())
                        return expect_type(expr, fn_mono->return_type(), expected);
                    error(expr) << "missing last argument to call continuation\n";
                }
            } else
                error(expr->loc()) << "return type '" << fn_mono->return_type() << "' does not match expected type '" << expected << "'\n";
        } else {
            std::string rela = (num_args+1 < fn_mono->num_args()) ? "few" : "many";
            size_t exp_args = fn_mono->num_args() > 0 ? fn_mono->num_args()-1 : 0;
            error(expr->loc()) << "too " << rela << " arguments: " << num_args << " for " << exp_args << "\n";
        }
    } else
        error(expr->loc()) << "too many type arguments to function: " << num_type_args << " for " << fn_poly->num_type_vars() << "\n";

    return type_error();
}

Type FieldExpr::check(InferSema& sema, Type expected) const {
    if (auto type = check_as_struct(sema, expected))
        return type;

    if (!lhs()->type()->is_error())
        error(lhs()) << "attempted access of field '" << symbol() << "' on type '" << lhs()->type() << "', but no field with that name was found\n";
    return sema.type_error();
}

Type FieldExpr::check_as_struct(InferSema& sema, Type expected) const {
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

    return Type();
}

Type MapExpr::check(InferSema& sema, Type expected) const {
    if (auto field_expr = lhs()->isa<FieldExpr>()) {
        if (field_expr->check_as_struct(sema, sema.unknown_type()))
            return check_as_map(sema, expected);
        return check_as_method_call(sema, expected);
    }

    return check_as_map(sema, expected);
}

Type MapExpr::check_as_map(InferSema& sema, Type expected) const {
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

Type MapExpr::check_as_method_call(InferSema& sema, Type expected) const {
    auto field_expr = lhs()->as<FieldExpr>();
    if (auto fn_method = sema.check(field_expr->lhs())->find_method(field_expr->symbol())) {
        Array<const Expr*> nargs(num_args() + 1);
        nargs[0] = field_expr->lhs();
        std::copy(args().begin(), args().end(), nargs.begin()+1);
        return field_expr->type_ = sema.check_call(this, fn_method, type_args(), inferred_args_, nargs, expected);
    } else
        error(this) << "no declaration for method '" << field_expr->symbol() << "' found\n";
    return sema.type_error();
}
#endif

Type BlockExprBase::check(InferSema& sema, Type expected) const {
    for (auto stmt : stmts())
        sema.check(stmt);

    sema.check(expr(), expected);

    return expr() ? expr()->type() : sema.unit().as<Type>();
}

Type IfExpr::check(InferSema& sema, Type expected) const {
    sema.check(cond(), sema.type_bool());
    sema.check(then_expr(), expected - else_expr()->type());
    sema.check(else_expr(), expected - then_expr()->type());
    return then_expr()->type() - else_expr()->type();
}

Type WhileExpr::check(InferSema& sema, Type) const {
    sema.check(cond(), sema.type_bool());
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body(), sema.unit());
    return sema.unit();
}

#if 0
Type ForExpr::check(InferSema& sema, Type expected) const {
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
        assert(false && field_expr && "TODO");
    }

    error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
}
#endif

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(InferSema& sema) const { sema.check(expr()); }
void ItemStmt::check(InferSema& sema) const { sema.check(item()); }

void LetStmt::check(InferSema& sema) const {
    auto expected = sema.check(local());
    if (init())
        sema.check(init(), expected);
}

//------------------------------------------------------------------------------

}
