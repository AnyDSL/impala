#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

using thorin::Array;
using thorin::ArrayRef;

namespace impala {

struct InstantiationHash {
    size_t operator () (const TypeVar t) const { return t->hash(); }
};
struct InstantiationEqual {
    bool operator () (TypeVar t1, TypeVar t2) const { return (t1->is_unified() && t2->is_unified()) ? t1.as<Type>() == t2.as<Type>() : t1->equal(*t2); }
};
typedef thorin::HashMap<TypeVar, Type, InstantiationHash, InstantiationEqual> Instantiationmap;

//------------------------------------------------------------------------------

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    void push_impl(const Impl* i) { impls_.push_back(i); }
    virtual void check_impls() {
        while (!impls_.empty()) {
            const Impl* i = impls_.back();
            impls_.pop_back();
            check_item(i);
        }
    }

    // error handling

    void expect_num(const Expr* exp);
    Type match_types(const ASTNode* pos, Type t1, Type t2);
    Type expect_type(const Expr* expr, Type found, Type expected, std::string typetype);
    Type expect_type(const Expr* expr, Type expected, std::string typetype) { return expect_type(expr, expr->type(), expected, typetype); }
    Type create_return_type(const ASTNode* node, Type ret_func);

    Type instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<const ASTType*> var_instances);
    Trait instantiate(const ASTNode* loc, Trait trait, Type self, thorin::ArrayRef<const ASTType*> var_instances);
    Type check_call(const Expr* lhs, const Expr* whole, ArrayRef<const Expr*> args, Type expected);

    // check wrappers

    Type check(const TypeDecl* type_decl) {
        if (!type_decl->checked_) { 
            type_decl->checked_ = true; 
            type_decl->type_ = type_decl->check(*this);
        }
        return type_decl->type();
    }
    Type check(const ValueDecl* decl, Type expected) {
        if (!decl->checked_) {
            decl->checked_ = true;
            decl->type_ = decl->check(*this, expected);
        }
        return decl->type();
    }
    void check_item(const Item* item) { item->check_item(*this); }
    Type check(const Expr* expr, Type expected, std::string typetype) {
        if (!expr->type_.empty())
            return expr->type_;
        Type inferred = expr->check(*this, expected);
        return expr->type_ = expect_type(expr, inferred, expected, typetype);
    }
    Type check(const Expr* expr, Type expected) { return check(expr, expected, ""); }
    /// a check that does not expect any type (i.e. any type is allowed)
    Type check(const Expr* expr) { return check(expr, unknown_type()); }
    Type check(const ASTType* ast_type) { return ast_type->type_ = ast_type->check(*this); }

private:
    bool nossa_;
    std::vector<const Impl*> impls_;
};

//------------------------------------------------------------------------------

void TypeSema::expect_num(const Expr* exp) {
    Type t = exp->type();

    if (t == type_error())
        return;

    if ((t != type_i8()) && (t != type_i16()) && (t != type_i32()) && // TODO factor this test out
            (t != type_i64()) && (t != type_f32()) && (t != type_f64()))
        error(exp) << "expected number type but found " << t << "\n";
}

Type TypeSema::match_types(const ASTNode* pos, Type t1, Type t2) {
    if (t1 == type_error() || t2 == type_error())
        return type_error();

    if (t1 == t2) {
        return t1;
    } else {
        error(pos) << "types do not match: " << t1 << " != " << t2 << "\n";
        return type_error();
    }
}

Type TypeSema::expect_type(const Expr* expr, Type found_type, Type expected, std::string typetype) {
    if (auto ut = expected.isa<UnknownType>()) {
        if (!ut->is_instantiated()) {
            if (found_type.isa<UnknownType>()) {
                return found_type;
            } else {
                ut->instantiate(found_type);
                return found_type;
            }
        }
    }

    // FEATURE make this check faster - e.g. store a "potentially not closed" flag
    if (!expected->is_closed())
        return found_type;

    if (found_type == type_error() || expected == type_error())
        return Type(*expected);
    if (found_type != expected) {
        if (found_type->is_generic()) {
            // try to infer instantiations for this generic type
            std::vector<Type> inst_types;
            Type inst = instantiate_unknown(found_type, inst_types);
            if (inst->unify_with(expected)) {
                for (auto t : inst_types) {
                    assert(t.representative() != nullptr);
                    expr->add_inferred_arg(Type(t.representative()));
                }
                // needed for bound checking
                check_bounds(expr, found_type, expr->inferred_args());
                return Type(*expected);
            }
        }
        error(expr) << "wrong " << typetype << " type; expected " << expected << " but found " << found_type << "\n";
    }
    return Type(*expected);
}

Type TypeSema::create_return_type(const ASTNode* node, Type ret_func) {
    if (ret_func.isa<FnType>()) {
        if (ret_func->size() == 1) {
            return ret_func->elem(0);
        } else {
            std::vector<Type> ret_types;
            for (auto t : ret_func->elems())
                ret_types.push_back(t);
            return tuple_type(ret_types);
        }
    } else {
        error(node) << "last argument is not a continuation function\n";
        return type_error();
    }
}

Trait TypeSema::instantiate(const ASTNode* loc, Trait trait, Type self, thorin::ArrayRef<const ASTType*> var_instances) {
    if ((var_instances.size()+1) == trait->num_type_vars()) {
        std::vector<Type> inst_types;
        inst_types.push_back(self);
        for (auto t : var_instances) inst_types.push_back(check(t));
        auto map = infer(trait, inst_types);

        check_bounds(loc, trait, inst_types, map);
        return trait->instantiate(map);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << var_instances.size() << " for " << (trait->num_type_vars()-1) << "\n";

    return trait_error();
}

Type TypeSema::instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<const ASTType*> var_instances) {
    if (var_instances.size() == type->num_type_vars()) {
        std::vector<Type> inst_types;
        for (auto t : var_instances) inst_types.push_back(check(t));
        auto map = infer(type, inst_types);

        check_bounds(loc, type, inst_types, map);
        return type->instantiate(map);
    } else {
        error(loc) << "wrong number of instances for bound type variables: " << var_instances.size() << " for " << type->num_type_vars() << "\n";
    }

    return type_error();
}

//------------------------------------------------------------------------------

/*
 * ASTType::check
 */

void TypeParamList::check_type_params(TypeSema& sema) const {
    // check bounds
    for (auto tp : type_params()) {
        for (auto b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>()) {
                TypeVar v = tp->type_var(sema);
                v->add_bound(trait_inst->to_trait(sema, v));
            } else {
                sema.error(tp) << "bounds must be trait instances, not types\n";
            }
        }
    }
}

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
    return Type(); // FEATURE
}

Type IndefiniteArrayASTType::check(TypeSema& sema) const {
    return Type(); // FEATURE
}

Type DefiniteArrayASTType::check(TypeSema& sema) const {
    return Type(); // FEATURE
}

Type TupleASTType::check(TypeSema& sema) const {
    std::vector<Type> types;
    for (auto elem : elems())
        types.push_back(sema.check(elem));

    return sema.tuple_type(types);
}

Type FnASTType::check(TypeSema& sema) const {
    check_type_params(sema);

    std::vector<Type> params;
    for (auto elem : elems())
        params.push_back(sema.check(elem));

    FnType fn_type = sema.fn_type(params);
    for (auto type_param : type_params())
        fn_type->bind(type_param->type_var(sema));

    return fn_type;
}

Type ASTTypeApp::check(TypeSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>()) {
            assert(elems().empty());
            return sema.check(type_decl);
        } else
            sema.error(this) << '\'' << symbol() << "' does not name a type\n";
    }

    return sema.type_error();
}

Trait ASTTypeApp::to_trait(TypeSema& sema, Type self) const {
    if (decl()) {
        if (auto trait_decl = decl()->isa<TraitDecl>()) {
            Trait trait = trait_decl->to_trait(sema);
            return sema.instantiate(this, trait, self, elems());
        } else
            sema.error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
    return sema.trait_error();
}

//------------------------------------------------------------------------------

Type ValueDecl::check(TypeSema& sema) const { return check(sema, Type()); }

Type ValueDecl::check(TypeSema& sema, Type expected) const {
    if (ast_type()) {
        Type t = sema.check(ast_type());
        if (expected.empty() || expected->unify_with(t)) {
            return t;
        } else {
            sema.error(this) << "could not unify types: expected '" << expected << "' but found '" << t << "'.\n";
            return sema.type_error();
        }
    } else if (expected.empty()) {
        sema.error(this) << "could not infer parameter type for " << this << ".\n";
        return sema.type_error();
    } else {
        return expected;
    }
}

Trait TraitDecl::to_trait(TypeSema& sema) const {
    check_item(sema);
    return trait();
}

void Fn::check_body(TypeSema& sema, FnType fn_type) const {
    Type body_type = sema.check(body());
    if (!body_type->is_closed()) return; // FEATURE make this check faster - e.g. store a "potentially not closed" flag
    if (body_type != sema.type_noreturn() && body_type != sema.type_error())
        sema.expect_type(body(), fn_type->return_type(), "return");
}

//------------------------------------------------------------------------------

/*
 * Items
 */

void TypeDeclItem::check_item(TypeSema& sema) const { sema.check(static_cast<const TypeDecl*>(this)); }
void ValueItem::check_item(TypeSema& sema) const { sema.check(static_cast<const ValueDecl*>(this)); }

Type TypeParam::check(TypeSema& sema) const { return sema.type_var(symbol()); }

Type ModDecl::check(TypeSema& sema) const {
    if (mod_contents())
        mod_contents()->check(sema);
    return Type();
}

void ModContents::check(TypeSema& sema) const {
    std::vector<const Item*> non_impls;
    for (auto item : items()) {
        if (auto impl = item->isa<const Impl>()) {
            sema.push_impl(impl);
        } else
            non_impls.push_back(item);
    }

    sema.check_impls();
    for (auto item : non_impls)
        sema.check_item(item);
}

Type ForeignMod::check(TypeSema& sema) const {
    return Type();
}

Type Typedef::check(TypeSema& sema) const {
    return Type();
}

Type EnumDecl::check(TypeSema& sema) const {
    return Type();
}

Type StructDecl::check(TypeSema& sema) const {
    return Type();
}

Type FieldDecl::check(TypeSema&) const {
    return Type();
}

Type FnDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    std::vector<Type> types;
    for (auto param : params())
        types.push_back(sema.check(param));

    // create FnType
    FnType fn_type = sema.fn_type(types);
    for (auto tp : type_params())
        fn_type->bind(tp->type_var(sema));
    type_ = fn_type;
    sema.unify(type_);

    if (body() != nullptr)
        check_body(sema, fn_type);

    type_.clear(); // will be set again by TypeSema's wrapper
    return fn_type;
}

Type StaticItem::check(TypeSema& sema) const {
    return Type();
}

void TraitDecl::check_item(TypeSema& sema) const {
    // did we already check this trait?
    if (!trait().empty())
        return;

    TypeVar self_var = self_param()->type_var(sema);
    trait_ = sema.trait(this);
    trait_->bind(self_var);

    check_type_params(sema);
    for (auto tp : type_params())
        trait_->bind(tp->type_var(sema));

    for (auto t : super())
        trait_->add_super_trait(t->to_trait(sema, self_var));

    // check methods
    for (auto m : methods()) {
        if (!trait_->add_method(m->symbol(), sema.check(m)))
            sema.error(m) << "a method with this name already exists in a super trait.\n";
    }

    sema.unify(trait());
}

void Impl::check_item(TypeSema& sema) const {
    check_type_params(sema);
    Type ftype = sema.check(for_type());

    Trait tinst;
    if (trait() != nullptr) {
        if (auto t = trait()->isa<ASTTypeApp>()) {
            // create impl
            tinst = t->to_trait(sema, ftype);
            TraitImpl impl = sema.implement_trait(this, tinst);
            for (auto tp : type_params()) {
                impl->bind(tp->type_var(sema));
            }

            // add impl to type
            if ((ftype != sema.type_error()) && (tinst != sema.trait_error()))
                ftype->add_implementation(impl);
        } else
            sema.error(trait()) << "expected trait instance.\n";
    }

    thorin::HashSet<Symbol> implemented_methods;
    for (auto fn : methods()) {
        Type fn_type = sema.check(fn);

        if (trait() != nullptr) {
            assert(!tinst.empty());

            Symbol meth_name = fn->symbol();
            Type t = tinst->find_method(meth_name);
            if (!t.empty()) {
                // remember name for check if all methods were implemented
                auto p = implemented_methods.insert(meth_name);
                assert(p.second && "There should be no such name in the set"); // else name analysis failed

                // check that the types match
                sema.match_types(fn, fn_type, t);
            }
        }
    }

    // check that all methods are implemented
    if (!tinst.empty()) {
        if (implemented_methods.size() != tinst->num_methods()) {
            assert(implemented_methods.size() < tinst->num_methods());
            for (auto p : tinst->all_methods()) {
                if (!implemented_methods.contains(p.first))
                    sema.error(this) << "Must implement method '" << p.first << "'\n";
            }
        }
    }
}

//------------------------------------------------------------------------------

/*
 * Expr::check
 */

Type EmptyExpr::check(TypeSema& sema, Type) const { return sema.unit(); }

Type BlockExpr::check(TypeSema& sema, Type expected) const {
    for (auto stmt : stmts())
        stmt->check(sema);

    sema.check(expr(), expected);
    return expr() ? expr()->type() : Type(sema.unit());
}

Type LiteralExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE we could enhance this using the expected type (e.g. 4 could be interpreted as int8 if needed)
    return sema.type(literal2type());
}

Type FnExpr::check(TypeSema& sema, Type expected) const {
    assert(type_params().empty());

    FnType fn_type;
    if (FnType exp_fn = expected.isa<FnType>()) {
        if (exp_fn->size() != params().size())
            sema.error(this) << "expected function with " << exp_fn->size() << " parameters, but found lambda expression with " << params().size() << " parameters\n";

        size_t i = 0;
        for (auto param : params())
            sema.check(param, exp_fn->elem(i++));

        fn_type = exp_fn;
    } else {
        std::vector<Type> par_types;
        for (auto param : params())
            par_types.push_back(sema.check(param));

        fn_type = sema.fn_type(par_types);
        sema.unify(fn_type);
    }

    assert(body() != nullptr);
    check_body(sema, fn_type);

    return fn_type;
}

Type PathExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE consider longer paths
    auto* last = path()->path_elems().back();
    if (value_decl()) {
        Type dec_type = sema.check(value_decl());
        if (last->args().empty()) {
            return dec_type;
        } else {
            if (dec_type != sema.type_error())
                return sema.instantiate(last, dec_type, last->args());
        }
    }
    return sema.type_error();
}

Type PrefixExpr::check(TypeSema& sema, Type expected) const {
    return sema.check(rhs(), expected); // TODO check if operator supports the type
}

Type InfixExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE other cases
    switch (kind()) {
        case EQ:
        case NE:
            sema.check(rhs(), sema.check(lhs()));
            return sema.type_bool();
        case LT:
        case LE:
        case GT:
        case GE:
            sema.check(rhs(), sema.check(lhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return sema.type_bool();
        case OROR:
        case ANDAND:
            sema.check(lhs(), sema.type_bool(), "left-hand side of logical boolean expression");
            sema.check(rhs(), sema.type_bool(), "right-hand side of logical boolean expression");
            return sema.type_bool();
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM: {
            auto type = sema.check(rhs(), sema.check(lhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return type;
        }
        case ASGN:
            sema.check(rhs(), sema.check(lhs()));
            return sema.unit();
        case ADD_ASGN:
        case SUB_ASGN:
        case MUL_ASGN:
        case DIV_ASGN:
        case REM_ASGN:
        case AND_ASGN:
        case  OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN: {
            // TODO handle floats etc
            sema.check(rhs(), sema.check(lhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
            return sema.unit();
        }
        default: THORIN_UNREACHABLE;
    }
}

Type PostfixExpr::check(TypeSema& sema, Type expected) const {
    return sema.check(lhs(), expected); // TODO check if operator supports the type
}

Type FieldExpr::check(TypeSema& sema, Type expected) const {
    sema.check(lhs());

    // FEATURE struct types
    // FEATURE maybe store a hash map of methods in the type to make this fast!
    sema.check_impls();
    Type fn = lhs()->type()->find_method(path_elem()->symbol());
    if (!fn.empty()) {
        if (fn != sema.type_error()) {
            FnType func;
            if (!path_elem()->args().empty()) {
                Type t = sema.instantiate(path_elem(), fn, path_elem()->args());
                sema.unify(t);
                func = t.as<FnType>();
            } else
                func = fn.as<FnType>();

            if (func->size() >= 1) {
                sema.expect_type(lhs(), func->elem(0), "object");
                return func->specialize_method(lhs()->type());
            } else
                sema.error(this) << "cannot call a method without Self parameter";
        }
        return sema.type_error();
    }
    sema.error(this) << "no declaration for method '" << path_elem() << "' found.\n";
    return sema.type_error();
}

Type CastExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type DefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type RepeatedDefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type IndefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type TupleExpr::check(TypeSema& sema, Type expected) const {
    std::vector<Type> types;
    if (auto exp_tup = expected.isa<TupleType>()) {
        if (exp_tup->size() != elems().size())
            sema.error(this) << "expected tuple with " << exp_tup->size() << " elements, but found tuple expression with " << elems().size() << " elements.\n";

        size_t i = 0;
        for (auto e : elems()) {
            sema.check(e, exp_tup->elem(i++));
            types.push_back(e->type());
        }
    } else {
        for (auto e : elems()) {
            sema.check(e);
            types.push_back(e->type());
        }
    }
    return sema.tuple_type(types);
}

Type StructExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type TypeSema::check_call(const Expr* lhs, const Expr* whole, ArrayRef<const Expr*> args, Type expected) {
    std::vector<Type> inst_types;
    FnType ofn = lhs->type().as<FnType>();
    FnType fn = ofn->is_generic() ? instantiate_unknown(ofn, inst_types).as<FnType>() : ofn;

    bool no_cont = fn->size() == (args.size()+1); // true if this is a normal function call (no continuation)
    if (no_cont || (fn->size() == args.size())) {
        for (size_t i = 0; i < args.size(); ++i) {
            check(args[i], fn->elem(i), "argument");
        }
        if (no_cont && !expected.isa<UnknownType>())
            create_return_type(whole, fn->elems().back())->unify_with(expected);

        // instantiate fn type
        if (ofn->is_generic()) {
            bool no_error = true;
            for (size_t i = 0; i < inst_types.size(); ++i) {
                UnknownType ut = inst_types[i].isa<UnknownType>();
                if (!ut || ut->is_instantiated()) {
                    unify(inst_types[i]);
                    lhs->add_inferred_arg(Type(inst_types[i].representative()));
                } else {
                    error(whole) << "could not find instance for type variable #" << i << ".\n";
                    no_error = false;
                }
            }
            if (no_error) {
                // TODO where should be set this new type? should we set it at all?
                check_bounds(whole, ofn, lhs->inferred_args());
            }
        }

        if (no_cont) // return type
            return create_return_type(whole, fn->elems().back()); 
        else        // same number of args as params -> continuation call
            return type_noreturn();
    } else
        error(whole) << "wrong number of arguments\n";
    return type_error();
}

Type MapExpr::check(TypeSema& sema, Type expected) const {
    Type lhst = sema.check(lhs());
    sema.unify(lhst);

    if (auto ofn = lhst.isa<FnType>()) {
        return sema.check_call(lhs(), this, args(), expected);
    } else if (!lhs()->type().isa<TypeError>()) {
        // REMINDER new error message if not only fn-types are allowed
        sema.error(lhs()) << "expected function type but found " << lhs()->type() << "\n";
    }

    return sema.type_error();
}

Type IfExpr::check(TypeSema& sema, Type expected) const {
    sema.check(cond(), sema.type_bool(), "condition");
    Type then_type = sema.check(then_expr(), sema.unknown_type());
    Type else_type = sema.check(else_expr(), sema.unknown_type());
    Type type = then_type.isa<NoReturnType>() ? else_type : then_type;
    if (!type.isa<TypeError>())
        return sema.expect_type(this, type, expected, "if expression");
    else
        return expected->is_known() ? expected : else_type;
}

Type ForExpr::check(TypeSema& sema, Type expected) const {
    if (auto map = expr()->isa<MapExpr>()) {
        Type lhst = sema.check(map->lhs());
        sema.unify(lhst);

        if (auto ofn = lhst.isa<FnType>()) {
            Array<const Expr*> args(map->args().size()+1);
            *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
            return sema.check_call(map->lhs(), this, args, expected);
        }
    } else if (auto field_expr = expr()->isa<FieldExpr>()) {
        assert(false && field_expr && "TODO");
    }

    sema.error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
}

//------------------------------------------------------------------------------

/*
 * Stmt::check
 */

void ExprStmt::check(TypeSema& sema) const {
    if (sema.check(expr()) == sema.type_noreturn())
        sema.error(expr()) << "expression does not return rendering subsequent statements unreachable\n";
}

void ItemStmt::check(TypeSema& sema) const {
    sema.check_item(item());
}

void LetStmt::check(TypeSema& sema) const {
    Type expected = sema.check(local(), sema.unknown_type());
    if (init())
        sema.check(init(), expected);
}

//------------------------------------------------------------------------------

bool type_analysis(Init& init, const ModContents* mod, bool nossa) {
    auto sema = new TypeSema(nossa);
    init.typetable = sema;
    mod->check(*sema);
#ifndef NDEBUG
    sema->verify();
#endif
    return sema->result();
}

//------------------------------------------------------------------------------

}
