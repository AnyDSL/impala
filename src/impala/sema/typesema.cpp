#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

namespace impala {

struct InstantiationHash {
    size_t operator () (const TypeVar t) const { return t->hash(); }
};
struct InstantiationEqual {
    bool operator () (TypeVar t1, TypeVar t2) const { return (t1->is_unified() && t2->is_unified()) ? t1.as<Type>() == t2.as<Type>() : t1->equal(*t2); }
};
typedef thorin::HashMap<TypeVar, Type, InstantiationHash, InstantiationEqual> InstantiationMapping;

//------------------------------------------------------------------------------

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    void push_impl(const Impl* i) { impls_.push_back(i); }
    void check_impls() {
        while (!impls_.empty()) {
            const Impl* i = impls_.back();
            impls_.pop_back();
            check(i);
        }
    }

    void expect_num(const Expr* exp);
    Type match_types(const ASTNode* pos, Type t1, Type t2);
    void expect_type(const Expr* found, Type expected, std::string typetype);
    Type create_return_type(const ASTNode* node, Type ret_func);
    void check_body(const ASTNode* fn, const Expr* body, Type fn_type) {
        Type body_type = check(body);
        if ((body_type != type_noreturn()) && (body_type != type_error())) {
            Type ret_func = fn_type->elems().back();  // TODO may be noret
            expect_type(body, create_return_type(fn, ret_func), "return");
        }
    }

    template<class T> void check_bounds(const ASTNode* loc, T generic, thorin::ArrayRef<Type> inst_types, SpecializeMapping& mapping);
    template<class T> T instantiate_unknown(T, std::vector<Type>& inst_types);
    Type instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<const ASTType*> var_instances);
    Type instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<Type> var_instances);
    Trait instantiate(const ASTNode* loc, Trait trait, Type self, thorin::ArrayRef<const ASTType*> var_instances);

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
    void check(const Item* item) { 
        if (auto type_decl_item = item->isa<TypeDeclItem>())
            check(type_decl_item);
        else if (auto value_item = item->isa<ValueItem>())
            check(value_item);
        else
            check(item->isa<MiscItem>());
    }
    Type check(const TypeDeclItem* type_decl_item) { return check((const TypeDecl*) type_decl_item); }
    Type check(const ValueItem* value_item) { return check((const TypeDecl*) value_item); }
    void check(const MiscItem* misc_item) { misc_item->check(*this); }
    Type check(const Expr* expr, Type expected, std::string typetype) {
        if (!expr->type_.empty())
            return expr->type_;

        if (auto ut = expected.isa<UninstantiatedType>()) {
            if (!ut->is_instantiated()) {
                expr->type_ = expr->check(*this, ut);
                if (!expr->type_.empty()) {
                    // if this variable has no instantiation yet instantiate it
                    if (!ut->is_instantiated()) {
                        ut->instantiate(expr->type_);
                    } else
                        assert(expr->type_ == ut->instance());
                }
                return ut->instance();
            } else
                expected = ut->instance();
        }
        expr->type_ = expr->check(*this, expected);
        expect_type(expr, expected, typetype);
        return expected;
    }
    Type check(const Expr* expr, Type expected) { return check(expr, expected, ""); }
    /// a check that does not expect any type (i.e. any type is allowed)
    Type check(const Expr* expr) { return check(expr, uninstantiated_type()); }
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

    if ((t != type_int()) && (t != type_int8()) && (t != type_int16()) && (t != type_int32()) &&
            (t != type_int64()) && (t != type_float()) && (t != type_double()))
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

void TypeSema::expect_type(const Expr* found, Type expected, std::string typetype) {
    if (found->type() == type_error() || expected == type_error())
        return;
    if (found->type() != expected)
        error(found) << "wrong " << typetype << " type; expected " << expected << " but found " << found->type() << "\n";
}

Type TypeSema::create_return_type(const ASTNode* node, Type ret_func) {
    if (ret_func.isa<FnType>()) {
        if (ret_func->size() == 1) {
            return ret_func->elem(0);
        } else {
            std::vector<Type> ret_types;
            for (auto t : ret_func->elems())
                ret_types.push_back(t);
            return tupletype(ret_types);
        }
    } else {
        error(node) << "last argument is not a continuation function\n";
        return type_error();
    }
}

template<class T> void TypeSema::check_bounds(const ASTNode* loc, T generic, thorin::ArrayRef<Type> inst_types, SpecializeMapping& mapping) {
    assert(inst_types.size() == generic->num_bound_vars());
    assert(inst_types.size() == mapping.size());
    // check the bounds
    for (size_t i = 0; i < generic->num_bound_vars(); ++i) {
        TypeVar v = generic->bound_var(i);
        Type instance = inst_types[i];
        assert(mapping.contains(*v));
        assert(mapping[*v] == *instance);

        for (Trait bound : v->bounds()) {
            SpecializeMapping m(mapping); // copy the mapping
            Trait spec_bound = bound->specialize(m);
            unify(spec_bound);

            if (instance != type_error() && spec_bound != trait_error()) {
                check_impls(); // first we need to check all implementations to be up-to-date
                if (!instance->implements(spec_bound))
                    error(loc) << "'" << instance << "' does not implement bound '" << spec_bound << "'\n";
            }
        }
    }
}

template<class T> T TypeSema::instantiate_unknown(T t, std::vector<Type>& inst_types) {
    // FIXME remember bound checking
    for (size_t i = 0; i < t->num_bound_vars(); ++i) inst_types.push_back(uninstantiated_type());
    SpecializeMapping mapping = create_spec_mapping(t, inst_types);
    return t->instantiate(mapping);
}

Trait TypeSema::instantiate(const ASTNode* loc, Trait trait, Type self, thorin::ArrayRef<const ASTType*> var_instances) {
    if ((var_instances.size()+1) == trait->num_bound_vars()) {
        std::vector<Type> inst_types;
        inst_types.push_back(self);
        for (auto t : var_instances) inst_types.push_back(check(t));
        SpecializeMapping mapping = create_spec_mapping(trait, inst_types);

        check_bounds(loc, trait, inst_types, mapping);
        return trait->instantiate(mapping);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << var_instances.size() << " for " << (trait->num_bound_vars()-1) << "\n";

    return trait_error();
}

Type TypeSema::instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<const ASTType*> var_instances) {
    if (var_instances.size() == type->num_bound_vars()) {
        std::vector<Type> inst_types;
        for (auto t : var_instances) inst_types.push_back(check(t));
        SpecializeMapping mapping = create_spec_mapping(type, inst_types);

        check_bounds(loc, type, inst_types, mapping);
        return type->instantiate(mapping);
    } else {
        type->dump();
        error(loc) << "wrong number of instances for bound type variables: " << var_instances.size() << " for " << type->num_bound_vars() << "\n";
    }

    return type_error();
}

Type TypeSema::instantiate(const ASTNode* loc, Type type, thorin::ArrayRef<Type> var_instances) {
    assert(var_instances.size() == type->num_bound_vars());
    SpecializeMapping mapping = create_spec_mapping(type, var_instances);
    check_bounds(loc, type, var_instances, mapping);
    return type->instantiate(mapping);
}

//------------------------------------------------------------------------------

/*
 * ASTType::check
 */

void TypeParamList::check_type_params(TypeSema& sema) const {
    // check bounds
    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
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
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.primtype(PrimType_##itype);
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

    return sema.tupletype(types);
}

Type FnASTType::check(TypeSema& sema) const {
    check_type_params(sema);

    std::vector<Type> params;
    for (auto elem : elems())
        params.push_back(sema.check(elem));

    FnType fntype = sema.fntype(params);
    for (auto type_param : type_params())
        fntype->add_bound_var(type_param->type_var(sema));

    return fntype;
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

Type ValueDecl::check(TypeSema& sema, Type expected) const {
    if (ast_type()) {
        return sema.check(ast_type());
    } else {
        return expected;
    }
}

Trait TraitDecl::to_trait(TypeSema& sema) const {
    check(sema);
    return trait();
}

//------------------------------------------------------------------------------

/*
 * TypeDecl
 */

Type TypeParam::check(TypeSema& sema) const { return sema.typevar(symbol()); }

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
        sema.check(item);
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

/*
 * MiscItem
 */

Type FnDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    std::vector<Type> types;
    for (auto param : params())
        types.push_back(sema.check(param));

    // create FnType
    Type fn_type = sema.fntype(types);
    for (auto tp : type_params())
        fn_type->add_bound_var(tp->type_var(sema));
    type_ = fn_type;
    sema.unify(type_);

    if (body() != nullptr)
        sema.check_body(this, body(), fn_type);

    type_.clear(); // will be set again by TypeSema's wrapper
    return fn_type;
}

Type StaticItem::check(TypeSema& sema) const {
    return Type();
}

void TraitDecl::check(TypeSema& sema) const {
    // did we already check this trait?
    if (!trait().empty())
        return;

    TypeVar self_var = self_param()->type_var(sema);
    trait_ = sema.trait(this);
    trait_->add_bound_var(self_var);

    check_type_params(sema);
    for (auto tp : type_params())
        trait_->add_bound_var(tp->type_var(sema));

    for (const ASTTypeApp* t : super())
        trait_->add_super_trait(t->to_trait(sema, self_var));

    // check methods
    for (auto m : methods()) {
        if (!trait_->add_method(m->symbol(), sema.check(m)))
            sema.error(m) << "a method with this name already exists in a super trait.\n";
    }

    sema.unify(trait());
}

void Impl::check(TypeSema& sema) const {
    check_type_params(sema);
    Type ftype = sema.check(for_type());

    Trait tinst;
    if (trait() != nullptr) {
        if (auto t = trait()->isa<ASTTypeApp>()) {
            // create impl
            tinst = t->to_trait(sema, ftype);
            TraitImpl impl = sema.implement_trait(this, tinst);
            for (auto tp : type_params()) {
                impl->add_bound_var(tp->type_var(sema));
            }

            // add impl to type
            if ((ftype != sema.type_error()) && (tinst != sema.trait_error()))
                ftype->add_implementation(impl);
        } else
            sema.error(trait()) << "expected trait instance.\n";
    }

    thorin::HashSet<Symbol> implemented_methods;
    for (auto fn : methods()) {
        Type fntype = sema.check(fn);

        if (trait() != nullptr) {
            assert(!tinst.empty());

            Symbol meth_name = fn->symbol();
            Type t = tinst->find_method(meth_name);
            if (!t.empty()) {
                // remember name for check if all methods were implemented
                auto p = implemented_methods.insert(meth_name);
                assert(p.second && "There should be no such name in the set"); // else name analysis failed

                // check that the types match
                sema.match_types(fn, fntype, t);
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
    return sema.primtype(literal2type());
}

Type FnExpr::check(TypeSema& sema, Type expected) const {
    assert(type_params().empty());

    std::vector<Type> par_types;
    if (FnType exp_fn = expected.isa<FnType>()) {
        if (exp_fn->size() != params().size())
            sema.error(this) << "expected function with " << exp_fn->size() << " parameters, but found lambda expression with " << params().size() << " parameters.\n";

        size_t i = 0;
        for (auto param : params())
            par_types.push_back(sema.check(param, exp_fn->elem(i++)));
    } else {
        for (auto param : params())
            par_types.push_back(sema.check(param));
    }

    // create FnType
    Type fn_type = sema.fntype(par_types);
    sema.unify(fn_type);

    assert(body() != nullptr);
    sema.check_body(this, body(), fn_type);

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
    Type lhstype;
    Type rhstype;
    // FEATURE other cases
    switch (kind()) {
        case EQ:
        case NE:
            lhstype = sema.check(lhs());
            rhstype = sema.check(rhs());
            sema.match_types(this, lhstype, rhstype);
            return sema.type_bool();
        case LT:
        case LE:
        case GT:
        case GE:
            lhstype = sema.check(lhs());
            rhstype = sema.check(rhs());
            sema.expect_num(lhs());
            sema.match_types(this, lhstype, rhstype);
            return sema.type_bool();
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case REM:
            lhstype = sema.check(lhs(), expected);
            rhstype = sema.check(rhs(), expected);
            sema.expect_num(lhs());
            return lhstype;
        case ASGN:
            lhstype = sema.check(lhs(), expected);
            rhstype = sema.check(rhs(), expected);
            return lhstype;
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
    for (Trait t : lhs()->type()->trait_impls()) {
        Type fn = t->find_method(path_elem()->symbol());
        if (!fn.empty()) {
            if (fn != sema.type_error()) {
                FnType func;
                if (!path_elem()->args().empty()) {
                    Type t = sema.instantiate(path_elem(), fn, path_elem()->args());
                    sema.unify(t);
                    func = t.as<FnType>();
                } else
                    func = fn.as<FnType>();

                // there should at least be two arguments: the continuation and the self object
                if (func->size() > 1) {
                    sema.expect_type(lhs(), func->elem(0), "object");
                    return func->specialize_method(lhs()->type());
                } else
                    sema.error(this) << "cannot call a method without any arguments";
            }
            return sema.type_error();
        }
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
    return sema.tupletype(types);
}

Type StructExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type MapExpr::check(TypeSema& sema, Type expected) const {
    Type lhst = sema.check(lhs());
    if (auto fn = lhst.isa<FnType>()) {
        bool was_generic = fn->is_generic();
        std::vector<Type> inst_types;
        if (was_generic)
            fn = sema.instantiate_unknown(lhst, inst_types).as<FnType>();

        bool no_cont = fn->size() == (args().size()+1); // true if this is a normal function call (no continuation)
        if (no_cont || (fn->size() == args().size())) {
            for (size_t i = 0; i < args().size(); ++i) {
                sema.check(arg(i), fn->elem(i), "argument");
            }

            // instantiate fn type
            if (was_generic) {
                for (Type t : inst_types) {
                    UninstantiatedType ut = t.as<UninstantiatedType>();
                    if (ut->is_instantiated())
                        lhs()->add_inferred_arg(ut->instance());
                }
                if (lhs()->inferred_args().size() == fn->num_bound_vars()) {
                    // TODO where should be set this new type? should we set it at all?
                } else
                    sema.error(this) << "could not find instances for all type variables.\n";
            }

            if (no_cont) // return type
                return sema.create_return_type(this, fn->elems().back()); 
            else        // same number of args as params -> continuation call
                return sema.type_noreturn();
        } else
            sema.error(this) << "wrong number of arguments\n";
    } else if (!lhs()->type().isa<TypeError>()) {
        // REMINDER new error message if not only fn-types are allowed
        sema.error(lhs()) << "expected function type but found " << lhs()->type() << "\n";
    }

    return sema.type_error();
}

Type IfExpr::check(TypeSema& sema, Type expected) const {
    sema.check(cond(), sema.type_bool(), "condition");
    Type then_type = sema.check(then_expr(), expected);
    Type else_type = sema.check(else_expr(), expected);
    if (then_type == sema.type_error() || else_type == sema.type_error())
        return sema.type_error();
    return then_type;
}

Type ForExpr::check(TypeSema& sema, Type expected) const {
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
    sema.check(item());
}

void LetStmt::check(TypeSema& sema) const {
    Type expected = sema.check(local());
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
