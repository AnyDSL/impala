#include "thorin/util/push.h"

#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/impala.h"
#include "impala/sema/typetable.h"

namespace impala {

//------------------------------------------------------------------------------

class TypeSema : public TypeTable {
public:
    TypeSema(const bool nossa)
        : nossa_(nossa)
    {}

    bool nossa() const { return nossa_; }
    void push_impl(const ImplItem* i) { impls_.push_back(i); }
    void check_impls() {
        while (!impls_.empty()) {
            const ImplItem* i = impls_.back();
            impls_.pop_back();
            check_item(i);
        }
    }

    // error handling

    bool expect_lvalue(const Expr* expr) {
        if (!expr->is_lvalue()) {
            error(expr) << "lvalue required in assignment\n";
            return  false;
        }
        return true;
    }
    bool expect_int(const Expr*);
    void expect_num(const Expr*);
    Type match_types(const ASTNode* pos, Type t1, Type t2);
    Type expect_type(const Expr* expr, Type found, Type expected, std::string what);
    Type expect_type(const Expr* expr, Type expected, std::string what) { return expect_type(expr, expr->type(), expected, what); }

    Bound instantiate(const Location& loc, Trait trait, Type self, ArrayRef<const ASTType*> args);
    Type instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args);
    Type check_call(const Location& loc, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected);

    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types, SpecializeMap& map);
    bool check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> types) {
        SpecializeMap map;
        return check_bounds(loc, unifiable, types, map);
    }

    // check wrappers

    Type check(const TypeableDecl* decl) {
        if (!decl->checked_) { 
            decl->checked_ = true; 
            decl->type_ = decl->check(*this);
        }
        return decl->type();
    }
    Type check(const ValueDecl* decl, Type expected) {
        if (!decl->checked_) {
            decl->checked_ = true;
            decl->type_ = decl->check(*this, expected);
        }
        return decl->type();
    }
    void check_item(const Item* item) { item->check_item(*this); }
    Type check(const Expr* expr, Type expected, std::string what) {
        if (!expr->type_.empty())
            return expr->type_;
        return expr->type_ = expect_type(expr, expr->check(*this, expected), expected, what);
    }
    Type check(const Expr* expr, Type expected) { return check(expr, expected, ""); }
    /// a check that does not expect any type (i.e. any type is allowed)
    Type check(const Expr* expr) { return check(expr, unknown_type()); }
    Type check(const ASTType* ast_type) { return ast_type->type_ = ast_type->check(*this); }

private:
    bool nossa_;
    std::vector<const ImplItem*> impls_;

public:
    const BlockExpr* cur_block_expr_ = nullptr;
    const Fn* cur_fn_ = nullptr;
};

//------------------------------------------------------------------------------

// TODO factor code with expect_num
// TODO maybe have variant which also checks expr
bool TypeSema::expect_int(const Expr* expr) {
    Type t = expr->type();

    if (!t->is_error() && !t->is_i8() && !t->is_i16() && !t->is_i32() && !t->is_i64()) { // TODO factor this test out
        error(expr) << "expected integer type but found " << t << "\n";
        return false;
    }
    return true;
}

void TypeSema::expect_num(const Expr* expr) {
    Type t = expr->type();

    if (!t->is_error() && !t->is_i8() && !t->is_i16() && !t->is_i32() && !t->is_i64() && !t->is_f32() && !t->is_f64()) // TODO factor this test out
        error(expr) << "expected number type but found " << t << "\n";
}

Type TypeSema::match_types(const ASTNode* pos, Type t1, Type t2) {
    if (t1 == t2) {
        return t1;
    } else {
        error(pos) << "types do not match: " << t1 << " != " << t2 << "\n";
        return type_error();
    }
}

Type TypeSema::expect_type(const Expr* expr, Type found_type, Type expected, std::string what) {
    if (found_type == expected)
        return expected;

    // TODO: quick hack
    if (auto ptr = found_type.isa<OwnedPtrType>()) {
        if (expected.isa<PtrType>()) 
            return expected;
    }

    if (found_type->is_polymorphic()) { // try to infer instantiations for this polymorphic type
        std::vector<Type> type_args;
        Type inst = instantiate_unknown(found_type, type_args);
        if (inst == expected) {
            check_bounds(expr->loc(), *found_type, type_args);
            return expected;
        }
    }
    error(expr) << "wrong " << what << " type; expected " << expected << " but found " << found_type << "\n";
    return expected;
}

Bound TypeSema::instantiate(const Location& loc, Trait trait, Type self, ArrayRef<const ASTType*> args) {
    if ((args.size()+1) == trait->num_type_vars()) {
        std::vector<Type> type_args;
        type_args.push_back(self);
        for (auto t : args) 
            type_args.push_back(check(t));
        check_bounds(loc, *trait, type_args);
        return trait->instantiate(type_args);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << args.size() << " for " << (trait->num_type_vars()-1) << "\n";

    return bound_error();
}

Type TypeSema::instantiate(const Location& loc, Type type, ArrayRef<const ASTType*> args) {
    if (args.size() == type->num_type_vars()) {
        std::vector<Type> type_args;
        for (auto t : args) 
            type_args.push_back(check(t));

        SpecializeMap map;
        check_bounds(loc, *type, type_args, map);
        return type->instantiate(map);
    } else
        error(loc) << "wrong number of instances for bound type variables: " << args.size() << " for " << type->num_type_vars() << "\n";

    return type_error();
}

bool TypeSema::check_bounds(const Location& loc, Uni unifiable, ArrayRef<Type> type_args, SpecializeMap& map) {
    map = specialize_map(unifiable, type_args);
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
        for (auto bound : type_param->bounds()) {
            if (auto type_app = bound->isa<ASTTypeApp>()) {
                auto type_var = type_param->type_var(sema);
                type_var->add_bound(type_app->bound(sema, type_var));
            } else {
                sema.error(type_param) << "bounds must be trait instances, not types\n";
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
    if (is_owned())
        return sema.owned_ptr_type(type);
    if (is_borrowed())
        return sema.borrowd_ptr_type(type);
    assert(false && "only owned and borrowed ptrs are supported");
    return Type();
}

Type IndefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type DefiniteArrayASTType::check(TypeSema& sema) const {
    return sema.definite_array_type(sema.check(elem_type()), dim());
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

Bound ASTTypeApp::bound(TypeSema& sema, Type self) const {
    if (decl()) {
        if (auto trait_decl = decl()->isa<TraitDecl>()) {
            sema.check_item(trait_decl);
            return sema.instantiate(this->loc(), trait_decl->trait(), self, elems());
        } else
            sema.error(this) << '\'' << symbol() << "' does not name a trait\n";
    }
    return sema.bound_error();
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
            sema.error(this) << "could not infer types: expected '" << expected << "' but found '" << t << "'.\n";
            return sema.type_error();
        }
    } else if (expected.empty()) {
        sema.error(this) << "could not infer parameter type for " << this << ".\n";
        return sema.type_error();
    } else {
        return expected;
    }
}

void Fn::check_body(TypeSema& sema, FnType fn_type) const {
    auto return_type = fn_type->return_type();
    Type body_type = sema.check(body(), return_type);
    if (!body_type->is_noret() && !body_type->is_error())
        sema.expect_type(body(), return_type, "return");

    for (auto param : params()) {
        if (param->is_mut() && !param->is_written())
            sema.warn(param) << "parameter '" << param->symbol() << "' declared mutable but parameter is never written to\n";
    }
}

//------------------------------------------------------------------------------

/*
 * items
 */

void TypeDeclItem::check_item(TypeSema& sema) const { sema.check(static_cast<const TypeDecl*>(this)); }
void ValueItem::check_item(TypeSema& sema) const { sema.check(static_cast<const ValueDecl*>(this)); }

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
        sema.check_item(item);
}

void ExternBlock::check_item(TypeSema& sema) const {
    if (!abi().empty())
        if (abi() != Symbol("\"C\"") && abi() != Symbol("\"device\"") && abi() != Symbol("\"thorin\""))
            sema.error(this) << "unknown extern specification.\n";  // TODO: better location
    for (auto fn : fns())
        sema.check(fn);
}

Type Typedef::check(TypeSema& sema) const {
    return Type();
}

Type EnumDecl::check(TypeSema& sema) const {
    return Type();
}

Type StructDecl::check(TypeSema& sema) const {
    check_type_params(sema);
    auto struct_type = sema.struct_type(this);
    for (auto field : fields())
        sema.check(field);
    return struct_type;
}

Type FieldDecl::check(TypeSema&) const {
    return Type();
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

    for (auto type_app : super_traits()) {
        if (!trait_->add_super_bound(type_app->bound(sema, self_var)))
            sema.error(type_app) << "duplicate super trait '" << type_app << "' for trait '" << symbol() << "'\n";
    }

    for (auto m : methods())
        sema.check(m);
}

void ImplItem::check_item(TypeSema& sema) const {
    check_type_params(sema);
    Type for_type = sema.check(this->type());

    Bound bound;
    if (trait() != nullptr) {
        if (auto type_app = trait()->isa<ASTTypeApp>()) {
            bound = type_app->bound(sema, for_type);
            auto impl = sema.impl(this, bound, for_type);
            for (auto tp : type_params())
                impl->bind(tp->type_var(sema));

            if (!for_type->is_error() && !bound->is_error()) {
                for_type.as<KnownType>()->add_impl(impl);
                bound->trait()->add_impl(impl);
            }
        } else
            sema.error(trait()) << "expected trait instance.\n";
    }

    thorin::HashSet<Symbol> implemented_methods;
    for (auto fn : methods()) {
        Type fn_type = sema.check(fn);

        if (trait() != nullptr) {
            assert(!bound.empty());

            Symbol meth_name = fn->symbol();
            Type t = bound->find_method(meth_name);
            if (!t.empty()) {
                // remember name for check if all methods were implemented
                auto p = implemented_methods.insert(meth_name);
                assert(p.second && "There should be no such name in the set"); // else name analysis failed

                // check that the types match
                sema.match_types(fn, fn_type, t);
            }
        }
    }

    // TODO
#if 0
    // check that all methods are implemented
    if (!bound.empty()) {
        if (implemented_methods.size() != bound->num_methods()) {
            assert(implemented_methods.size() < bound->num_methods());
            for (auto p : bound->all_methods()) {
                if (!implemented_methods.contains(p.first))
                    sema.error(this) << "Must implement method '" << p.first << "'\n";
            }
        }
    }
#endif
}

//------------------------------------------------------------------------------

/*
 * expressions
 */

Type EmptyExpr::check(TypeSema& sema, Type) const { return sema.unit(); }

Type BlockExpr::check(TypeSema& sema, Type expected) const {
    THORIN_PUSH(sema.cur_block_expr_, this);
    for (auto stmt : stmts())
        stmt->check(sema);

    sema.check(expr(), expected);

    for (auto local : locals_) {
        if (local->is_mut() && !local->is_written())
            sema.warn(local) << "variable '" << local->symbol() << "' declared mutable but variable is never written to\n";
    }

    return expr() ? expr()->type() : sema.unit().as<Type>();
}

Type LiteralExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE we could enhance this using the expected type (e.g. 4 could be interpreted as int8 if needed)
    return sema.type(literal2type());
}

Type FnExpr::check(TypeSema& sema, Type expected) const {
    THORIN_PUSH(sema.cur_fn_, this);
    assert(type_params().empty());

    FnType fn_type;
    if (FnType exp_fn = expected.isa<FnType>()) {
        if (exp_fn->num_elems() == num_params()+1) // add return param to infer type
            const_cast<FnExpr*>(this)->params_.push_back(Param::create(ret_var_handle_, "return", body()->pos1(), nullptr));
        else if (exp_fn->num_elems() != num_params())
            sema.error(this) << "expected function with " << exp_fn->num_elems() << " parameters, but found lambda expression with " << num_params() << " parameters\n";

        size_t i = 0;
        for (auto param : params())
            sema.check(param, exp_fn->elem(i++));

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

Type PathExpr::check(TypeSema& sema, Type expected) const {
    // FEATURE consider longer paths
    //auto* last = path()->path_elems().back();
    if (value_decl()) {
        if (auto local = value_decl()->isa<LocalDecl>()) {
            if (local->is_mut() && local->fn() != sema.cur_fn_)
                local->take_address();
        }
        return sema.check(value_decl());
    }
    return sema.type_error();
}

Type PrefixExpr::check(TypeSema& sema, Type expected) const {
    // TODO check if operator supports the type
    auto rtype = sema.check(rhs());
    switch (kind()) {
        case AND:
            rhs()->take_address();
            return sema.borrowd_ptr_type(rtype);
        case TILDE:
            return sema.owned_ptr_type(rtype);
        case MUL:
            if (auto ptr = rtype.isa<PtrType>())
                return ptr->referenced_type();
        case INC:
        case DEC:
            sema.expect_num(rhs());
            sema.expect_lvalue(rhs());
            return rtype;
        case ADD:
        case SUB:
            sema.expect_num(rhs());
            return rtype;
        case RUN:
        case HLT:
            return sema.check(rhs());
        default:
            THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

Type InfixExpr::check(TypeSema& sema, Type expected) const {
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
            auto type = sema.check(lhs(), sema.check(rhs()));
            sema.expect_num(lhs());
            sema.expect_num(rhs());
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
        case REM_ASGN:
        case AND_ASGN:
        case  OR_ASGN:
        case XOR_ASGN:
        case SHL_ASGN:
        case SHR_ASGN: {
            // TODO handle floats etc
            sema.check(rhs(), sema.check(lhs()));
            if (sema.expect_lvalue(lhs())) {
                sema.expect_num(lhs());
                sema.expect_num(rhs());
                return sema.unit();
            }
            break;
        }
        default: THORIN_UNREACHABLE;
    }

    return sema.type_error();
}

Type PostfixExpr::check(TypeSema& sema, Type expected) const {
    // TODO check if operator supports the type
    sema.check(lhs(), expected);
    sema.expect_lvalue(lhs());
    return lhs()->type(); 
}

Type FieldExpr::check(TypeSema& sema, Type expected) const {
    return sema.check(lhs());
}

Type CastExpr::check(TypeSema& sema, Type expected) const {
    // TODO check whether cast is possible at all
    sema.check(lhs());
    return sema.check(ast_type());
}

Type DefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    Type elem_type = sema.unknown_type();
    for (auto arg : args())
        sema.expect_type(arg, sema.check(arg), elem_type, "element of definite array expression");
    return sema.definite_array_type(elem_type, num_args());
}

Type RepeatedDefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    return Type(); // TODO
}

Type IndefiniteArrayExpr::check(TypeSema& sema, Type expected) const {
    sema.check(dim());
    sema.expect_int(dim());
    return sema.indefinite_array_type(sema.check(elem_type()));
}

Type TupleExpr::check(TypeSema& sema, Type expected) const {
    std::vector<Type> types;
    if (auto exp_tup = expected.isa<TupleType>()) {
        if (exp_tup->num_elems() != num_args())
            sema.error(this) << "expected tuple with " << exp_tup->num_elems() << " elements, but found tuple expression with " << num_args() << " elements.\n";

        size_t i = 0;
        for (auto arg : args()) {
            sema.check(arg, exp_tup->elem(i++));
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

Type StructExpr::check(TypeSema& sema, Type expected) const {
    return Type();
}

Type TypeSema::check_call(const Location& loc, FnType fn_poly, const ASTTypes& type_args, std::vector<Type>& inferred_args, ArrayRef<const Expr*> args, Type expected) {
    size_t num_type_args = type_args.size();
    size_t num_args = args.size();

    if (num_type_args <= fn_poly->num_type_vars()) {
        for (auto type_arg : type_args)
            inferred_args.push_back(check(type_arg));

        for (size_t i = num_type_args, e = fn_poly->num_type_vars(); i != e; ++i)
            inferred_args.push_back(unknown_type());

        assert(inferred_args.size() == fn_poly->num_type_vars());
        auto fn_mono = fn_poly->instantiate(inferred_args).as<FnType>();

        bool is_contuation = num_args == fn_mono->num_elems();
        if (is_contuation || num_args+1 == fn_mono->num_elems()) {
            for (size_t i = 0; i != num_args; ++i)
                check(args[i], fn_mono->elem(i), "argument");

            if (fn_mono->return_type() == expected || is_contuation) { // TODO this looks overly complicated
                bool is_known = true;
                for (size_t i = 0, e = inferred_args.size(); i != e; ++i) {
                    if (!inferred_args[i]->is_known()) {
                        is_known = false;
                        error(loc) << "could not find instance for type variable #" << i << ".\n";
                    }
                }

                if (is_known) {
                    check_bounds(loc, fn_poly, inferred_args);
                    return expected;
                }
            } else
                error(loc) << "cannot match return type\n";
        } else
            error(loc) << "wrong number of arguments\n";
    } else
        error(loc) << "too many type arguments to function\n";

    return type_error();
}

Type MapExpr::check(TypeSema& sema, Type expected) const {
    auto ltype = sema.check(lhs());
    if (auto ptr = ltype.isa<PtrType>()) {
        ltype.clear();
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto field_expr = is_method_call()) {
        sema.check_impls();
        if (auto fn_method = sema.check(field_expr->lhs())->find_method(field_expr->symbol())) {
            Array<const Expr*> nargs(num_args() + 1);
            nargs[0] = field_expr->lhs();
            std::copy(args().begin(), args().end(), nargs.begin()+1);
            return sema.check_call(this->loc(), fn_method, type_args(), inferred_args_, nargs, expected);
        } else
            sema.error(this) << "no declaration for method '" << field_expr->symbol() << "' found.\n";
    } else if (auto fn_poly = ltype.isa<FnType>()) {
        return sema.check_call(this->loc(), fn_poly, type_args(), inferred_args_, args(), expected);
    } else if (auto array = ltype.isa<ArrayType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0)))
                return array->elem_type();
            else
                sema.error(this) << "require integer as array subscript\n";
        } else
            sema.error(this) << "too many array subscripts\n";
    } else if (auto exp_tup = ltype.isa<TupleType>()) {
        if (num_args() == 1) {
            sema.check(arg(0));
            if (sema.expect_int(arg(0))) {
                if (auto lit = arg(0)->isa<LiteralExpr>())
                    return exp_tup->elem(lit->get_u64());
                else
                    sema.error(this) << "require literal as tuple subscript\n";
            } else
                sema.error(this) << "require integer as tuple subscript\n";
        } else
            sema.error(this) << "too many tuple subscripts\n";
    } else
        sema.error(this) << "incorrect type for map expression\n";

    return sema.type_error();
}

Type ForExpr::check(TypeSema& sema, Type expected) const {
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();
    if (auto map = forexpr->isa<MapExpr>()) {
        Type lhst = sema.check(map->lhs());

        if (auto fn_for = lhst.isa<FnType>()) {
            Array<const Expr*> args(map->args().size()+1);
            *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
            return sema.check_call(map->loc(), fn_for, map->type_args(), map->inferred_args_, args, expected);
        }
    } else if (auto field_expr = forexpr->isa<FieldExpr>()) {
        assert(false && field_expr && "TODO");
    }

    sema.error(expr()) << "the looping expression does not support the 'for' protocol\n";
    return sema.unit();
}

Type IfExpr::check(TypeSema& sema, Type expected) const {
    sema.check(cond(), sema.type_bool(), "condition");
    Type then_type = sema.check(then_expr(), sema.unknown_type());
    Type else_type = sema.check(else_expr(), sema.unknown_type());

    if (then_type->is_noret() && else_type->is_noret())
        return sema.type_noret();
    if (then_type->is_noret())
        return sema.expect_type(else_expr(), else_type, expected, "if expression");
    if (else_type->is_noret())
        return sema.expect_type(then_expr(), then_type, expected, "if expression");
    if (then_type == else_type)
        return sema.expect_type(this, then_type, expected, "if expression");

    sema.error(this) << "different types in arms of an if expression\n";
    sema.error(then_expr()) << "type of the consequence is '" << then_type << "'\n";
    sema.error(else_expr()) << "type of the alternative is '" << else_type << "'\n";
    return sema.type_error();
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(TypeSema& sema) const {
    if (sema.check(expr())->is_noret())
        sema.error(expr()) << "expression does not return rendering subsequent statements unreachable\n";
}

void ItemStmt::check(TypeSema& sema) const {
    sema.check_item(item());
}

void LetStmt::check(TypeSema& sema) const {
    sema.cur_block_expr_->add_local(local());
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
