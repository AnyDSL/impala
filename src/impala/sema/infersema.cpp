#include <sstream>

#include "thorin/util/array.h"
#include "thorin/util/iterator.h"
#include "thorin/util/log.h"
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

    const Type* reduce(const Lambda* lambda, ArrayRef<const ASTType*> ast_type_args, std::vector<const Type*>& type_args);
    void fill_type_args(std::vector<const Type*>& type_args, const ASTTypes& ast_type_args);
    const Type* close(int num_lambdas, const Type* body);
    size_t num_lambdas(const Lambda* lambda);
    const Type* expr2expected(const Expr* expr);
    const Type* arg(const Type* type, size_t i) { return i < type->size() ? type->arg(i) : type_error(); }

    template<class T>
    const Type* arg(const Expr* expr, const Type* expected, size_t i) {
        if (auto type = expected->template isa<T>())
            return arg(type, i);
        return expr2expected(expr);
    }

    // unification related stuff

    /**
     * Gets the representative of @p type.
     * Initializes @p type with @p UnknownType if @p type is @c nullptr.
     * Updates @p todo_ if something changed.
     */
    const Type* type(const Type*& type);
    const Type* type(const Typeable* typeable) { return type(typeable->type_); }

    /**
     * @c unify(t, u).
     * Initializes @p t with @p UnknownType if @p type is @c nullptr.
     * Updates @p todo_ if something changed.
     */
    const   Type*& constrain(const    Type*& t, const   Type* u);
    const   Type*& constrain(const    Type*& t, const   Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const   Type*& constrain(const Typeable* t, const   Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const   Type*& constrain(const Typeable* t, const   Type* u)                { return constrain(t->type_, u); }
    const FnType*& constrain(const  FnType*& t, const FnType* u) { return (const FnType*&) constrain((const Type*&)t, (const Type*)u); }

    /// Unifies @p t and @p u. Does @attention { not } update @p todo_.
    const Type* unify(const Type* t, const Type* u);

    // check wrappers

    void check(const ModContents* n) { n->check(*this); }
    const Type* check(const LocalDecl* local) {
        auto type = local->check(*this);
        if (type->is_hashed())
            constrain(local, type);
        return type;
    }
    const Type* check(const FieldDecl* f) { return constrain(f, f->check(*this)); }
    void check(const Item* n) { n->check(*this); }
    void check(const Stmt* n) { n->check(*this); }
    const Type* check(const Expr* expr, const Type* expected) { return constrain(expr, expr->check(*this, expected)); }
    const Type* check(const Expr* expr) { return constrain(expr, expr->check(*this, expr2expected(expr))); }

    const Var* check(const ASTTypeParam* ast_type_param) {
        if (!ast_type_param->type())
            ast_type_param->type_ = ast_type_param->check(*this);
        return ast_type_param->type()->as<Var>();
    }

    const Type* check(const ASTType* ast_type) {
        if (ast_type->type() && ast_type->type()->is_known())
            return ast_type->type();
        return constrain(ast_type, ast_type->check(*this));
    }

    const Type* check_call(const FnType* fn_type, ArrayRef<const Expr*> args, const Type* expected);
    const Type* check_call(const FnType* fn_type, const std::deque<AutoPtr<const Expr>>& args, const Type* expected) {
        Array<const Expr*> array(args.begin(), args.end());
        return check_call(fn_type, array, expected);
    }

private:
    /// Used for union/find - see https://en.wikipedia.org/wiki/Disjoint-set_data_structure#Disjoint-set_forests .
    struct Representative {
        Representative() {}
        Representative(const Type* type)
            : parent(this)
            , type(type)
        {}

        bool is_root() const { return parent != nullptr; }

        Representative* parent = nullptr;
        const Type* type = nullptr;
        int rank = 0;
    };

    Representative* representative(const Type* type);
    Representative* find(Representative* repr);
    const Type* find(const Type* type);

    /**
     * @p x will be the new representative.
     * Returns again @p x.
     */
    Representative* unify(Representative* x, Representative* y);

    /**
     * Depending on the rank either @p x or @p y will be the new representative.
     * Returns the new representative.
     */
    Representative* unify_by_rank(Representative* x, Representative* y);

    TypeMap<Representative> representatives_;
    thorin::HashMap<const Expr*, const Type*> expr2expected_;
    GIDMap<Lambda, const Lambda*> old2new_;
    bool todo_ = true;

    friend void type_inference(Init&, const ModContents*);
};

//------------------------------------------------------------------------------

/*
 * helpers
 */

const Type* InferSema::reduce(const Lambda* lambda, ArrayRef<const ASTType*> ast_type_args, std::vector<const Type*>& type_args) {
    auto num = num_lambdas(lambda);
    if (ast_type_args.size() <= num) {
        for (size_t i = 0, e = ast_type_args.size(); i != e; ++i)
            constrain(type_args[i], check(ast_type_args[i]));

        while (type_args.size() < num)
            type_args.push_back(unknown_type());

        size_t i = 0;
        const Type* type = lambda;
        while (auto lambda = type->isa<Lambda>())
            type = application(lambda, type_args[i++]);

        return type;
    }

    return type_error();
}

void InferSema::fill_type_args(std::vector<const Type*>& type_args, const ASTTypes& ast_type_args) {
    for (size_t i = 0, e = type_args.size(); i != e; ++i) {
        if (i < ast_type_args.size())
            constrain(type_args[i], check(ast_type_args[i]));
        else if (!type_args[i])
            type_args[i] = unknown_type();
    }
}

size_t InferSema::num_lambdas(const Lambda* lambda) {
    size_t num = 0;
    while (lambda) {
        lambda = lambda->body()->isa<Lambda>();
        ++num;
    }
    return num;
}

const Type* InferSema::close(int num_lambdas, const Type* body) {
    auto result = body;
    while (num_lambdas-- != 0) {
        result = lambda(result, "TODO");
    }

    return result;
}

const Type* InferSema::expr2expected(const Expr* expr) {
    auto i = expr2expected_.find(expr);
    if (i == expr2expected_.end()) {
        auto type = unknown_type();
        auto p = expr2expected_.emplace(expr, type);
        assert(p.second);
        return type;
    }

    return i->second;
}

//------------------------------------------------------------------------------

/*
 * unification
 */

const Type* InferSema::type(const Type*& type) {
    if (type == nullptr) {
        todo_ = true;
        return type = unknown_type();
    }

    auto otype = type;
    type = find(otype);
    todo_ |= otype != type;
    return type;
}

const Type*& InferSema::constrain(const Type*& t, const Type* u) {
    if (t == nullptr) {
        todo_ = true;
        return t = find(u);
    }

    auto otype = t;
    t = unify(t, u);
    todo_ |= otype != t;
    return t;
}

const Type* InferSema::unify(const Type* t, const Type* u) {
    assert(t->is_hashed() && u->is_hashed());

    auto t_repr = find(representative(t));
    auto u_repr = find(representative(u));
    t = t_repr->type;
    u = u_repr->type;

    // HACK needed as long as we have this stupid tuple problem
    if (auto t_fn = t->isa<FnType>()) {
        if (auto u_fn = u->isa<FnType>()) {
            if (t_fn->empty() && u_fn->size() == 1 && u_fn->arg(0)->isa<UnknownType>()) return unify(t_repr, u_repr)->type;
            if (u_fn->empty() && t_fn->size() == 1 && t_fn->arg(0)->isa<UnknownType>()) return unify(t_repr, u_repr)->type;
        }
    }

    if (t == u)              return t;
    if (t->isa<TypeError>()) return t;
    if (u->isa<TypeError>()) return u;
    if (t->isa<NoRetType>()) return u;
    if (u->isa<NoRetType>()) return t;

    if (t->isa<UnknownType>() && u->isa<UnknownType>())
        return unify_by_rank(t_repr, u_repr)->type;

    if (t->isa<UnknownType>()) return unify(u_repr, t_repr)->type;
    if (u->isa<UnknownType>()) return unify(t_repr, u_repr)->type;

    if (t->kind() == u->kind() && t->size() == u->size()) {
        Array<const Type*> nargs(t->size());
        for (size_t i = 0, e = nargs.size(); i != e; ++i)
            nargs[i] = unify(t->arg(i), u->arg(i));

        return t->rebuild(nargs);
    }

    assert(false && "TODO: this assert is currently only here in order to debug incorrect type error during type inference");
    return type_error();
}

//------------------------------------------------------------------------------

/*
 * union-find
 */

auto InferSema::representative(const Type* type) -> Representative* {
    assert(type->is_hashed());
    auto i = representatives_.find(type);
    if (i == representatives_.end()) {
        auto p = representatives_.emplace(type, type);
        assert_unused(p.second);
        i = p.first;
    }
    return &i->second;
}

auto InferSema::find(Representative* repr) -> Representative* {
    if (repr->parent != repr)
        repr->parent = find(repr->parent);
    return repr->parent;
}

const Type* InferSema::find(const Type* type) {
    if (type->is_hashed())
        return find(representative(type))->type;
    return type;
}

auto InferSema::unify(Representative* x, Representative* y) -> Representative* {
    assert(x->is_root() && y->is_root());

    if (x == y)
        return x;
    ++x->rank;
    return y->parent = x;
}

auto InferSema::unify_by_rank(Representative* x, Representative* y) -> Representative* {
    assert(x->is_root() && y->is_root());

    if (x == y)
        return x;
    if (x->rank < y->rank)
        return x->parent = y;
    else if (x->rank > y->rank)
        return y->parent = x;
    else {
        ++x->rank;
        return y->parent = x;
    }
}

//------------------------------------------------------------------------------

void type_inference(Init& init, const ModContents* mod) {
    auto sema = new InferSema();
    init.typetable = sema;

    int i = 0;
    for (;sema->todo_; ++i) {
        sema->todo_ = false;
        sema->check(mod);
    }

    DLOG("iterations needed for type inference: %", i);
}

//------------------------------------------------------------------------------

/*
 * misc
 */

const Var* ASTTypeParam::check(InferSema& sema) const {
    for (auto bound : bounds())
        sema.check(bound);
    return sema.var(lambda_depth());
}

void ASTTypeParamList::check_ast_type_params(InferSema& sema) const {
    for (auto ast_type_param : ast_type_params())
        sema.check(ast_type_param);
}

const Type* LocalDecl::check(InferSema& sema) const {
    if (ast_type())
        return sema.check(ast_type());
    else if (!type())
        return sema.unknown_type();
    return type();
}

const Type* Fn::check_body(InferSema& sema, const FnType* fn_type) const {
    return sema.check(body(), fn_type->return_type());
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

const Type* ErrorASTType::check(InferSema& sema) const { return sema.type_error(); }

const Type* PrimASTType::check(InferSema& sema) const {
    switch (kind()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.prim_type(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const Type* PtrASTType::check(InferSema& sema) const {
    auto referenced_type = sema.check(referenced_ast_type());
    switch (kind()) {
        case Borrowed: return sema.borrowed_ptr_type(referenced_type, addr_space());
        case Mut:      return sema.     mut_ptr_type(referenced_type, addr_space());
        case Owned:    return sema.   owned_ptr_type(referenced_type, addr_space());
    }
    THORIN_UNREACHABLE;
}

const Type* IndefiniteArrayASTType::check(InferSema& sema) const { return sema.indefinite_array_type(sema.check(elem_ast_type())); }
const Type* DefiniteArrayASTType::check(InferSema& sema) const { return sema.definite_array_type(sema.check(elem_ast_type()), dim()); }
const Type* SimdASTType::check(InferSema& sema) const { return sema.simd_type(sema.check(elem_ast_type()), size()); }

const Type* TupleASTType::check(InferSema& sema) const {
    Array<const Type*> types(num_ast_type_args());
    for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
        types[i] = sema.check(ast_type_arg(i));

    return sema.tuple_type(types);
}

const Type* FnASTType::check(InferSema& sema) const {
    check_ast_type_params(sema);

    Array<const Type*> types(num_ast_type_args());
    for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
        types[i] = sema.check(ast_type_arg(i));

    return sema.close(num_ast_type_params(), sema.fn_type(types));
}

const Type* Typeof::check(InferSema& sema) const { return sema.check(expr()); }

const Type* ASTTypeApp::check(InferSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>()) {
            if (auto ast_type_param = type_decl->isa<ASTTypeParam>())
                return sema.var(ast_type_param->lambda_depth_);
            if (auto type = sema.type(type_decl)) {
                if (auto lambda = type->isa<Lambda>())
                    return sema.reduce(lambda, ast_type_args(), type_args_);
                return type;
            }
        }
    }

    return sema.type_error();
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
    check_ast_type_params(sema);
    sema.close(num_ast_type_params(), sema.check(ast_type()));
#if 0

    if (ast_type_params().size() > 0) {
        auto abs = sema.typedef_abs(sema.type(ast_type())); // TODO might be nullptr
        for (auto lambda : lambdas())
            abs->bind(lambda->lambda());
    } else
        sema.constrain(this, sema.type(ast_type()));
#endif
}

void EnumDecl::check(InferSema&) const { /*TODO*/ }

void StructDecl::check(InferSema& sema) const {
    if (type_ && type_->is_known())
        return;

    auto struct_type = sema.struct_type(this, num_field_decls());
    check_ast_type_params(sema);

    for (size_t i = 0, e = num_field_decls(); i != e; ++i)
        struct_type->set(i, sema.check(field_decl(i)));

    sema.constrain(this, struct_type);
}

const Type* FieldDecl::check(InferSema& sema) const { return sema.check(ast_type()); }

void FnDecl::check(InferSema& sema) const {
    check_ast_type_params(sema);

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.check(param(i));

    sema.constrain(this, sema.close(num_ast_type_params(), sema.fn_type(param_types)));

    if (body() != nullptr)
        check_body(sema, fn_type());
}

void StaticItem::check(InferSema& sema) const {
    if (init())
        sema.constrain(this, sema.check(init()));
}

void TraitDecl::check(InferSema& /*sema*/) const {}
void ImplItem::check(InferSema& /*sema*/) const {}


//------------------------------------------------------------------------------

/*
 * expressions
 */

const Type* EmptyExpr::check(InferSema& sema, const Type*) const { return sema.unit(); }
const Type* LiteralExpr::check(InferSema& sema, const Type*) const { return sema.prim_type(literal2type()); }
const Type* CharExpr::check(InferSema& sema, const Type*) const { return sema.type_u8(); }
const Type* StrExpr::check(InferSema& sema, const Type*) const { return sema.definite_array_type(sema.type_u8(), values_.size()); }

const Type* FnExpr::check(InferSema& sema, const Type* expected) const {
    assert(ast_type_params().empty());

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.constrain(param(i), sema.arg<FnType>(this, expected, i));

    auto fn_type = sema.fn_type(param_types);
    assert(body() != nullptr);
    check_body(sema, fn_type);
    return fn_type;
}

const Type* PathExpr::check(InferSema& sema, const Type* expected) const {
    if (value_decl())
        return sema.constrain(value_decl(), expected);
    return sema.type_error();
}

const Type* PrefixExpr::check(InferSema& sema, const Type* expected) const {
    switch (kind()) {
        case AND: {
            auto expected_referenced_type = sema.arg<PtrType>(this, expected, 0);
            auto rtype = sema.check(rhs(), expected_referenced_type);
            return sema.borrowed_ptr_type(rtype, 0);
        }
        case TILDE:
            return sema.owned_ptr_type(sema.check(rhs(), sema.arg<PtrType>(this, expected, 0)));
        case MUL: {
            auto type = sema.check(rhs(), sema.borrowed_ptr_type(expected));
            if (auto ptr_type = type->isa<PtrType>())
                return ptr_type->referenced_type();
            return type;
        }
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

const Type* InfixExpr::check(InferSema& sema, const Type* expected) const {
    switch (kind()) {
        case EQ: case NE:
        case LT: case LE:
        case GT: case GE:
            sema.check(lhs(), sema.type(rhs()));
            sema.check(rhs(), sema.type(lhs()));
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
            sema.check(lhs(), sema.unify(expected, sema.type(rhs())));
            sema.check(rhs(), sema.type(lhs()));
            return sema.type(rhs());
        case ASGN:
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
        case SHL_ASGN: case SHR_ASGN:
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN:
            sema.check(lhs(), sema.type(rhs()));
            sema.check(rhs(), sema.type(lhs()));
            return sema.unit();
    }

    THORIN_UNREACHABLE;
}

const Type* PostfixExpr::check(InferSema& sema, const Type* expected) const {
    return sema.check(lhs(), expected);
}

const Type* CastExpr::check(InferSema& sema, const Type*) const {
    sema.check(lhs());
    return sema.check(ast_type());
}

const Type* DefiniteArrayExpr::check(InferSema& sema, const Type* expected) const {
    auto expected_elem_type = sema.arg<ArrayType>(this, expected, 0);

    for (const auto& arg : args())
        expected_elem_type = sema.unify(expected_elem_type, sema.type(arg));

    for (const auto& arg : args())
        sema.check(arg, expected_elem_type);

    return sema.definite_array_type(expected_elem_type, num_args());
}

const Type* SimdExpr::check(InferSema& sema, const Type* expected) const {
    auto expected_elem_type = sema.arg<SimdType>(this, expected, 0);

    for (const auto& arg : args())
        expected_elem_type = sema.unify(expected_elem_type, sema.type(arg));

    for (const auto& arg : args())
        sema.check(arg, expected_elem_type);

    return sema.simd_type(expected_elem_type, num_args());
}

const Type* RepeatedDefiniteArrayExpr::check(InferSema& sema, const Type* expected) const {
    auto expected_elem_type = sema.arg<ArrayType>(this, expected, 0);
    return sema.definite_array_type(sema.check(value(), expected_elem_type), count());
}

const Type* IndefiniteArrayExpr::check(InferSema& sema, const Type*) const {
    sema.check(dim());
    return sema.indefinite_array_type(sema.check(elem_ast_type()));
}

const Type* TupleExpr::check(InferSema& sema, const Type* expected) const {
    Array<const Type*> types(num_args());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.check(arg(i), sema.arg<TupleType>(this, expected, i));

    return sema.tuple_type(types);
}

const Type* StructExpr::check(InferSema& sema, const Type* /*expected*/) const {
    for (const auto& elem : elems()) {
        sema.check(elem.expr());
    }
    // TODO use fields to constrain type
    return sema.check(ast_type_app());
}

const Type* InferSema::check_call(const FnType* fn_type, ArrayRef<const Expr*> args, const Type* expected) {
    bool is_returning = args.size()+1 == fn_type->size();

    if (is_returning) {
        Array<const Type*> types(args.size()+1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = type(args[i]);
        types.back() = this->fn_type({expected}); // TODO nullptr check
        fn_type = unify(fn_type, this->fn_type(types))->as<FnType>();
    } else {
        Array<const Type*> types(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = type(args[i]);
        fn_type = unify(fn_type, this->fn_type(types))->as<FnType>();
    }

    auto max_arg_index = std::min(args.size(), fn_type->size());

    for (size_t i = 0; i != max_arg_index; ++i)
        constrain(args[i], fn_type->arg(i));

    for (size_t i = 0; i != args.size(); ++i)
        check(args[i], arg(fn_type, i));

    return fn_type->return_type();
#if 0
    type_args.resize(fn_poly->num_lambdas());
    fill_type_args(type_args, ast_type_args);

    constrain(fn_mono, fn_poly->reduce(type_args)->as<FnType>());

    bool is_returning = args.size()+1 == fn_mono->num_args();

    if (is_returning) {
        Array<const Type*> types(args.size()+1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = type(args[i]);
        types.back() = fn_type({expected}); // TODO nullptr check
        constrain(fn_mono, fn_type(types));
    } else {
        Array<const Type*> types(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = type(args[i]);
        constrain(fn_mono, fn_type(types));
    }

    auto max_arg_index = std::min(args.size(), fn_mono->num_args());

    for (size_t i = 0; i != max_arg_index; ++i)
        constrain(args[i], fn_mono->arg(i));

    for (size_t i = 0; i != args.size(); ++i)
        check(args[i], arg(fn_mono, i));

    for (auto& type_arg : type_args)
        type_arg = find(type_arg);

    return fn_mono->return_type();
#endif
}

const Type* FieldExpr::check(InferSema& sema, const Type* /*TODO expected*/) const {
    auto ltype = sema.check(lhs());
    if (ltype->isa<PtrType>()) {
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto struct_type = ltype->isa<StructType>()) {
        if (auto field_decl = struct_type->struct_decl()->field_decl(symbol()))
            return struct_type->arg(field_decl->index());
    }

    return sema.type_error();
}

const Type* TypeAppExpr::check(InferSema& sema, const Type* /*expected*/) const {
    if (auto type = sema.check(lhs())) {
        if (auto lambda = type->isa<Lambda>()) {
            auto num = sema.num_lambdas(lambda);
            if (num_ast_type_args() <= num) {
                for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
                    sema.constrain(type_args_[i], sema.check(ast_type_arg(i)));

                while (num_type_args() < num)
                    type_args_.push_back(sema.unknown_type());

                for (auto& type_arg : type_args_)
                    type_arg = sema.type(type_arg);

                return sema.reduce(lambda, ast_type_args(), type_args_);
            }
        }
    }
    return sema.type_error();
}

const Type* MapExpr::check(InferSema& sema, const Type* expected) const {
    auto ltype = sema.check(lhs());

    if (ltype->isa<Lambda>()) {
        TypeAppExpr::create(lhs_);
        ltype = sema.check(lhs());
    }

    if (ltype->isa<PtrType>()) {
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto fn_type = ltype->isa<FnType>()) {
        return sema.check_call(fn_type, args(), expected);
    } else {
        if (num_args() == 1)
            sema.check(arg(0));

        if (auto array = ltype->isa<ArrayType>())
            return array->elem_type();
        else if (auto tuple_type = ltype->isa<TupleType>()) {
            if (auto lit = arg(0)->isa<LiteralExpr>())
                return sema.arg(tuple_type, lit->get_u64());
        } else if (auto simd_type = ltype->isa<SimdType>())
            return simd_type->elem_type();
    }

    return sema.type_error();
}

const Type* BlockExprBase::check(InferSema& sema, const Type* expected) const {
    for (auto stmt : stmts())
        sema.check(stmt);

    sema.check(expr(), expected);

    return expr() ? sema.type(expr()) : sema.unit()->as<Type>();
}

const Type* IfExpr::check(InferSema& sema, const Type* expected) const {
    sema.check(cond(), sema.type_bool());
    sema.check(then_expr(), expected);
    sema.check(else_expr(), expected);
    sema.constrain(then_expr(), sema.type(else_expr()), expected);
    sema.constrain(else_expr(), sema.type(then_expr()), expected);
    return sema.constrain(this, sema.type(then_expr()), sema.type(else_expr()));
}

const Type* WhileExpr::check(InferSema& sema, const Type*) const {
    sema.check(cond(), sema.type_bool());
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body(), sema.unit());
    return sema.unit();
}

const Type* ForExpr::check(InferSema& sema, const Type* expected) const {
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();

    if (auto map = forexpr->isa<MapExpr>()) {
        auto ltype = sema.check(map->lhs());

        if (auto fn_for = ltype->isa<FnType>()) {
            if (fn_for->size() != 0) {
                if (auto fn_ret = fn_for->args().back()->isa<FnType>()) {
                    sema.constrain(break_decl_->type_, fn_ret); // inherit the type for break

                    // copy over args and check call
                    Array<const Expr*> args(map->args().size()+1);
                    *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
                    return sema.check_call(fn_for, args, expected);
                }
            }
        }
    }

    return sema.unit();
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(InferSema& sema) const { sema.check(expr()); }
void ItemStmt::check(InferSema& sema) const { sema.check(item()); }

void LetStmt::check(InferSema& sema) const {
    auto expected = sema.check(local());
    if (init())
        sema.constrain(local(), sema.check(init(), expected));
}

//------------------------------------------------------------------------------

}
