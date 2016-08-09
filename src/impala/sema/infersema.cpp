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

    // unification related stuff

    /**
     * Gets the representative of @p type.
     * Initializes @p type with @p UnknownType if @p type is @c nullptr.
     */
    const Type* find_type(const Type*& type);
    const Type* find_type(const Typeable* typeable) { return find_type(typeable->type_); }

    /**
     * @c unify(t, u).
     * Initializes @p t with @p UnknownType if @p type is @c nullptr.
     */
    const Type*& constrain(const    Type*& t, const   Type* u);
    const Type*& constrain(const Typeable* t, const   Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const Type*& constrain(const Typeable* t, const   Type* u)                { return constrain(t->type_, u); }

    /// Obeys subtyping.
    const Type* coerce(const Type* dst, const Expr* src);
    const Type* coerce(const Typeable* dst, const Expr* src) { return dst->type_ = coerce(dst->type_, src); }

    // check wrappers

    void check(const ModContents* n) { n->check(*this); }
    const Type* check(const LocalDecl* local) {
        auto type = local->check(*this);
        if (type->is_hashed())
            constrain(local, type);
        return type;
    }
    const Type* check(const Ptrn* p) { return constrain(p, p->check(*this)); }
    const Type* check(const FieldDecl* f) { return constrain(f, f->check(*this)); }
    void check(const Item* n) { n->check(*this); }
    void check(const Stmt* n) { n->check(*this); }
    const Type* check(const Expr* expr) { return constrain(expr, expr->check(*this)); }
    const Type* check(const Expr* expr, const Type* t) { return constrain(expr, expr->check(*this), t); }

    const Var* check(const ASTTypeParam* ast_type_param) {
        if (!ast_type_param->type())
            ast_type_param->type_ = ast_type_param->check(*this);
        return ast_type_param->type()->as<Var>();
    }

    const Type* check(const ASTType* ast_type) {
        if (ast_type->type())
            return ast_type->type();
        return constrain(ast_type, ast_type->check(*this));
    }

    const Type* check_call(const Expr* lhs, ArrayRef<const Expr*> args);
    const Type* check_call(const Expr* lhs, const std::deque<AutoPtr<const Expr>>& args) {
        Array<const Expr*> array(args.begin(), args.end());
        return check_call(lhs, array);
    }

    const FnType* fn_type(const Type* type) {
        if (auto tuple_type = type->isa<TupleType>())
            return TypeTable::fn_type(tuple_type->ops());
        return TypeTable::fn_type({type});
    }

    const FnType* fn_type(ArrayRef<const Type*> types) { return fn_type(tuple_type(types)); }

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

    /// Unifies @p t and @p u.
    const Type* unify(const Type* t, const Type* u);

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

        size_t i = type_args.size();
        const Type* type = lambda;
        while (auto lambda = type->isa<Lambda>())
            type = app(lambda, type_args[--i]);

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

//------------------------------------------------------------------------------

/*
 * unification
 */

const Type* InferSema::find_type(const Type*& type) {
    if (type == nullptr)
        return type = unknown_type();
    return type = find(type);
}

const Type*& InferSema::constrain(const Type*& t, const Type* u) {
    if (t == nullptr)
        return t = find(u);
    return t = unify(t, u);
}

const Type* InferSema::coerce(const Type* dst, const Expr* src) {
    src->type_ = find_type(src);

    auto t = unify(dst, src->type_);

    if (t->is_known() && src->type()->is_known()) {
        if (t->isa<BorrowedPtrType>() && !src->type()->isa<PtrType>()) {
            src = PrefixExpr::create_addrof(src);
            check(src);
        }
        if (is_subtype(t, src->type_) && t != src->type())
            ImplicitCastExpr::create(src, t);
    }

    return t;
}

const Type* InferSema::unify(const Type* dst, const Type* src) {
    assert(dst->is_hashed() && src->is_hashed());

    auto dst_repr = find(representative(dst));
    auto src_repr = find(representative(src));

    dst = dst_repr->type;
    src = src_repr->type;

    // normalise singleton tuples to their element
    if (src->isa<TupleType>() && src->num_ops() == 1) src = src->op(0);
    if (dst->isa<TupleType>() && dst->num_ops() == 1) dst = dst->op(0);

    // HACK needed as long as we have this stupid tuple problem
    if (auto dst_fn = dst->isa<FnType>()) {
        if (auto src_fn = src->isa<FnType>()) {
            if (dst_fn->num_ops() != 1 && src_fn->num_ops() == 1 && src_fn->op(0)->isa<UnknownType>()) {
                if (dst_fn->is_known())
                    return unify(dst_repr, src_repr)->type;
            }

            if (src_fn->num_ops() != 1 && dst_fn->num_ops() == 1 && dst_fn->op(0)->isa<UnknownType>()) {
                if (src_fn->is_known())
                    return unify(src_repr, dst_repr)->type;
            }
        }
    }

    if (dst == src)            return dst;
    if (dst->isa<TypeError>()) return src; // guess the other one
    if (src->isa<TypeError>()) return dst; // dito

    if (dst->isa<UnknownType>() && src->isa<UnknownType>())
        return unify_by_rank(dst_repr, src_repr)->type;

    if (dst->isa<UnknownType>()) return unify(src_repr, dst_repr)->type;
    if (src->isa<UnknownType>()) return unify(dst_repr, src_repr)->type;

    if (dst->num_ops() == src->num_ops()) {
        if (auto dst_borrowed_ptr_type = dst->isa<BorrowedPtrType>()) {
            if (auto src_owned_ptr_type = src->isa<OwnedPtrType>()) {
                if (src_owned_ptr_type->addr_space() == dst_borrowed_ptr_type->addr_space()) {
                    auto referenced_type = unify(dst_borrowed_ptr_type->referenced_type(), src_owned_ptr_type->referenced_type());
                    return borrowed_ptr_type(referenced_type, dst_borrowed_ptr_type->addr_space());
                }
            }
        }

        if (auto dst_indefinite_array_type = dst->isa<IndefiniteArrayType>()) {
            if (auto src_definite_array_type = src->isa<DefiniteArrayType>()) {
                auto elem_type = unify(dst_indefinite_array_type->elem_type(), src_definite_array_type->elem_type());
                return indefinite_array_type(elem_type);
            }
        }

        if (dst->kind() == src->kind()) {
            Array<const Type*> op(dst->num_ops());
            for (size_t i = 0, e = op.size(); i != e; ++i)
                op[i] = unify(dst->op(i), src->op(i));

            return dst->rebuild(op);
        }
    }

    return dst;
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
    if (repr->parent != repr) {
        todo_ = true;
        repr->parent = find(repr->parent);
    }
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
    todo_ = true;
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
            if (auto type = sema.find_type(type_decl)) {
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
    auto body_type = sema.check(ast_type());

    if (ast_type_params().size() > 0) {
        // TODO parametric Typedefs
#if 0
        auto abs = sema.typedef_abs(sema.type(ast_type())); // TODO might be nullptr
        for (auto lambda : lambdas())
            abs->bind(lambda->lambda());
#endif
    } else
        sema.constrain(this, body_type);
}

void EnumDecl::check(InferSema&) const { /*TODO*/ }

void StructDecl::check(InferSema& sema) const {
    if (type_)
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
    size_t e = num_params();
    // TODO remove wild hack to reduce Typedef'd tuple types to argument lists of return continuations
    if (!is_continuation()) {
        auto ret_type = sema.check(param(e - 1));
        if (ret_type->num_ops() == 1) {
            if (auto ret_tuple_type = ret_type->op(0)->isa<TupleType>()) {
                param_types[--e] = sema.fn_type(ret_tuple_type->ops());
            }
        }
    }

    for (size_t i = 0; i != e; ++i) {
        param_types[i] = sema.check(param(i));
        if (type() && !type()->isa<UnknownType>())
            sema.constrain(param(i), fn_type()->op(i));
    }

    sema.constrain(this, sema.close(num_ast_type_params(), sema.fn_type(param_types)));

    if (body() != nullptr) {
        sema.check(body());
        sema.coerce(fn_type()->return_type(), body());
    }
}

void StaticItem::check(InferSema& sema) const {
    if (ast_type())
        sema.constrain(this, sema.check(ast_type()));
    if (init())
        sema.constrain(this, sema.check(init()));
}

void TraitDecl::check(InferSema& /*sema*/) const {}
void ImplItem::check(InferSema& /*sema*/) const {}


//------------------------------------------------------------------------------

/*
 * expressions
 */

const Type* EmptyExpr::check(InferSema& sema) const { return sema.unit(); }
const Type* LiteralExpr::check(InferSema& sema) const { return sema.prim_type(literal2type()); }
const Type* CharExpr::check(InferSema& sema) const {
    return sema.type_u8();
}

const Type* StrExpr::check(InferSema& sema) const {
    return sema.definite_array_type(sema.type_u8(), values_.size());
}

const Type* FnExpr::check(InferSema& sema) const {
    assert(ast_type_params().empty());

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i) {
        param_types[i] = sema.check(param(i));
        if (type())
            sema.constrain(param(i), fn_type()->op(i));
    }

    auto body_type = sema.check(body());
    if (body_type->isa<NoRetType>() || body_type->isa<UnknownType>())
        return sema.fn_type(param_types);
    else {
        param_types.back() = sema.constrain(params().back(), sema.fn_type(body_type));
        return sema.fn_type(param_types);
    }
}

const Type* PathExpr::check(InferSema& sema) const {
    if (value_decl())
        return sema.find_type(value_decl());
    return sema.type_error();
}

const Type* PrefixExpr::check(InferSema& sema) const {
    switch (kind()) {
        case AND:   return sema.borrowed_ptr_type(sema.check(rhs()), 0);
        case TILDE: return sema.   owned_ptr_type(sema.check(rhs()), 0);
        case MUL: {
            auto type = sema.check(rhs());
            if (auto ptr_type = type->isa<PtrType>())
                return ptr_type->referenced_type();
            else {
                assert(false && "what todo now?");
                return type;
            }
        }
        case INC: case DEC:
        case ADD: case SUB:
        case NOT:
        case RUN: case HLT:
            return sema.check(rhs());
        case OR:  case OROR: // Lambda
            THORIN_UNREACHABLE;
    }
    THORIN_UNREACHABLE;
}

const Type* InfixExpr::check(InferSema& sema) const {
    switch (kind()) {
        case EQ: case NE:
        case LT: case LE:
        case GT: case GE: {
            auto ltype = sema.check(lhs());
            auto rtype = sema.check(rhs());
            sema.constrain(lhs(), rtype);
            sema.constrain(rhs(), ltype);
            return sema.type_bool();
        }
        case OROR:
        case ANDAND:
            sema.check(lhs(), sema.type_bool());
            sema.check(rhs(), sema.type_bool());
            return sema.type_bool();
        case ADD: case SUB:
        case MUL: case DIV: case REM:
        case SHL: case SHR:
        case AND: case OR:  case XOR: {
            auto ltype = sema.check(lhs());
            auto rtype = sema.check(rhs());
            sema.constrain(lhs(), rtype);
            sema.constrain(rhs(), ltype);
            return rhs()->type();
        }
        case ASGN:
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
        case SHL_ASGN: case SHR_ASGN:
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN: {
            sema.check(lhs());
            sema.check(rhs());
            sema.coerce(lhs(), rhs());
            return sema.unit();
        }
    }

    THORIN_UNREACHABLE;
}

const Type* PostfixExpr::check(InferSema& sema) const {
    return sema.check(lhs());
}

const Type* ExplicitCastExpr::check(InferSema& sema) const {
    sema.check(lhs());
    return sema.check(ast_type());
}

const Type* ImplicitCastExpr::check(InferSema& sema) const {
    sema.check(lhs());
    return type();
}

const Type* DefiniteArrayExpr::check(InferSema& sema) const {
    const Type* expected_elem_type;
    if (type_ == nullptr)
        expected_elem_type = sema.unknown_type();
    else if (auto definite_array_type = type_->isa<DefiniteArrayType>())
        expected_elem_type = definite_array_type->elem_type();
    else
        expected_elem_type = sema.type_error();

    for (const auto& arg : args())
        sema.check(arg);

    for (const auto& arg : args())
        expected_elem_type = sema.coerce(expected_elem_type, arg);

    return sema.definite_array_type(expected_elem_type, num_args());
}

const Type* SimdExpr::check(InferSema& sema) const {
    const Type* expected_elem_type;
    if (type_ == nullptr)
        expected_elem_type = sema.unknown_type();
    else if (auto simd_type = type_->isa<SimdType>())
        expected_elem_type = simd_type->elem_type();
    else
        expected_elem_type = sema.type_error();

    for (const auto& arg : args())
        sema.check(arg);

    for (const auto& arg : args())
        expected_elem_type = sema.coerce(expected_elem_type, arg);

    return sema.simd_type(expected_elem_type, num_args());
}

const Type* RepeatedDefiniteArrayExpr::check(InferSema& sema) const {
    return sema.definite_array_type(sema.check(value()), count());
}

const Type* IndefiniteArrayExpr::check(InferSema& sema) const {
    sema.check(dim());
    return sema.indefinite_array_type(sema.check(elem_ast_type()));
}

const Type* TupleExpr::check(InferSema& sema) const {
    Array<const Type*> types(num_args());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.check(arg(i));

    return sema.tuple_type(types);
}

const Type* StructExpr::check(InferSema& sema) const {
    auto type = sema.check(ast_type_app());

    for (const auto& elem : elems())
        sema.check(elem.expr());

    return type;
}

const Type* InferSema::check_call(const Expr* lhs, ArrayRef<const Expr*> args) {
    auto fn_type = lhs->type()->as<FnType>();
    for (auto arg : args)
        check(arg);

    if (args.size() == fn_type->num_ops()) {
        Array<const Type*> types(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = coerce(fn_type->op(i), args[i]);
        constrain(lhs, this->fn_type(types));
        return type_noret();
    } else if (args.size()+1 == fn_type->num_ops()) {
        Array<const Type*> types(args.size()+1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = coerce(fn_type->op(i), args[i]);
        types.back() = fn_type->ops().back();
        auto result = constrain(lhs, this->fn_type(types));
        if (auto fn_type = result->isa<FnType>())
            return fn_type->return_type();
        else
            return type_error();
    } else
        return type_error();
}

const Type* FieldExpr::check(InferSema& sema) const {
    auto ltype = sema.check(lhs());
    if (ltype->isa<PtrType>()) {
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (auto struct_type = ltype->isa<StructType>()) {
        if (auto field_decl = struct_type->struct_decl()->field_decl(symbol()))
            return struct_type->op(field_decl->index());
    }

    return sema.type_error();
}

const Type* TypeAppExpr::check(InferSema& sema) const {
    if (auto type = sema.check(lhs())) {
        if (auto lambda = type->isa<Lambda>()) {
            auto num = sema.num_lambdas(lambda);
            if (num_ast_type_args() <= num) {
                for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
                    type_args_.push_back(sema.check(ast_type_arg(i)));

                while (num_type_args() < num)
                    type_args_.push_back(sema.unknown_type());

                for (auto& type_arg : type_args_)
                    type_arg = sema.find_type(type_arg);

                return sema.reduce(lambda, ast_type_args(), type_args_);
            }
        }
    }
    return sema.type_error();
}

const Type* MapExpr::check(InferSema& sema) const {
    // TODO always check args such that TypeSema doesn't see nullptrs as types
    auto ltype = sema.check(lhs());

    if (ltype->isa<Lambda>()) {
        TypeAppExpr::create(lhs_);
        ltype = sema.check(lhs());
    }

    if (ltype->isa<PtrType>()) {
        PrefixExpr::create_deref(lhs_);
        ltype = sema.check(lhs());
    }

    if (ltype->isa<FnType>()) {
        return sema.check_call(lhs(), args());
    } else if (ltype->isa<UnknownType>()) {
        if (type_ == nullptr)
            return type_ = sema.unknown_type();
        return type_;
    } else {
        if (num_args() == 1)
            sema.check(arg(0));

        if (auto array = ltype->isa<ArrayType>())
            return array->elem_type();
        else if (auto tuple_type = ltype->isa<TupleType>()) {
            if (auto lit = arg(0)->isa<LiteralExpr>())
                return tuple_type->op(lit->get_u64());
        } else if (auto simd_type = ltype->isa<SimdType>())
            return simd_type->elem_type();
    }

    return sema.type_error();
}

const Type* BlockExprBase::check(InferSema& sema) const {
    for (auto stmt : stmts())
        sema.check(stmt);

    return expr() ? sema.check(expr()) : sema.unit()->as<Type>();
}

const Type* IfExpr::check(InferSema& sema) const {
    sema.check(cond());
    sema.constrain(cond(), sema.type_bool());
    auto then_type = sema.check(then_expr());
    auto else_type = sema.check(else_expr());

    if (then_type->isa<NoRetType>()) return else_type;
    if (else_type->isa<NoRetType>()) return then_type;

    sema.constrain(then_expr(), else_type);
    return sema.constrain(else_expr(), then_type);
}

const Type* WhileExpr::check(InferSema& sema) const {
    sema.check(cond());
    sema.constrain(cond(), sema.type_bool());
    sema.check(break_decl());
    sema.check(continue_decl());
    sema.check(body());
    sema.constrain(cond(), sema.unit());
    return sema.unit();
}

const Type* ForExpr::check(InferSema& sema) const {
    auto forexpr = expr();
    if (auto prefix = forexpr->isa<PrefixExpr>())
        if (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT)
            forexpr = prefix->rhs();

    if (auto map = forexpr->isa<MapExpr>()) {
        auto ltype = sema.check(map->lhs());

        if (auto fn_for = ltype->isa<FnType>()) {
            if (fn_for->num_ops() != 0) {
                if (auto fn_ret = fn_for->ops().back()->isa<FnType>()) {
                    sema.constrain(break_decl_->type_, fn_ret); // inherit the type for break

                    // copy over args and check call
                    Array<const Expr*> args(map->args().size()+1);
                    *std::copy(map->args().begin(), map->args().end(), args.begin()) = fn_expr();
                    return sema.check_call(map->lhs(), args);
                }
            }
        }
    }

    return sema.unit();
}

//------------------------------------------------------------------------------

/*
 * patterns
 */

const Type* TuplePtrn::check(InferSema& sema) const {
    Array<const Type*> types(num_elems());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.check(elem(i));
    return sema.tuple_type(types);
}

const Type* IdPtrn::check(InferSema& sema) const {
    return sema.check(local());
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::check(InferSema& sema) const { sema.check(expr()); }
void ItemStmt::check(InferSema& sema) const { sema.check(item()); }

void LetStmt::check(InferSema& sema) const {
    sema.check(ptrn());
    if (init()) {
        sema.check(init());
        sema.coerce(ptrn(), init());
        //sema.constrain(local(), init()->type());
    }
}

//------------------------------------------------------------------------------

}
