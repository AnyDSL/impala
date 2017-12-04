#include <memory>

#include "thorin/util/array.h"
#include "thorin/util/iterator.h"
#include "thorin/util/log.h"

#include "impala/ast.h"
#include "impala/impala.h"

using namespace thorin;

namespace impala {

//------------------------------------------------------------------------------

class InferSema : public TypeTable {
public:
    // helpers

    const Type* reduce(const Lambda* lambda, ASTTypeArgs ast_type_args, std::vector<const Type*>& type_args);
    void fill_type_args(std::vector<const Type*>& type_args, const ASTTypes& ast_type_args);
    const Type* lambdas(int num_lambdas, const Type* body);
    size_t num_lambdas(const Lambda* lambda);

    // unification related stuff

    /// Unifies @p t and @p u.
    const Type* unify(const Type* t, const Type* u);

    /**
     * Gets the representative of @p type.
     * Initializes @p type with @p UnknownType if @p type is @c nullptr.
     */
    const Type* find_type(const Type*& type);
    const Type* find_type(const Typeable* node) { return find_type(node->type_); }

    /**
     * @c unify(t, u).
     * Initializes @p t with @p UnknownType if @p type is @c nullptr.
     */
    const Type*& constrain(const    Type*& t, const Type* u);
    const Type*& constrain(const Typeable* t, const Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const Type*& constrain(const Typeable* t, const Type* u)                { return constrain(t->type_, u); }

    /// obeys subtyping
    const Type* coerce(const Type* dst, const Expr* src);
    const Type* coerce(const Typeable* dst, const Expr* src) { return dst->type_ = coerce(dst->type_, src); }
    void assign(const Expr* dst, const Expr* src);

    // infer wrappers

    const Type* infer(const LocalDecl* local) {
        auto type = local->infer(*this);
        constrain(local, type);
        return type;
    }
    const Type* infer(const Ptrn* p) { return constrain(p, p->infer(*this)); }
    const Type* infer(const FieldDecl* f) { return constrain(f, f->infer(*this)); }
    const Type* infer(const OptionDecl* o) { return constrain(o, o->infer(*this)); }
    void infer(const Item* n) { n->infer(*this); }
    const Type* infer_head(const Item* n) {
        return (n->type_ == nullptr || n->type_->isa<UnknownType>()) ? n->type_ = n->infer_head(*this) : n->type_;
    }
    void infer(const Stmt* n) { n->infer(*this); }
    const Type* infer(const Expr* expr) { return constrain(expr, expr->infer(*this)); }
    const Type* infer(const Expr* expr, const Type* t) { return constrain(expr, expr->infer(*this), t); }
    const Type* infer(const Path* path) { return constrain(path, path->infer(*this)); }
    const Type* infer(const Path* path, const Type* t) { return constrain(path, path->infer(*this), t); }

    const Var* infer(const ASTTypeParam* ast_type_param) {
        if (!ast_type_param->type())
            ast_type_param->type_ = ast_type_param->infer(*this);
        return ast_type_param->type()->as<Var>();
    }

    const Type* infer(const ASTType* ast_type) {
        return constrain(ast_type, ast_type->infer(*this));
    }

    const Type* infer_call(const Expr* lhs, ArrayRef<const Expr*> args, const Type* call_type);
    const Type* infer_call(const Expr* lhs, const Exprs& args, const Type* call_type) {
        Array<const Expr*> array(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            array[i] = args[i].get();
        return infer_call(lhs, array, call_type);
    }

    const Type* rvalue(const Expr* expr) {
        auto type = infer(expr);
        if (type->isa<RefType>() || (type->isa<UnknownType>() && !expr->isa<RValueExpr>())) {
            todo_ = true;
            return infer(RValueExpr::create(expr));
        }
        return type;
    }

    const Type* wrap_ref(const RefType* ref, const Type* type) {
        return ref ? ref_type(type, ref->is_mut(), ref->addr_space()) : type;
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

    TypeMap<std::unique_ptr<Representative>> representatives_;
    bool todo_ = true;

    friend void type_inference(Init&, const Module*);
};

//------------------------------------------------------------------------------

/*
 * helpers
 */

const Type* InferSema::reduce(const Lambda* lambda, ASTTypeArgs ast_type_args, std::vector<const Type*>& type_args) {
    auto num = num_lambdas(lambda);
    if (ast_type_args.size() <= num) {
        for (size_t i = 0, e = ast_type_args.size(); i != e; ++i)
            constrain(type_args[i], infer(ast_type_args[i].get()));

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
            constrain(type_args[i], infer(ast_type_args[i].get()));
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

const Type* InferSema::lambdas(int num_lambdas, const Type* body) {
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
    find_type(src);
    auto ref = split_ref_type(dst);

    // automatically take address of src if dst is a BorrowedPtrType
    if (auto borrowed_ptr_type = dst->isa<BorrowedPtrType>()) {
        if (!borrowed_ptr_type->is_mut() && !src->type()->isa<UnknownType>() && !src->type()->isa<PtrType>())
            infer(src = PrefixExpr::create_addrof(src));
    }

    // insert implicit cast for subtyping
    if (dst->is_known() && src->type()->is_known() && is_strict_subtype(dst, src->type()))
        infer(src = ImplicitCastExpr::create(src, dst));

    return wrap_ref(ref, unify(dst, src->type()));
}

void InferSema::assign(const Expr* dst, const Expr* src) {
    if (!dst->type()->isa<UnknownType>() && src->type()->is_known())
        coerce(dst->type(), src);
}

const Type* InferSema::unify(const Type* dst, const Type* src) {
    auto dst_repr = find(representative(dst));
    auto src_repr = find(representative(src));

    dst = dst_repr->type;
    src = src_repr->type;

    // normalize singleton tuples to their element
    if (src->isa<TupleType>() && src->num_ops() == 1) src = src->op(0);
    if (dst->isa<TupleType>() && dst->num_ops() == 1) dst = dst->op(0);

    if (dst->isa<UnknownType>() && src->isa<UnknownType>())
        return unify(dst_repr, src_repr)->type;
    if (dst->isa<UnknownType>()) return unify(src_repr, dst_repr)->type;
    if (src->isa<UnknownType>()) return unify(dst_repr, src_repr)->type;

    if (dst == src && dst->is_known()) return dst;
    if (dst->isa<TypeError>() || dst->isa<InferError>()) return dst; // propagate errors
    if (src->isa<TypeError>() || src->isa<InferError>()) return src; // dito

    if (dst->num_ops() == src->num_ops()) {
        // do not unify the operands if the types do not match
        if (auto dst_borrowed_ptr_type = dst->isa<BorrowedPtrType>()) {
            if (auto src_owned_ptr_type = src->isa<OwnedPtrType>()) {
                if (src_owned_ptr_type->addr_space() == dst_borrowed_ptr_type->addr_space())
                    return borrowed_ptr_type(unify(dst->op(0), src->op(0)),
                                             dst_borrowed_ptr_type->is_mut(),
                                             dst_borrowed_ptr_type->addr_space());
            }
        }

        if (dst->isa<IndefiniteArrayType>() && src->isa<DefiniteArrayType>())
            return indefinite_array_type(unify(dst->op(0), src->op(0)));

        if (dst->tag() == src->tag()) {
            // Handle nominal types
            if (src->is_nominal() && src != dst)
                return infer_error(dst, src);

            Array<const Type*> op(dst->num_ops());
            for (size_t i = 0, e = op.size(); i != e; ++i)
                op[i] = unify(dst->op(i), src->op(i));
            return dst->rebuild(op);
        }
    }

    return infer_error(dst, src);
}

//------------------------------------------------------------------------------

/*
 * union-find
 */

auto InferSema::representative(const Type* type) -> Representative* {
    auto i = representatives_.find(type);
    if (i == representatives_.end()) {
        auto p = representatives_.emplace(type, std::make_unique<Representative>(type));
        assert_unused(p.second);
        i = p.first;
    }
    return &*i->second;
}

auto InferSema::find(Representative* repr) -> Representative* {
    if (repr->parent != repr) {
        todo_ = true;
        repr->parent = find(repr->parent);
    }
    return repr->parent;
}

const Type* InferSema::find(const Type* type) {
    return find(representative(type))->type;
}

auto InferSema::unify(Representative* x, Representative* y) -> Representative* {
    assert(x->is_root() && y->is_root());

    if (x == y)
        return x;
    ++x->rank;
    todo_ = true;
    return y->parent = x;
}

//------------------------------------------------------------------------------

void type_inference(Init& init, const Module* module) {
    auto sema = new InferSema();
    init.typetable.reset(sema);

    int i = 0;
    for (;sema->todo_; ++i) {
        sema->todo_ = false;
        sema->infer(module);
    }

    DLOG("iterations needed for type inference: {}", i);
}

//------------------------------------------------------------------------------

/*
 * misc
 */

const Var* ASTTypeParam::infer(InferSema& sema) const {
    for (const auto& bound : bounds())
        sema.infer(bound.get());
    return sema.var(lambda_depth());
}

void ASTTypeParamList::infer_ast_type_params(InferSema& sema) const {
    for (const auto& ast_type_param : ast_type_params())
        sema.infer(ast_type_param.get());
}

const Type* LocalDecl::infer(InferSema& sema) const {
    if (ast_type())
        return sema.infer(ast_type());
    else if (!type())
        return sema.unknown_type();
    return type();
}

const Type* Path::infer(InferSema& sema) const {
    if (!elem(0)->decl_) return sema.type_error();

    auto last_type = sema.constrain(elem(0), sema.find_type(elem(0)->decl_));

    for (size_t i = 1, e = num_elems(); i != e; ++i) {
        auto cur_elem = elem(i);
        auto cur_type = sema.find_type(cur_elem);

        if (auto enum_type = last_type->isa<EnumType>()) {
            // lookup enum option
            auto enum_decl = enum_type->enum_decl();
            auto option_decl = enum_decl->option_decl(cur_elem->symbol());
            auto option_type = option_decl ? sema.find_type(option_decl) : sema.type_error();

            cur_type = sema.constrain(cur_elem, sema.find_type(option_type));
            cur_elem->decl_ = option_decl;
        } else if (last_type->is_known()) {
            cur_type = sema.constrain(cur_elem, sema.type_error());
        }
        last_type = cur_type;
    }

    return last_type;
}

//------------------------------------------------------------------------------

/*
 * AST types
 */

const Type* ErrorASTType::infer(InferSema& sema) const { return sema.type_error(); }

const Type* PrimASTType::infer(InferSema& sema) const {
    switch (tag()) {
#define IMPALA_TYPE(itype, atype) case TYPE_##itype: return sema.prim_type(PrimType_##itype);
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const Type* PtrASTType::infer(InferSema& sema) const {
    auto pointee = sema.infer(referenced_ast_type());
    switch (tag()) {
        case Borrowed: return sema.borrowed_ptr_type(pointee, false, addr_space());
        case Mut:      return sema.borrowed_ptr_type(pointee,  true, addr_space());
        case Owned:    return sema.   owned_ptr_type(pointee, addr_space());
    }
    THORIN_UNREACHABLE;
}

const Type* IndefiniteArrayASTType::infer(InferSema& sema) const { return sema.indefinite_array_type(sema.infer(elem_ast_type())); }
const Type* DefiniteArrayASTType::infer(InferSema& sema) const { return sema.definite_array_type(sema.infer(elem_ast_type()), dim()); }
const Type* SimdASTType::infer(InferSema& sema) const { return sema.simd_type(sema.infer(elem_ast_type()), size()); }

const Type* TupleASTType::infer(InferSema& sema) const {
    Array<const Type*> types(num_ast_type_args());
    for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
        types[i] = sema.infer(ast_type_arg(i));
    // tuples of size 1 do not exist
    if (types.size() == 1) return types.back();
    return sema.tuple_type(types);
}

const Type* FnASTType::infer(InferSema& sema) const {
    infer_ast_type_params(sema);

    Array<const Type*> types(num_ast_type_args());
    for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
        types[i] = sema.infer(ast_type_arg(i));
    return sema.lambdas(num_ast_type_params(), sema.fn_type(types));
}

const Type* Typeof::infer(InferSema& sema) const { return unpack_ref_type(sema.infer(expr())); }

const Type* ASTTypeApp::infer(InferSema& sema) const {
    sema.infer(path());

    if (decl() && decl()->is_type_decl()) {
        if (auto ast_type_param = decl()->isa<ASTTypeParam>())
            return sema.var(ast_type_param->lambda_depth_);
        auto type = sema.find_type(decl());
        if (auto lambda = type->isa<Lambda>())
            return sema.reduce(lambda, ast_type_args(), type_args_);
        return type;
    }

    return sema.type_error();
}

//------------------------------------------------------------------------------

/*
 * Item::infer_head
 */

const Type* Module::infer_head(InferSema&) const { /*TODO*/ return nullptr; }
const Type* ModuleDecl::infer_head(InferSema&) const { /*TODO*/ return nullptr; }
const Type* ExternBlock::infer_head(InferSema&) const { return nullptr; }
const Type* Typedef::infer_head(InferSema&) const { /*TODO*/ return nullptr; }

const Type* StructDecl::infer_head(InferSema& sema) const {
    infer_ast_type_params(sema);
    auto struct_type = sema.struct_type(this, num_field_decls());
    for (size_t i = 0, e = num_field_decls(); i != e; ++i)
        struct_type->set(i, sema.infer(field_decl(i)));
    return struct_type;
}

const Type* EnumDecl::infer_head(InferSema& sema) const {
    infer_ast_type_params(sema);
    auto enum_type = sema.enum_type(this, num_option_decls());
    for (size_t i = 0, e = num_option_decls(); i != e; ++i)
        enum_type->set(i, sema.infer(option_decl(i)));
    return enum_type;
}

const Type* StaticItem::infer_head(InferSema& sema) const {
    if (ast_type())
        return sema.infer(ast_type());
    if (type_ == nullptr)
        type_ = sema.unknown_type();
    return nullptr;
}

const Type* FnDecl::infer_head(InferSema& sema) const {
    infer_ast_type_params(sema);

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.infer(param(i));

    return sema.lambdas(num_ast_type_params(), sema.fn_type(param_types));
}

const Type* TraitDecl::infer_head(InferSema&) const { /*TODO*/ return nullptr; }
const Type* ImplItem::infer_head(InferSema&) const { /*TODO*/ return nullptr; }

/*
 * Item::infer
 */

void ModuleDecl::infer(InferSema&) const {
}

void Module::infer(InferSema& sema) const {
    for (const auto& item : items())
        sema.infer_head(item.get());

    for (const auto& item : items())
        sema.infer(item.get());
}

void ExternBlock::infer(InferSema& sema) const {
    for (const auto& fn_decl : fn_decls())
        sema.infer(fn_decl.get());
}

void Typedef::infer(InferSema& sema) const {
    infer_ast_type_params(sema);
    auto body_type = sema.infer(ast_type());

    if (ast_type_params().size() > 0) {
        // TODO parametric Typedefs
#if 0
        auto abs = sema.typedef_abs(sema.type(ast_type())); // TODO might be nullptr
        for (const auto& lambda : lambdas())
            abs->bind(lambda->lambda());
#endif
    } else
        sema.constrain(this, body_type);
}

void EnumDecl::infer(InferSema& sema) const {
    infer_ast_type_params(sema);
    for (size_t i = 0, e = num_option_decls(); i != e; ++i)
        enum_type()->set(i, sema.infer(option_decl(i)));
}

const Type* OptionDecl::infer(InferSema& sema) const {
    if (num_args() != 0) {
        Array<const Type*> params(num_args() + 1);
        for (size_t i = 0, e = args().size(); i != e; ++i)
            params[i] = sema.infer(arg(i));
        params.back() = sema.fn_type(sema.find_type(enum_decl()));
        return sema.fn_type(params);
    } else {
        return sema.find_type(enum_decl());
    }
}

void StructDecl::infer(InferSema& sema) const {
    infer_ast_type_params(sema);
    for (size_t i = 0, e = num_field_decls(); i != e; ++i)
        struct_type()->set(i, sema.infer(field_decl(i)));
}

const Type* FieldDecl::infer(InferSema& sema) const { return sema.infer(ast_type()); }

void FnDecl::infer(InferSema& sema) const {
    infer_ast_type_params(sema);

    sema.infer(pe_expr());

    Array<const Type*> param_types(num_params());
    size_t e = num_params();

    for (size_t i = 0; i != e; ++i) {
        param_types[i] = sema.infer(param(i));
        if (type() && type()->isa<FnType>())
            sema.constrain(param(i), fn_type()->param(i));
    }

    sema.constrain(this, sema.lambdas(num_ast_type_params(), sema.fn_type(param_types)));

    if (body() != nullptr) {
        if (!sema.infer(body())->isa<NoRetType>())
            sema.coerce(fn_type()->return_type(), body());
    }
}

void StaticItem::infer(InferSema& sema) const {
    if (ast_type())
        sema.constrain(this, sema.infer(ast_type()));
    if (init())
        sema.constrain(this, sema.rvalue(init()));
}

void TraitDecl::infer(InferSema& /*sema*/) const {}
void ImplItem::infer(InferSema& /*sema*/) const {}

//------------------------------------------------------------------------------

/*
 * expressions
 */

const Type* EmptyExpr::infer(InferSema& sema) const { return sema.unit(); }
const Type* LiteralExpr::infer(InferSema& sema) const { return sema.prim_type(literal2type()); }
const Type* CharExpr::infer(InferSema& sema) const {
    return sema.type_u8();
}

const Type* StrExpr::infer(InferSema& sema) const {
    return sema.definite_array_type(sema.type_u8(), values_.size());
}

const Type* FnExpr::infer(InferSema& sema) const {
    assert(ast_type_params().empty());

    sema.infer(pe_expr());

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i) {
        param_types[i] = sema.infer(param(i));
        if (type() && type()->isa<FnType>())
            sema.constrain(param(i), fn_type()->param(i));
    }

    auto body_type = sema.rvalue(body());
    if (num_params() == 0 && body_type->isa<NoRetType>())
        return sema.fn_type(sema.unit());

    if (body_type->isa<NoRetType>() || body_type->isa<UnknownType>())
        return sema.fn_type(param_types);
    else {
        if (num_params() == 0) return sema.fn_type(sema.fn_type(sema.type_error()));

        param_types.back() = sema.constrain(params().back().get(), sema.fn_type(body_type));
        return sema.fn_type(param_types);
    }
}

const Type* PathExpr::infer(InferSema& sema) const {
    sema.infer(path());
    if (value_decl()) {
        auto type = sema.find_type(value_decl());
        return value_decl()->is_mut() ? sema.ref_type(type, true, 0) : type;
    }

    return sema.type_error();
}

const Type* PrefixExpr::infer(InferSema& sema) const {
    switch (tag()) {
        case AND: {
            auto type = sema.infer(rhs());
            if (auto ref = type->isa<RefType>())
                return sema.borrowed_ptr_type(ref->pointee(), false, ref->addr_space());
            // The type might turn out to be a ref type later on
            if (type->isa<UnknownType>()) return sema.find_type(this);
            return sema.borrowed_ptr_type(type, false, 0);
        }
        case MUT: {
            auto type = sema.infer(rhs());
            if (auto ref = type->isa<RefType>())
                return sema.borrowed_ptr_type(ref->pointee(), true, ref->addr_space());
            // The type might turn out to be a ref type later on
            if (type->isa<UnknownType>()) return sema.find_type(this);
            return sema.borrowed_ptr_type(type, true, 0);
        }
        case TILDE:
            return sema.owned_ptr_type(sema.rvalue(rhs()), 0);
        case MUL: {
            auto type = sema.rvalue(rhs());
            if (auto ptr_type = type->isa<PtrType>())
                return sema.ref_type(ptr_type->pointee(), ptr_type->is_mut(), ptr_type->addr_space());
            else
                return sema.find_type(this);
        }
        case INC: case DEC:
            return unpack_ref_type(sema.infer(rhs()));
        case ADD: case SUB:
        case NOT:
        case HLT:
            return sema.rvalue(rhs());
        case KNOWN:
            return sema.type_bool();
        case RUNRUN:
            return sema.infer(rhs());
        case OR: case OROR: case RUN: // Lambda
            THORIN_UNREACHABLE;
    }
    THORIN_UNREACHABLE;
}

const Type* InfixExpr::infer(InferSema& sema) const {
    switch (tag()) {
        case EQ: case NE:
        case LT: case LE:
        case GT: case GE: {
            auto ltype = sema.rvalue(lhs());
            auto rtype = sema.rvalue(rhs());
            sema.constrain(lhs(), rtype);
            sema.constrain(rhs(), ltype);
            if (auto simd = rhs()->type()->isa<SimdType>())
                return sema.simd_type(sema.type_bool(), simd->dim());
            return sema.type_bool();
        }
        case OROR:
        case ANDAND:
            sema.rvalue(lhs());
            sema.rvalue(rhs());
            sema.constrain(lhs(), sema.type_bool());
            sema.constrain(rhs(), sema.type_bool());
            return sema.type_bool();
        case ADD: case SUB:
        case MUL: case DIV: case REM:
        case SHL: case SHR:
        case AND: case OR:  case XOR: {
            auto ltype = sema.rvalue(lhs());
            auto rtype = sema.rvalue(rhs());
            sema.constrain(lhs(), rtype);
            sema.constrain(rhs(), ltype);
            return rtype;
        }
        case ASGN:
        case ADD_ASGN: case SUB_ASGN:
        case MUL_ASGN: case DIV_ASGN: case REM_ASGN:
        case SHL_ASGN: case SHR_ASGN:
        case AND_ASGN: case  OR_ASGN: case XOR_ASGN: {
            sema.infer(lhs());
            sema.rvalue(rhs());
            sema.assign(lhs(), rhs());
            return sema.unit();
        }
        case AS:
            THORIN_UNREACHABLE;
    }

    THORIN_UNREACHABLE;
}

const Type* PostfixExpr::infer(InferSema& sema) const {
    return unpack_ref_type(sema.infer(lhs()));
}

const Type* ExplicitCastExpr::infer(InferSema& sema) const {
    sema.rvalue(src());
    return sema.infer(ast_type());
}

const Type* ImplicitCastExpr::infer(InferSema& sema) const {
    sema.rvalue(src());
    return type();
}

const Type* RValueExpr::infer(InferSema& sema) const {
    auto src_type = sema.infer(src());
    // references are converted to rvalues
    if (auto ref_type = src_type->isa<RefType>())
        return ref_type->pointee();
    // function calls never result in references
    if (auto map = src()->isa<MapExpr>()) {
        if (map->lhs()->type()->isa<FnType>())
            return src_type;
    }
    // if the type is not known, we cannot make a decision yet
    return src_type->isa<UnknownType>() ? sema.find_type(this) : src_type;
}

const Type* DefiniteArrayExpr::infer(InferSema& sema) const {
    const Type* expected_elem_type;
    if (type_ == nullptr)
        expected_elem_type = sema.unknown_type();
    else if (auto definite_array_type = type_->isa<DefiniteArrayType>())
        expected_elem_type = definite_array_type->elem_type();
    else
        expected_elem_type = sema.type_error();

    for (const auto& arg : args())
        sema.rvalue(arg.get());

    for (const auto& arg : args())
        expected_elem_type = sema.coerce(expected_elem_type, arg.get());

    return sema.definite_array_type(expected_elem_type, num_args());
}

const Type* SimdExpr::infer(InferSema& sema) const {
    const Type* expected_elem_type;
    if (type_ == nullptr)
        expected_elem_type = sema.unknown_type();
    else if (auto simd_type = type_->isa<SimdType>())
        expected_elem_type = simd_type->elem_type();
    else
        expected_elem_type = sema.type_error();

    for (const auto& arg : args())
        sema.rvalue(arg.get());

    for (const auto& arg : args())
        expected_elem_type = sema.coerce(expected_elem_type, arg.get());

    return sema.simd_type(expected_elem_type, num_args());
}

const Type* RepeatedDefiniteArrayExpr::infer(InferSema& sema) const {
    return sema.definite_array_type(sema.rvalue(value()), count());
}

const Type* IndefiniteArrayExpr::infer(InferSema& sema) const {
    sema.rvalue(dim());
    return sema.indefinite_array_type(sema.infer(elem_ast_type()));
}

const Type* TupleExpr::infer(InferSema& sema) const {
    Array<const Type*> types(num_args());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.rvalue(arg(i));

    return sema.tuple_type(types);
}

const Type* StructExpr::infer(InferSema& sema) const {
    auto type = sema.infer(ast_type_app());

    for (size_t i = 0, e = num_elems(); i != e; ++i)
        sema.rvalue(elem(i)->expr());

    if (auto struct_type = type->isa<StructType>()) {
        for (size_t i = 0, e = num_elems(); i != e; ++i) {
            elem(i)->field_decl_ = struct_type->struct_decl()->field_decl(elem(i)->symbol());
            if (elem(i)->field_decl() != nullptr)
                sema.coerce(struct_type->op(elem(i)->field_decl()->index()), elem(i)->expr());
        }
    }

    return type;
}

const Type* InferSema::infer_call(const Expr* lhs, ArrayRef<const Expr*> args, const Type* call_type) {
    auto fn_type = lhs->type()->as<FnType>();

    if (args.size() == fn_type->num_params()) {
        Array<const Type*> types(args.size());
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = coerce(fn_type->param(i), args[i]);
        constrain(lhs, this->fn_type(types));
        return type_noret();
    }

    if (args.size()+1 == fn_type->num_params()) {
        Array<const Type*> types(args.size()+1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            types[i] = coerce(fn_type->param(i), args[i]);
        types.back() = fn_type->last_param();

        auto result = constrain(lhs, this->fn_type(types));
        if (auto fn_type = result->isa<FnType>())
            return fn_type->return_type();
        return call_type;
    }

    return type_error();
}

const Type* FieldExpr::infer(InferSema& sema) const {
    auto ltype = sema.infer(lhs());
    if (is_ptr(ltype)) {
        PrefixExpr::create_deref(lhs_.get());
        ltype = sema.infer(lhs());
    }

    auto ref = split_ref_type(ltype);

    if (auto struct_type = ltype->isa<StructType>()) {
        if (auto field_decl = struct_type->struct_decl()->field_decl(symbol())) {
            return sema.wrap_ref(ref, struct_type->op(field_decl->index()));
        }
    }

    return ltype->is_known() ? sema.type_error() : sema.find_type(this);
}

const Type* TypeAppExpr::infer(InferSema& sema) const {
    if (auto type = sema.rvalue(lhs())) {
        if (auto lambda = type->isa<Lambda>()) {
            auto num = sema.num_lambdas(lambda);
            if (type_args_.size() < num) {
                assert(type_args_.empty());

                for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
                    type_args_.push_back(sema.infer(ast_type_arg(i)));

                while (num_type_args() < num)
                    type_args_.push_back(sema.unknown_type());
            }

            for (auto& type_arg : type_args_) {
                // The most precise type is required here.
                // using find_type is not enough, as the
                // type may be an aggregate
                type_arg = sema.unify(type_arg, type_arg);
            }

            return sema.reduce(lambda, ast_type_args(), type_args_);
        }
    }
    return sema.find_type(this);
}

const Type* MapExpr::infer(InferSema& sema) const {
    auto ltype = sema.infer(lhs());
    if (is_ptr(ltype)) {
        PrefixExpr::create_deref(lhs_.get());
        ltype = sema.infer(lhs());
    }

    auto ref = split_ref_type(ltype);

    for (const auto& arg : args())
        sema.rvalue(arg.get());

    if (ltype->isa<UnknownType>())
        return sema.find_type(this);

    if (auto array = ltype->isa<ArrayType>())
        return sema.wrap_ref(ref, array->elem_type());

    if (auto tuple_type = ltype->isa<TupleType>()) {
        if (auto lit = arg(0)->isa<LiteralExpr>())
            return sema.wrap_ref(ref, tuple_type->op(lit->get_u64()));
        else
            return sema.wrap_ref(ref, sema.type_error());
    }

    if (auto simd_type = ltype->isa<SimdType>())
        return sema.wrap_ref(ref, simd_type->elem_type());

    if (ref && (ltype->isa<Lambda>() || ltype->isa<FnType>()))
        ltype = sema.rvalue(lhs());

    if (ltype->isa<Lambda>()) {
        if (!lhs()->isa<TypeAppExpr>())
            TypeAppExpr::create(lhs());
        ltype = sema.infer(lhs());
    }

    if (ltype->isa<FnType>())
        return sema.infer_call(lhs(), args(), sema.find_type(this));

    return sema.type_error();
}

const Type* BlockExpr::infer(InferSema& sema) const {
    for (const auto& stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            sema.infer_head(item_stmt->item());
    }

    for (const auto& stmt : stmts())
        sema.infer(stmt.get());

    return expr() ? sema.rvalue(expr()) : sema.unit()->as<Type>();
}

const Type* IfExpr::infer(InferSema& sema) const {
    sema.rvalue(cond());
    sema.constrain(cond(), sema.type_bool());
    auto then_type = sema.rvalue(then_expr());
    auto else_type = sema.rvalue(else_expr());

    if (then_type->isa<NoRetType>() || then_type->isa<UnknownType>()) return else_type;
    if (else_type->isa<NoRetType>() || else_type->isa<UnknownType>()) return then_type;

    sema.constrain(then_expr(), else_type);
    return sema.constrain(else_expr(), then_type);
}

const Type* MatchExpr::infer(InferSema& sema) const {
    sema.rvalue(expr());
    for (size_t i = 0, e = num_arms(); i != e; ++i) {
        sema.infer(arm(i)->ptrn());
        sema.coerce(arm(i)->ptrn(), expr());
        sema.rvalue(arm(i)->expr());
        if (i > 0)
            sema.coerce(arm(i)->expr(), arm(i-1)->expr());
    }
    return num_arms() > 0 ? arm(0)->expr()->type() : sema.type_error();
}

const Type* WhileExpr::infer(InferSema& sema) const {
    sema.rvalue(cond());
    sema.constrain(cond(), sema.type_bool());
    sema.infer(break_decl());
    sema.infer(continue_decl());
    sema.rvalue(body());
    return sema.unit();
}

const Type* ForExpr::infer(InferSema& sema) const {
    sema.rvalue(fn_expr());
    auto forexpr = expr();

    if (auto map = forexpr->isa<MapExpr>()) {
        auto ltype = sema.rvalue(map->lhs());

        for (size_t i = 0, e = map->num_args(); i != e; ++i)
            sema.rvalue(map->arg(i));

        if (auto fn_for = ltype->isa<FnType>()) {
            if (fn_for->num_params() != 0) {
                if (auto fn_ret = fn_for->last_param()->isa<FnType>())
                    sema.constrain(break_decl_.get(), fn_ret); // inherit the type for break
            }

            // copy over args and infer call
            Array<const Expr*> args(map->num_args() + 1);
            for (size_t i = 0, e = map->num_args(); i != e; ++i)
                args[i] = map->arg(i);
            args.back() = fn_expr();
            return sema.infer_call(map->lhs(), args, type_);
        }
    }

    return sema.unit();
}

//------------------------------------------------------------------------------

/*
 * patterns
 */

const Type* TuplePtrn::infer(InferSema& sema) const {
    auto types = Array<const Type*>(num_elems(), [&](auto i) { return sema.infer(this->elem(i)); });
    return sema.tuple_type(types);
}

const Type* IdPtrn::infer(InferSema& sema) const {
    return sema.infer(local());
}

const Type* EnumPtrn::infer(InferSema& sema) const {
    auto ret_type = sema.find_type(this);
    if (num_args() > 0) {
        Array<const Type*> params(num_args() + 1);
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            params[i] = sema.infer(arg(i));
        }
        params.back() = sema.fn_type(ret_type);
        sema.infer(path(), sema.fn_type(params));
        return ret_type;
    } else {
        return sema.infer(path());
    }
}

const Type* LiteralPtrn::infer(InferSema& sema) const {
    return sema.infer(literal());
}

//------------------------------------------------------------------------------

/*
 * statements
 */

void ExprStmt::infer(InferSema& sema) const { sema.infer(expr()); }
void ItemStmt::infer(InferSema& sema) const { sema.infer(item()); }

void LetStmt::infer(InferSema& sema) const {
    sema.infer(ptrn());
    if (init()) {
        sema.rvalue(init());
        sema.coerce(ptrn(), init());
    }
}

void AsmStmt::infer(InferSema& sema) const {
    for (const auto& output : outputs()) sema.infer(output->expr());
    for (const auto&  input :  inputs()) sema.rvalue(input->expr());
}

//------------------------------------------------------------------------------

}
