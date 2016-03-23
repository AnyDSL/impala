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

    const Type* instantiate(const Lambda* lambda, ArrayRef<const ASTType*> ast_type_args, std::vector<const Type*>& type_args) {
        auto num = num_lambdas(lambda);
        if (ast_type_args.size() <= num) {
            for (size_t i = 0, e = ast_type_args.size(); i != e; ++i)
                constrain(type_args[i], check(ast_type_args[i]));

            while (type_args.size() < num)
                type_args.push_back(unknown_type());

            return lambda->reduce(type_args);
        }

        return type_error();
    }

    void fill_type_args(std::vector<const Type*>& type_args, const ASTTypes& ast_type_args);
    const Type* safe_get_arg(const Type* type, size_t i) { return type && i < type->size() ? type->arg(i) : nullptr; }

    const Type* close(ArrayRef<const Lambda*> lambdas, const Type* body) {
        for (auto& lambda : thorin::reverse_range(lambdas))
            body = impala::close(const_cast<const Lambda*&>(lambda), body);

        closing_ = false;
        return body;
    }

    size_t num_lambdas(const Lambda* lambda) {
        size_t num = 0;
        while (lambda) {
            lambda = lambda->body()->isa<Lambda>();
            ++num;
        }
        return num;
    }

    // unification related stuff

    /**
     * Gets the representative of @p type.
     * Initializes @p type with @p UnknownType if @p type is @c nullptr.
     * Updates @p todo_ if something changed.
     */
    const Type* type(const Type*& type) {
        if (type == nullptr) {
            todo_ = true;
            return type = unknown_type();
        }

        auto otype = type;
        type = find(otype);
        todo_ |= otype != type;
        return type;
    }

    /// Invokes @c type(typeable->type_).
    const Type* type(const Typeable* typeable) { return type(typeable->type_); }

    /**
     * @c unify(t, u).
     * Initializes @p t with @p UnknownType if @p type is @c nullptr.
     * Updates @p todo_ if something changed.
     */
    const Type*& constrain(const Type*& t, const Type* u) {
        if (t == nullptr) {
            if (u == nullptr)
                return t;

            todo_ = true;
            return t = find(u);
        }

        auto otype = t;
        t = unify(t, u);
        todo_ |= otype != t;
        return t;
    }

    const   Type*& constrain(const    Type*& t, const   Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const   Type*& constrain(const Typeable* t, const   Type* u, const Type* v) { return constrain(constrain(t, u), v); }
    const   Type*& constrain(const Typeable* t, const   Type* u)                { return constrain(t->type_, u); }
    const FnType*& constrain(const  FnType*& t, const FnType* u) { return (const FnType*&) constrain((const Type*&)t, (const Type*)u); }

    /// Unifies @p t and @p u. Does @attention { not } update @p todo_.
    const Type* unify(const Type* t, const Type* u) {
        assert(t && t->is_hashed() && THORIN_IMPLIES(u, u->is_hashed()));

        // HACK needed as long as we have this stupid tuple problem
        if (auto t_fn = t->isa<FnType>()) {
            if (auto u_fn = u->isa<FnType>()) {
                if (t_fn->empty() && u_fn->size() == 1 && u_fn->arg(0)->isa<UnknownType>()) return unify(representative(t), representative(u))->type;
                if (u_fn->empty() && t_fn->size() == 1 && t_fn->arg(0)->isa<UnknownType>()) return unify(representative(u), representative(t))->type;
            }
        }

        if (u == nullptr)                                   return t;
        if (t == u)                                         return t;
        if (t->isa<TypeError>())                            return t;
        if (u->isa<TypeError>())                            return u;
        if (t->isa<UnknownType>() && u->isa<UnknownType>()) return unify_by_rank(representative(t), representative(u))->type;
        if (t->isa<UnknownType>())                          return unify        (representative(u), representative(t))->type;
        if (u->isa<UnknownType>())                          return unify        (representative(t), representative(u))->type;

        if (t->kind() == u->kind() && t->size() == u->size()) {
            if (auto t_de_bruijn = t->isa<DeBruijn>()) {
                auto u_de_brujin = u-> as<DeBruijn>();
                if (t_de_bruijn->depth() == u_de_brujin->depth() && t_de_bruijn->lambda()->kind() == u_de_brujin->lambda()->kind())
                    return de_bruijn(old2new_[t_de_bruijn->lambda()]);
            } else if (auto t_lambda = t->isa<Lambda>()) {
                auto u_lambda = u->as<Lambda>();
                auto n_lambda = this->lambda(t_lambda->name());

                assert(!old2new_.contains(t_lambda));
                old2new_[t_lambda] = n_lambda;

                auto n_body = unify(t_lambda->body(), u_lambda->body());

                auto i = old2new_.find(t_lambda);
                assert(i != old2new_.end() && i->second == n_lambda);
                old2new_.erase(i);

                return impala::close(n_lambda, n_body);
            } else {
                Array<const Type*> nargs(t->size());
                for (size_t i = 0, e = nargs.size(); i != e; ++i)
                    nargs[i] = unify(t->arg(i), u->arg(i));

                return t->rebuild(nargs);
            }
        }

        assert(false && "TODO");
        return type_error();
    }

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
    const Type* check(const Expr* expr) {
        auto i = expr2expected_.find(expr);
        if (i == expr2expected_.end()) {
            auto p = expr2expected_.emplace(expr, unknown_type());
            assert(p.second);
            i = p.first;
        }
        return constrain(expr, expr->check(*this, i->second));
    }

    const Lambda* check(const ASTTypeParam* ast_type_param) {
        ast_type_param->type_ = ast_type_param->check(*this);
        return ast_type_param->lambda();
    }

    const Type* check(const ASTType* ast_type) {
        if (ast_type->type() && ast_type->type()->is_known())
            return ast_type->type();

        if (closing_)
            return ast_type->type_ = ast_type->check(*this);
        else
            return ast_type->type_;

        //if (type->is_hashed())
            //return constrain(ast_type, type);

        //return type;
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

        Representative* parent = nullptr;
        const Type* type = nullptr;
        int rank = 0;
    };

    Representative* representative(const Type* type) {
        assert(type->is_hashed());
        auto i = representatives_.find(type);
        if (i == representatives_.end()) {
            auto p = representatives_.emplace(type, type);
            assert_unused(p.second);
            i = p.first;
        }
        return &i->second;
    }

    Representative* find(Representative* repr) {
        if (repr->parent != repr)
            repr->parent = find(repr->parent);
        return repr->parent;
    }

    const Type* find(const Type* type) {
        if (type->is_hashed())
            return find(representative(type))->type;
        return type;
    }

    /// @p x will be the new representative. Returns again @p x.
    Representative* unify(Representative* x, Representative* y) {
        auto x_root = find(x);
        auto y_root = find(y);

        if (x_root == y_root)
            return x_root;
        ++x_root->rank;
        return y_root->parent = x_root;
    }

    /// Depending on the rank either @p x or @p y will be the new representative. Returns the new representative.
    Representative* unify_by_rank(Representative* x, Representative* y) {
        auto x_root = find(x);
        auto y_root = find(y);

        if (x_root == y_root)
            return x_root;
        if (x_root->rank < y_root->rank)
            return x_root->parent = y_root;
        else if (x_root->rank > y_root->rank)
            return y_root->parent = x_root;
        else {
            ++x_root->rank;
            return y_root->parent = x_root;
        }
    }

    TypeMap<Representative> representatives_;
    thorin::HashMap<const Expr*, const Type*> expr2expected_;
    GIDMap<Lambda, const Lambda*> old2new_;
    bool todo_ = true;
public: // hack
    bool closing_ = false;

    friend void type_inference(Init&, const ModContents*);
};

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

const Lambda* ASTTypeParam::check(InferSema& sema) const {
    for (auto bound : bounds())
        sema.check(bound);
    return sema.lambda(symbol().str());
}

Array<const Lambda*> ASTTypeParamList::open_ast_type_params(InferSema& sema) const {
    sema.closing_ = true;
    Array<const Lambda*> lambdas(num_ast_type_params());
    for (size_t i = 0, e = num_ast_type_params(); i != e; ++i)
        lambdas[i] = sema.check(ast_type_param(i));
    return lambdas;
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
    auto lambdas = open_ast_type_params(sema);

    Array<const Type*> types(num_ast_type_args());
    for (size_t i = 0, e = num_ast_type_args(); i != e; ++i)
        types[i] = sema.check(ast_type_arg(i));

    return sema.close(lambdas, sema.fn_type(types));
}

const Type* Typeof::check(InferSema& sema) const { return sema.check(expr()); }

const Type* ASTTypeApp::check(InferSema& sema) const {
    if (decl()) {
        if (auto type_decl = decl()->isa<TypeDecl>()) {
            if (auto ast_type_param = type_decl->isa<ASTTypeParam>())
                return sema.de_bruijn(ast_type_param->lambda());
            else if (auto type = sema.type(type_decl)) {
                if (type->is_hashed()) {
                    if (auto lambda = type->isa<Lambda>())
                        return sema.instantiate(lambda, ast_type_args(), type_args_);
                    else
                        return type;
                }
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
    open_ast_type_params(sema);
    sema.check(ast_type());
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
    auto lambdas = open_ast_type_params(sema);

    for (auto field : field_decls()) {
        if (auto field_type = sema.check(field)) {
            if (!field_type || !field_type->is_known())
                return; // bail out for now if we don't yet know all field types
        }
    }

    auto struct_type = sema.struct_type(this, num_field_decls());

    for (auto field : field_decls())
        struct_type->set(field->index(), sema.type(field));
}

const Type* FieldDecl::check(InferSema& sema) const { return sema.check(ast_type()); }

void FnDecl::check(InferSema& sema) const {
    auto lambdas = open_ast_type_params(sema);

    Array<const Type*> param_types(num_params());
    for (size_t i = 0, e = num_params(); i != e; ++i)
        param_types[i] = sema.check(param(i));

    auto open_fn_type = sema.fn_type(param_types);
    auto new_fn_type = sema.close(lambdas, open_fn_type);

    sema.constrain(this, new_fn_type);

    for (size_t i = 0, e = num_params(); i != e; ++i) {
        sema.check(param(i));
        sema.constrain(param(i), fn_type()->arg(i));
    }

    if (body() != nullptr)
        check_body(sema, fn_type());
}

void StaticItem::check(InferSema& sema) const {
    sema.constrain(this, sema.type(init()));
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
        param_types[i] = sema.constrain(param(i), sema.safe_get_arg(expected, i));

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
            auto expected_referenced_type = sema.safe_get_arg(expected, 0);
            auto rtype = sema.check(rhs(), expected_referenced_type);
            return sema.borrowed_ptr_type(rtype, 0);

        }
        case TILDE:
            return sema.owned_ptr_type(sema.check(rhs(), sema.safe_get_arg(expected, 0)));
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
    auto expected_elem_type = sema.safe_get_arg(expected, 0);

    for (const auto& arg : args())
        expected_elem_type = sema.unify(expected_elem_type, sema.type(arg));

    for (const auto& arg : args())
        sema.check(arg, expected_elem_type);

    return sema.definite_array_type(expected_elem_type, num_args());
}

const Type* SimdExpr::check(InferSema& sema, const Type* expected) const {
    auto expected_elem_type = sema.safe_get_arg(expected, 0);

    for (const auto& arg : args())
        expected_elem_type = sema.unify(expected_elem_type, sema.type(arg));

    for (const auto& arg : args())
        sema.check(arg, expected_elem_type);

    return sema.simd_type(expected_elem_type, num_args());
}

const Type* RepeatedDefiniteArrayExpr::check(InferSema& sema, const Type* expected) const {
    auto expected_elem_type = sema.safe_get_arg(expected, 0);
    return sema.definite_array_type(sema.check(value(), expected_elem_type), count());
}

const Type* IndefiniteArrayExpr::check(InferSema& sema, const Type*) const {
    sema.check(dim());
    return sema.indefinite_array_type(sema.check(elem_ast_type()));
}

const Type* TupleExpr::check(InferSema& sema, const Type* expected) const {
    Array<const Type*> types(num_args());
    for (size_t i = 0, e = types.size(); i != e; ++i)
        types[i] = sema.check(arg(i), sema.safe_get_arg(expected, i));

    return sema.tuple_type(types);
}

void InferSema::fill_type_args(std::vector<const Type*>& type_args, const ASTTypes& ast_type_args) {
    for (size_t i = 0, e = type_args.size(); i != e; ++i) {
        if (i < ast_type_args.size())
            constrain(type_args[i], check(ast_type_args[i]));
        else if (!type_args[i])
            type_args[i] = unknown_type();
    }
}

const Type* StructExpr::check(InferSema&, const Type* /*expected*/) const {
#if 0
    if (auto decl = path()->decl()) {
        if (auto typeable_decl = decl->isa<TypeableDecl>()) {
            if (auto decl_type = sema.type(typeable_decl)) {
                type_args_.resize(decl_type->num_lambdas());
                sema.fill_type_args(type_args_, ast_type_args_);

                if (auto struct_type = decl_type->instantiate(type_args_))
                    return struct_type;
            }
        }
    }

    return sema.type_error();
#endif
    return nullptr;
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
        check(args[i], safe_get_arg(fn_type, i));

    return fn_type->return_type();
#if 0
    type_args.resize(fn_poly->num_lambdas());
    fill_type_args(type_args, ast_type_args);

    constrain(fn_mono, fn_poly->instantiate(type_args)->as<FnType>());

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
        check(args[i], safe_get_arg(fn_mono, i));

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

const Type* TypeAppExpr::check(InferSema& sema, const Type* expected) const {
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

                return sema.instantiate(lambda, ast_type_args(), type_args_);
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
                return sema.safe_get_arg(tuple_type, lit->get_u64());
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
    sema.constrain(then_expr(), sema.type(else_expr()), expected);
    sema.constrain(else_expr(), sema.type(then_expr()), expected);
    sema.check(then_expr(), expected);
    sema.check(else_expr(), expected);
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
