#include "impala/ast.h"

#include "thorin/irbuilder.h"
#include "thorin/continuation.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/util/array.h"
#include "thorin/util/push.h"
#include "thorin/world.h"

using namespace thorin;

namespace impala {

class CodeGen : public IRBuilder {
public:
    CodeGen(World& world)
        : IRBuilder(world)
    {}

    const Def* frame() const { assert(cur_fn); return cur_fn->frame(); }
    /// Enter \p x and perform \p get_value to collect return value.
    const Def* converge(const Expr* expr, JumpTarget& x) {
        emit_jump(expr, x);
        if (enter(x))
            return cur_bb->get_value(1, convert(expr->type()));
        return nullptr;
    }

    void emit_jump_boolean(bool val, JumpTarget& x, const thorin::Location& loc) {
        if (is_reachable()) {
            cur_bb->set_value(1, world().literal(val, loc));
            jump(x, loc);
        }
    }

    Continuation* create_continuation(const LocalDecl* decl) {
        auto result = continuation(convert(decl->type())->as<thorin::FnType>(), decl->loc(), decl->symbol().str());
        result->param(0)->name = "mem";
        decl->value_ = Value::create_val(*this, result);
        return result;
    }

    void set_continuation(Continuation* continuation) {
        cur_bb = continuation;
        set_mem(continuation->param(0));
    }

    void jump_to_continuation(Continuation* continuation, const thorin::Location& loc) {
        if (is_reachable())
            cur_bb->jump(continuation, continuation->type_args(), {get_mem()}, loc);
        set_continuation(continuation);
    }

    Value lemit(const Expr* expr) {
        assert(!expr->needs_cast());
        return expr->lemit(*this);
    }
    const Def* remit(const Expr* expr) {
        auto def = expr->remit(*this);
        if (expr->needs_cast())
            def = world().convert(convert(expr->type()), def, def->loc());
        return def;
    }
    const Def* remit(const Expr* expr, MapExpr::State state, Location eval_loc) {
        assert(!expr->needs_cast());
        return expr->as<MapExpr>()->remit(*this, state, eval_loc);
    }
    void emit_jump(const Expr* expr, JumpTarget& x) { if (is_reachable()) expr->emit_jump(*this, x); }
    void emit_branch(const Expr* expr, JumpTarget& t, JumpTarget& f) { expr->emit_branch(*this, t, f); }
    void emit(const Stmt* stmt) { if (is_reachable()) stmt->emit(*this); }
    void emit(const Item* item) {
        assert(!item->done_);
        item->emit_item(*this);
#ifndef NDEBUG
        item->done_ = true;
#endif
    }
    Value emit(const ValueDecl* decl) {
        assert(decl->value_.kind() != thorin::Value::Empty);
        return decl->value_;
    }
    Value emit(const ValueDecl* decl, const Def* init) {
        if (!decl->value_)
            decl->value_ = decl->emit(*this, init);
        return decl->value_;
    }
    const thorin::Type* convert(const Unifiable* uni) {
        auto unifiable = uni->unify();
        if (!unifiable->thorin_type_) {
            for (auto type_var : unifiable->type_vars())    // convert type vars
                type_var->thorin_type_ = world().type_param(type_var->name().str());

            unifiable->thorin_type_ = unifiable->convert(*this);

            Array<const thorin::TypeParam*> type_params(unifiable->num_type_vars());
            for (size_t i = 0, e = type_params.size(); i != e; ++i)
                type_params[i] = convert(unifiable->type_var(i))->as<thorin::TypeParam>();
            thorin::close(unifiable->thorin_type_, type_params);
        }
        return unifiable->thorin_type_;
    }
    template<class T> const thorin::Type* convert(Proxy<T> type) { return convert(type->unify()); }

    const Fn* cur_fn = nullptr;
};

/*
 * Type
 */

void Unifiable::convert_args(CodeGen& cg, std::vector<const thorin::Type*>& nargs) const {
    for (auto arg : args())
        nargs.push_back(cg.convert(arg));
}

const thorin::Type* PrimTypeNode::convert(CodeGen& cg) const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, ttype) \
        case PrimType_##itype: return cg.world().type_##ttype();
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const thorin::Type* NoRetTypeNode::convert(CodeGen&) const { return nullptr; }

const thorin::Type* FnTypeNode::convert(CodeGen& cg) const {
    std::vector<const thorin::Type*> nargs;
    nargs.push_back(cg.world().mem_type());
    convert_args(cg, nargs);
    return cg.world().fn_type(nargs, num_type_vars());
}

const thorin::Type* TupleTypeNode::convert(CodeGen& cg) const {
    std::vector<const thorin::Type*> nargs;
    convert_args(cg, nargs);
    return cg.world().tuple_type(nargs);
}

const thorin::Type* StructAbsTypeNode::convert(CodeGen& cg) const {
    thorin_type_ = thorin_struct_abs_type_ = cg.world().struct_abs_type(num_args(), num_type_vars(), struct_decl()->symbol().str());
    size_t i = 0;
    for (auto arg : args())
        thorin_struct_abs_type_->set(i++, cg.convert(arg));
    thorin_type_ = nullptr; // will be set again by CodeGen's wrapper
    return thorin_struct_abs_type_;
}

const thorin::Type* StructAppTypeNode::convert(CodeGen& cg) const {
    std::vector<const thorin::Type*> nargs;
    convert_args(cg, nargs);
    return cg.world().struct_app_type(cg.convert(struct_abs_type())->as<thorin::StructAbsType>(), nargs);
}

const thorin::Type* TraitAbsNode::convert(CodeGen& cg) const {
    std::vector<const thorin::Type*> args;

    for (auto super_trait : super_traits())
        args.push_back(cg.convert(super_trait));

    for (auto method : trait_decl()->methods())
        args.push_back(cg.convert(method->type()));

    return cg.world().tuple_type(args);
}

const thorin::Type* TraitAppNode::convert(CodeGen& cg) const {
    Array<const thorin::Type*> nargs(num_args());
    for (size_t i = 0, e = nargs.size(); i != e; ++i)
        nargs[i] = cg.convert(arg(i));
    return cg.convert(trait())->instantiate(nargs);
}

const thorin::Type* ImplNode::convert(CodeGen&) const { THORIN_UNREACHABLE; }
const thorin::Type* PtrTypeNode::convert(CodeGen& cg) const { return cg.world().ptr_type(cg.convert(referenced_type()), 1, -1, thorin::AddrSpace(addr_space())); }
const thorin::Type* DefiniteArrayTypeNode::convert(CodeGen& cg) const { return cg.world().definite_array_type(cg.convert(elem_type()), dim()); }
const thorin::Type* IndefiniteArrayTypeNode::convert(CodeGen& cg) const { return cg.world().indefinite_array_type(cg.convert(elem_type())); }

const thorin::Type* SimdTypeNode::convert(CodeGen& cg) const {
    auto scalar = cg.convert(elem_type());
    return cg.world().type(scalar->as<thorin::PrimType>()->primtype_kind(), size());
}

const thorin::Type* MatrixTypeNode::convert(CodeGen& cg) const {
    return cg.world().definite_array_type(cg.convert(elem_type()), size());
}

/*
 * Decls and Function
 */

Value LocalDecl::emit(CodeGen& cg, const Def* init) const {
    auto thorin_type = cg.convert(type());

    if (is_mut()) {
        if (is_address_taken())
            value_ = Value::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), loc(), symbol().str()));
        else
            value_ = Value::create_mut(cg, handle(), thorin_type, symbol().str());

        if (init)
            value_.store(init, loc());
    } else
        value_ = Value::create_val(cg, init);

    return value_;
}

Continuation* Fn::emit_head(CodeGen& cg, const Location& loc) const {
    return continuation_ = cg.continuation(cg.convert(fn_type())->as<thorin::FnType>(), loc, fn_symbol().remove_quotation());
}

void Fn::emit_body(CodeGen& cg, const Location& loc) const {
    // setup function nest
    continuation()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, continuation());

    // setup memory + frame
    size_t i = 0;
    const Def* mem_param = continuation()->param(i++);
    mem_param->name = "mem";
    cg.set_mem(mem_param);
    frame_ = cg.create_frame(loc);

    // name params and setup store locations
    for (auto param : params()) {
        auto p = continuation()->param(i++);
        p->name = param->symbol().str();
        cg.emit(param, p);
    }
    assert(i == continuation()->num_params());
    if (continuation()->num_params() != 0 && continuation()->params().back()->type()->isa<thorin::FnType>())
        ret_param_ = continuation()->params().back();

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        const Def* mem = cg.get_mem();

        if (auto tuple = def->type()->isa<thorin::TupleType>()) {
            std::vector<const Def*> args;
            args.push_back(mem);
            for (size_t i = 0, e = tuple->num_args(); i != e; ++i)
                args.push_back(cg.extract(def, i, loc));
            cg.cur_bb->jump(ret_param(), {}, args, loc.end());
        } else
            cg.cur_bb->jump(ret_param(), {}, {mem, def}, loc.end());
    }
}

/*
 * items
 */

void ValueItem::emit_item(CodeGen& cg) const {
    cg.emit(static_cast<const ValueDecl*>(this), nullptr);
}

void ModContents::emit(CodeGen& cg) const {
    for (auto item : items())
        cg.emit(item);
}

Value FnDecl::emit(CodeGen& cg, const Def*) const {
    // create thorin function
    value_ = Value::create_val(cg, emit_head(cg, loc()));
    if (is_extern())
        continuation_->make_external();

    // handle main function
    if (symbol() == "main") {
        continuation()->make_external();
    }

    if (body())
        emit_body(cg, loc());
    return value_;
}

void ExternBlock::emit_item(CodeGen& cg) const {
    for (auto fn : fns()) {
        cg.emit(static_cast<const ValueDecl*>(fn), nullptr); // TODO use init
        auto continuation = fn->continuation();
        if (abi() == "\"C\"")
            continuation->cc() = thorin::CC::C;
        else if (abi() == "\"device\"")
            continuation->cc() = thorin::CC::Device;
        else if (abi() == "\"thorin\"")
            continuation->set_intrinsic();
    }
}

void ModDecl::emit_item(CodeGen&) const {
}

void ImplItem::emit_item(CodeGen& cg) const {
    if (def_)
        return;

    Array<const Def*> args(num_methods());
    for (size_t i = 0, e = args.size(); i != e; ++i) {
        cg.emit(static_cast<const ValueDecl*>(method(i)), nullptr); // TODO use init
        args[i] = method(i)->continuation();
    }

    for (size_t i = 0, e = args.size(); i != e; ++i)
        method(i)->emit_body(cg, loc());

    def_ = cg.world().tuple(args, loc());
}

Value StaticItem::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world().bottom(cg.convert(type()), loc()) : cg.remit(this->init());
    if (!is_mut())
        return Value::create_val(cg, init);
    return Value::create_ptr(cg, cg.world().global(init, loc(), true, symbol().str()));
}

void StructDecl::emit_item(CodeGen& cg) const {
    cg.convert(type());
}

void TraitDecl::emit_item(CodeGen& cg) const {
    cg.convert(trait_abs());
}

void Typedef::emit_item(CodeGen&) const {
}

/*
 * expressions
 */

Value Expr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* Expr::remit(CodeGen& cg) const { return lemit(cg).load(loc()); }
void Expr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    if (auto def = cg.remit(this)) {
        assert(cg.is_reachable());
        cg.cur_bb->set_value(1, def);
        cg.jump(x, loc().end());
    } else
        assert(!cg.is_reachable());
}
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f, loc().end()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}, loc()); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeKind tkind;

    switch (kind()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: tkind = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: tkind = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world().literal(tkind, box(), loc());
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world().literal_pu8(value(), loc());
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world().literal_pu8(values_[i], loc());

    auto str = cg.world().definite_array(args, loc());;
    if (is_used_as_global())
        return cg.world().global(str, loc());

    return str;
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = cg.remit(lhs());
    auto thorin_type = cg.convert(ast_type()->type());
    return cg.world().convert(thorin_type, def, loc());
}

Value PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl(), nullptr);
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            const Def* def = var.load(loc());
            const Def* one = cg.world().one(def->type(), loc());
            const Def* ndef = cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one, loc());
            var.store(ndef, loc());
            return ndef;
        }
        case ADD: return cg.remit(rhs());
        case SUB: return cg.world().arithop_minus(cg.remit(rhs()), loc());
        case NOT: return cg.world().arithop_not(cg.remit(rhs()), loc());
        case TILDE: {
            auto def = cg.remit(rhs());
            auto ptr = cg.alloc(def->type(), rhs()->extra(), loc());
            cg.store(ptr, def, loc());
            return ptr;
        }
        case AND: {
            auto var = cg.lemit(rhs());
            assert(var.kind() == Value::PtrRef);
            return var.def();
        }
        case RUN: return cg.remit(rhs(), MapExpr::Run, loc());
        case HLT: return cg.remit(rhs(), MapExpr::Hlt, loc());
        default:  return cg.lemit(this).load(loc());
    }
}

Value PrefixExpr::lemit(CodeGen& cg) const {
    if (kind() == MUL)
        return Value::create_ptr(cg, cg.remit(rhs()));
    THORIN_UNREACHABLE; // TODO
}

void PrefixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    if (kind() == NOT && is_type_bool(cg.convert(type())))
        cg.emit_branch(rhs(), f, t);
    else
        cg.branch(cg.remit(this), t, f, loc().end());
}

void InfixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget x(rhs()->loc().begin(), "and_extra");
            cg.emit_branch(lhs(), x, f);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        case OROR: {
            JumpTarget x(rhs()->loc().begin(), "or_extra");
            cg.emit_branch(lhs(), t, x);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        default:
            cg.branch(cg.remit(this), t, f, loc().end());
            return;
    }
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget t(lhs()->loc().begin(), "and_true"), f(rhs()->loc().begin(), "and_false"), x(loc().end(), "and_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(rhs(), x);
            if (cg.enter(f)) cg.emit_jump_boolean(false, x, lhs()->loc().end());
            return cg.converge(this, x);
        }
        case OROR: {
            JumpTarget t(lhs()->loc().begin(), "or_true"), f(rhs()->loc().begin(), "or_false"), x(loc().end(), "or_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump_boolean(true, x, rhs()->loc().end());
            if (cg.enter(f)) cg.emit_jump(rhs(), x);
            return cg.converge(this, x);
        }
        default:
            auto op = (TokenKind)kind();
            if (Token::is_assign(op)) {
                Value lvar = cg.lemit(lhs());
                const Def* rdef = cg.remit(rhs());

                if (op != Token::ASGN)
                    rdef = emit(cg, Token::separate_assign(op), lvar.load(loc()), rdef);

                lvar.store(rdef, loc());
                return cg.world().tuple({}, loc());
            }

            const Def* ldef = cg.remit(lhs());
            const Def* rdef = cg.remit(rhs());
            return emit(cg, op, ldef, rdef);
    }
}

const Def* InfixExpr::emit(CodeGen& cg, TokenKind op, const Def* ldef, const Def* rdef) const {
    if (lvec_ == SCALAR && rvec_ == SCALAR)
        return cg.world().binop(Token::to_binop(op), ldef, rdef, loc());

    if (kind() == MUL && lvec_ != SCALAR && rvec_ != SCALAR) {
        auto lmat = lhs()->type().as<MatrixType>();
        auto rmat = rhs()->type().as<MatrixType>();
        if (!lmat->is_vector() || !rmat->is_vector()) {
            // matrix-vector or vector-matrix multiplication
            bool transpose = lmat->is_vector();
            int rows = transpose ? lmat->cols() : lmat->rows();
            int cols = rmat->cols();
            int lrows = lmat->rows();
            int rrows = rmat->rows();
            Array<const Def*> defs(rows * cols);
            for (int i = 0; i < cols; i++) {
                for (int j = 0; j < rows; j++) {
                    const Def* sum = nullptr;
                    for (int k = 0, n = rmat->rows(); k < n; k++) {
                        auto mul = cg.world().arithop_mul(
                            cg.world().extract(ldef, transpose ? j : k * lrows + j, loc()),
                            cg.world().extract(rdef,                 i * rrows + k, loc()), loc());
                        sum = sum ? cg.world().arithop_add(sum, mul, loc()) : mul;
                    }
                    defs[transpose ? i : i * rows + j] = sum;
                }
            }
            return cg.world().definite_array(defs, loc());
        }
    }

    // component-wise ops and scalar vs. matrix/vector ops are handled here
    typedef std::function<const Def*(int)> ExtractFn;
    auto lextract = lvec_ == SCALAR ?
        ExtractFn([&] (int i) { return ldef; }) :
        ExtractFn([&] (int i) { return cg.world().extract(ldef, i, loc()); });
    auto rextract = rvec_ == SCALAR ?
        ExtractFn([&] (int i) { return rdef; }) :
        ExtractFn([&] (int i) { return cg.world().extract(rdef, i, loc()); });

    int n = (lvec_ != SCALAR ? lhs() : rhs())->type().as<MatrixType>()->size();
    Array<const Def*> defs(n);
    for (int i = 0; i < n; i++)
        defs[i] = cg.world().binop(Token::to_binop(op), lextract(i), rextract(i), loc());
    return cg.world().definite_array(defs, loc());
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    Value var = cg.lemit(lhs());
    const Def* def = var.load(loc());
    const Def* one = cg.world().one(def->type(), loc());
    var.store(cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one, loc()), loc());
    return def;
}

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, loc());
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(count());
    std::fill_n(args.begin(), count(), cg.remit(value()));
    return cg.world().definite_array(args, loc());
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().tuple(thorin_args, loc());
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = cg.remit(dim());
    return cg.world().indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, loc());
}

const Def* SimdExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().vector(thorin_args, loc());
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (const auto& elem : elems())
        defs[elem.field_decl()->index()] = cg.remit(elem.expr());
    return cg.world().struct_agg(cg.convert(type())->as<thorin::StructAppType>(), defs, loc());
}

Value MapExpr::lemit(CodeGen& cg) const {
    assert(lhs()->type().isa<ArrayType>() || lhs()->type().isa<TupleType>() || lhs()->type().isa<SimdType>());
    auto agg = cg.lemit(lhs());
    return Value::create_agg(agg, cg.remit(arg(0)));
}

const Def* MapExpr::remit(CodeGen& cg) const { return remit(cg, None, Location()); }

const Def* MapExpr::remit(CodeGen& cg, State state, Location eval_loc) const {
    if (auto fn_poly = lhs()->type().isa<FnType>()) {
        assert(fn_poly->num_type_vars() == num_inferred_args());

        Array<const thorin::Type*> type_args(fn_poly->num_type_vars());
        for (size_t i = 0, e = type_args.size(); i != e; ++i)
            type_args[i] = cg.convert(inferred_arg(i));

        const Def* dst = cg.remit(lhs());
        std::vector<const Def*> defs;
        defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad
        for (auto arg : args())
            defs.push_back(cg.remit(arg));
        defs.front() = cg.get_mem(); // now get the current memory monad

        auto ret_type = args().size() == fn_mono()->num_args() ? nullptr : cg.convert(fn_mono()->return_type());
        auto old_bb = cg.cur_bb;
        auto ret = cg.call(dst, type_args, defs, ret_type, loc());
        if (ret_type)
            cg.set_mem(cg.cur_bb->param(0));

        if (state != None) {
            auto eval = state == Run ? &thorin::World::run : &thorin::World::hlt;
            auto cont = old_bb->args().back();
            old_bb->update_callee((cg.world().*eval)(old_bb->callee(), cont, eval_loc, ""));
        }

        return ret;
    } else if (lhs()->type().isa<ArrayType>() || lhs()->type().isa<TupleType>() || lhs()->type().isa<SimdType>()) {
        auto index = cg.remit(arg(0));
        return cg.extract(cg.remit(lhs()), index, loc());
    }
    THORIN_UNREACHABLE;
}

const Def* MatrixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case VEC2:
        case VEC3:
        case VEC4:
        case MAT2X2:
        case MAT3X3:
        case MAT4X4:
        case MAT2X3:
        case MAT2X4:
        case MAT3X2:
        case MAT3X4:
        case MAT4X2:
        case MAT4X3: {
            auto mat = type().as<MatrixType>();
            int i = 0, n = mat->size();
            Array<const Def*> defs(n);
            for (auto arg : args()) {
                auto def = cg.remit(arg);
                if (arg->type().isa<MatrixType>()) {
                    for (int j = 0, m = def->size(); j < m; j++)
                        defs[i++] = cg.world().extract(def, j, loc());
                } else {
                    defs[i++] = def;
                }
            }

            if (i == 1) {
                // repetition constructor, like so: vec4(1.0f)
                if (mat->is_vector()) {
                    for (; i < n; i++) defs[i] = defs[0];
                } else {
                    // for matrices, this means defs[0] * identity
                    auto z = cg.world().zero(cg.convert(mat->elem_type()), loc());
                    for (int i = 0, n = mat->rows(); i < n; i++) {
                        for (int j = 0, m = mat->cols(); j < m; j++)
                            defs[i * mat->cols() + j] = i == j ? defs[0] : z;
                    }
                }
            }
            return cg.world().definite_array(defs, loc());
        }
        case MAT_INVERSE:       return emit_inverse(cg, cg.remit(arg(0))); break;
        case MAT_DETERMINANT:   return emit_determinant(cg, cg.remit(arg(0))); break;
        case VEC_CROSS:         return emit_cross(cg, cg.remit(arg(0)), cg.remit(arg(1))); break;
        case VEC_DOT:           return emit_dot(cg, cg.remit(arg(0)), cg.remit(arg(1))); break;
        default: break;
    }
    THORIN_UNREACHABLE;
}

static const Def* submatrix(CodeGen& cg, int n, int row, int col, const Def* def, const Location& loc) {
    // removes row and col from the given square matrix
    Array<const Def*> defs((n - 1) * (n - 1));
    for (int i = 0, p = 0; i < n; i++) {
        if (i == col) continue;
        for (int j = 0, q = 0; j < n; j++) {
            if (j == row) continue;
            defs[p * (n - 1) + q] = cg.world().extract(def, i * n + j, loc);
            q++;
        }
        p++;
    }
    return cg.world().definite_array(defs, loc);
}

static const Def* determinant(CodeGen& cg, int n, const Def* def, const Location& loc) {
    if (n == 1) return cg.world().extract(def, 0, loc);
    if (n == 2) {
        return cg.world().arithop_sub(
            cg.world().arithop_mul(cg.world().extract(def, 0, loc),
                                   cg.world().extract(def, 3, loc), loc),
            cg.world().arithop_mul(cg.world().extract(def, 1, loc),
                                   cg.world().extract(def, 2, loc), loc), loc);
    }

    const Def* sum = nullptr;
    for (int i = 0; i < n; i++) {
        auto mul = cg.world().arithop_mul(
            cg.world().extract(def, i, loc),
            determinant(cg, n - 1, submatrix(cg, n, i, 0, def, loc), loc), loc);
        sum = sum ? cg.world().binop(i % 2 ? ArithOp_add : ArithOp_sub, sum, mul, loc) : mul;
    }
    return sum;
}

const Def* MatrixExpr::emit_determinant(CodeGen& cg, const Def* def) const {
    return determinant(cg, arg(0)->type().as<MatrixType>()->rows(), def, loc());
}

const Def* MatrixExpr::emit_inverse(CodeGen& cg, const Def* def) const {
    // the formula used is A.t(com A) = (det A).Id
    auto mat = arg(0)->type().as<MatrixType>();
    int n = mat->rows();

    // 1. compute (det A)
    auto det = determinant(cg, n, def, loc());

    // 2. compute t(com A)
    Array<const Def*> elems(n * n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            auto subdet = determinant(cg, n - 1, submatrix(cg, n, i, j, def, loc()), loc());
            elems[i * n + j] = (i + j) % 2 ? cg.world().arithop_minus(subdet, loc()) : subdet;
        }
    }

    // 3. compute det A == 0 ? 0 : 1/detA
    auto elem = cg.convert(mat->elem_type());
    auto zero = cg.world().zero(elem, loc());
    auto one  = cg.world().literal(pf32(1.0f), loc());
    auto cmp  = cg.world().cmp_eq(det, zero, loc());
    auto inv  = cg.world().select(cmp, zero, cg.world().arithop_div(one, det, loc()), loc());

    for (int i = 0; i < n * n; i++) elems[i] = cg.world().arithop_mul(elems[i], inv, loc());

    return cg.world().definite_array(elems, loc());
}

const Def* MatrixExpr::emit_cross(CodeGen& cg, const Def* ldef, const Def* rdef) const {
    Array<const Def*> defs(3);
    for (int i = 0; i < 3; i++) {
        int j = (i + 1) % 3, k = (i + 2) % 3;
        defs[i] = cg.world().arithop_sub(
            cg.world().arithop_mul(
                cg.world().extract(ldef, j, loc()),
                cg.world().extract(rdef, k, loc()), loc()),
            cg.world().arithop_mul(
                cg.world().extract(ldef, k, loc()),
                cg.world().extract(rdef, j, loc()), loc()), loc());
    }
    return cg.world().definite_array(defs, loc());
}

const Def* MatrixExpr::emit_dot(CodeGen& cg, const Def* ldef, const Def* rdef) const {
    int n = arg(0)->type().as<MatrixType>()->rows();
    const Def* sum = nullptr;
    for (int i = 0; i < n; i++) {
        auto mul = cg.world().arithop_mul(
            cg.world().extract(ldef, i, loc()),
            cg.world().extract(rdef, i, loc()), loc());
        sum = sum ? cg.world().arithop_add(sum, mul, loc()) : mul;
    }
    return sum;
}

Value FieldExpr::lemit(CodeGen& cg) const {
    return Value::create_agg(cg.lemit(lhs()), cg.world().literal_qu32(index(), loc()));
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    if (lhs()->type().isa<MatrixType>()) {
        int i = 0, n = swizzle().size();
        auto def = cg.remit(lhs());
        Array<const Def*> defs(n);
        for (auto s : swizzle()) {
            defs[i++] = cg.world().extract(def, s, loc());
        }
        return n > 1 ? cg.world().definite_array(defs, loc()) : defs[0];
    }

    return cg.extract(cg.remit(lhs()), index(), loc());
}

const Def* BlockExprBase::remit(CodeGen& cg) const {
    for (auto stmt : stmts())
        cg.emit(stmt);
    return cg.remit(expr());
}

const Def* RunBlockExpr::remit(CodeGen& cg) const {
    if (cg.is_reachable()) {
        World& w = cg.world();
        auto lrun = w.basicblock(loc(), "run_block");
        auto next = w.basicblock(loc(), "run_next");
        auto old_bb = cg.cur_bb;
        cg.cur_bb->jump(lrun, {}, {}, loc());
        cg.enter(lrun);
        auto res = BlockExprBase::remit(cg);
        if (cg.is_reachable()) {
            assert(res);
            cg.cur_bb->jump(next, {}, {}, loc());
            cg.enter(next);
            old_bb->update_callee(w.run(lrun, next, loc()));
            return res;
        }

        old_bb->update_callee(w.run(lrun, w.bottom(w.fn_type(), loc()), loc()));
    }
    return nullptr;
}

void IfExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    JumpTarget t(then_expr()->loc().begin(), "if_then"), f(else_expr()->loc().begin(), "if_else");
    cg.emit_branch(cond(), t, f);
    if (cg.enter(t))
        cg.emit_jump(then_expr(), x);
    if (cg.enter(f))
        cg.emit_jump(else_expr(), x);
    cg.jump(x, loc().end());
}

const Def* IfExpr::remit(CodeGen& cg) const {
    JumpTarget x(loc().end(), "next");
    return cg.converge(this, x);
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    JumpTarget x(loc().end(), "next");
    auto break_continuation = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_continuation, loc().end());
    return cg.world().tuple({}, loc());
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb(cond()->loc().begin(), "while_head"), body_bb(body()->loc().begin(), "while_body");
    auto continue_continuation = cg.create_continuation(continue_decl());

    cg.jump(head_bb, cond()->loc().end());
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_continuation, cond()->loc().end());
    }
    cg.jump(head_bb, cond()->loc().end());
    head_bb.seal();
    cg.enter(exit_bb);
}

const Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const Def*> defs;
    defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_continuation = cg.create_continuation(break_decl());

    // peel off run and halt
    auto forexpr = expr();
    auto prefix = forexpr->isa<PrefixExpr>();
    if (prefix && (prefix->kind() == PrefixExpr::RUN || prefix->kind() == PrefixExpr::HLT))
        forexpr = prefix->rhs();

    // emit call
    auto map_expr = forexpr->as<MapExpr>();
    for (auto arg : map_expr->args())
        defs.push_back(cg.remit(arg));
    defs.push_back(cg.remit(fn_expr()));
    defs.push_back(break_continuation);
    auto fun = cg.remit(map_expr->lhs());
    if (prefix && prefix->kind() == PrefixExpr::RUN) fun = cg.world().run(fun, break_continuation, loc());
    if (prefix && prefix->kind() == PrefixExpr::HLT) fun = cg.world().hlt(fun, break_continuation, loc());

    defs.front() = cg.get_mem(); // now get the current memory monad
    cg.call(fun, {}, defs, nullptr, map_expr->loc());

    cg.set_continuation(break_continuation);
    if (break_continuation->num_params() == 2)
        return break_continuation->param(1);
    else {
        Array<const Def*> defs(break_continuation->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_continuation->param(i+1);
        return cg.world().tuple(defs, loc());
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto continuation = emit_head(cg, loc());
    emit_body(cg, loc());
    return continuation;
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const {
    if (cg.is_reachable())
        cg.remit(expr());
}

void ItemStmt::emit(CodeGen& cg) const {
    cg.emit(item());
}

void LetStmt::emit(CodeGen& cg) const {
    if (cg.is_reachable()) {
        if (init()) {
            auto def = cg.remit(init());
            def->name = local()->symbol().str();
            cg.emit(local(), def);
        } else
            cg.emit(local(), nullptr);
    }
}

void AsmStmt::emit(CodeGen& cg) const {
    Array<const thorin::Type*> outs(num_outputs());
    for (size_t i = 0, e = num_outputs(); i != e; ++i)
        outs[i] = cg.convert(output(i).expr()->type());

    Array<const Def*> ins(num_inputs());
    for (size_t i = 0, e = num_inputs(); i != e; ++i)
        ins[i] = cg.remit(input(i).expr());

    thorin::Assembly::Flags flags = thorin::Assembly::Flags::NoFlag;
    for (const auto& option : options()) {
        if (option == "volatile")
            flags |= thorin::Assembly::Flags::HasSideEffects;
        else if (option == "alignstack")
            flags |= thorin::Assembly::Flags::IsAlignStack;
        else if (option == "intel")
            flags |= thorin::Assembly::Flags::IsIntelDialect;
    }

    auto assembly = cg.world().assembly(outs, cg.get_mem(), ins, asm_template(),
            output_constraints(), input_constraints(), clobbers(), flags, loc());

    size_t i = 0;
    cg.set_mem(assembly->out(i++));
    for (const auto& output: outputs())
        cg.lemit(output.expr()).store(assembly->out(i++), loc());
}

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) {
    CodeGen cg(world);
    mod->emit(cg);
    clear_value_numbering_table(world);
}

//------------------------------------------------------------------------------

}
