#include "impala/ast.h"

#include "thorin/irbuilder.h"
#include "thorin/lambda.h"
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

    Lambda* create_continuation(const LocalDecl* decl) {
        auto result = lambda(convert(decl->type())->as<thorin::FnType>(), decl->loc(), decl->symbol().str());
        result->param(0)->name = "mem";
        decl->var_ = Var::create_val(*this, result);
        return result;
    }

    void set_continuation(Lambda* lambda) {
        cur_bb = lambda;
        set_mem(lambda->param(0));
    }

    void jump_to_continuation(Lambda* lambda, const thorin::Location& loc) {
        if (is_reachable())
            cur_bb->jump(lambda, {get_mem()}, loc);
        set_continuation(lambda);
    }

    Var lemit(const Expr* expr) {
        assert(!expr->needs_cast());
        return expr->lemit(*this);
    }
    const Def* remit(const Expr* expr) {
        auto def = expr->remit(*this);
        if (expr->needs_cast())
            def = world().convert(convert(expr->type()), def, def->loc());
        return def;
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
    Var emit(const ValueDecl* decl) {
        assert(decl->var_.kind() != thorin::Var::Empty);
        return decl->var_;
    }
    Var emit(const ValueDecl* decl, const Def* init) {
        if (!decl->var_)
            decl->var_ = decl->emit(*this, init);
        return decl->var_;
    }
    const thorin::Type* convert(const Type* type) {
        if (thorin_type(type) == nullptr)
            thorin_type(type) = convert_rec(type);
        return thorin_type(type);
    }

    void convert_args(const Type*, std::vector<const thorin::Type*>& nargs);
    const thorin::Type* convert_rec(const Type*);

    const thorin::Type*& thorin_type(const Type* type) { return impala2thorin_[type]; }
    const thorin::StructType*& thorin_struct_type(const StructType* type) { return struct_type_impala2thorin_[type]; }

    const Fn* cur_fn = nullptr;
    TypeMap<const thorin::Type*> impala2thorin_;
    GIDMap<StructType, const thorin::StructType*> struct_type_impala2thorin_;
};

/*
 * Type
 */

void CodeGen::convert_args(const Type* type, std::vector<const thorin::Type*>& nargs) {
    for (auto arg : type->args())
        nargs.push_back(convert(arg));
}

const thorin::Type* CodeGen::convert_rec(const Type* type) {
    if (auto type_abs = type->isa<TypeAbs>()) {
        auto thorin_type_param = world().type_param(type_abs->type_param()->name());
        thorin_type(type_abs->type_param()) = thorin_type_param;
        return thorin_type(type_abs) = world().type_abs(thorin_type_param, convert(type_abs->body()));
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_kind()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world().type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto fn_type = type->isa<FnType>()) {
        std::vector<const thorin::Type*> nargs;
        nargs.push_back(world().mem_type());
        convert_args(fn_type, nargs);
        return world().fn_type(nargs);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Type*> nargs;
        convert_args(tuple_type, nargs);
        return world().tuple_type(nargs);
    } else if (auto struct_type = type->isa<StructType>()) {
        thorin_struct_type(struct_type) = world().struct_type(struct_type->struct_decl()->symbol().str(), struct_type->size());
        thorin_type(type) = thorin_struct_type(struct_type);
        size_t i = 0;
        for (auto arg : struct_type->args())
            thorin_struct_type(struct_type)->set(i++, convert(arg));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return thorin_struct_type(struct_type);
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world().ptr_type(convert(ptr_type->referenced_type()), 1, -1, thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world().definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world().indefinite_array_type(convert(indefinite_array_type->elem_type()));
    } else if (auto simd_type = type->isa<SimdType>()) {
        return world().type(convert(simd_type->elem_type())->as<thorin::PrimType>()->primtype_kind(), simd_type->dim());
    }
    THORIN_UNREACHABLE;
}

/*
 * Decls and Function
 */

Var LocalDecl::emit(CodeGen& cg, const Def* init) const {
    auto thorin_type = cg.convert(type());
    if (!init)
        init = cg.world().bottom(thorin_type, loc());
    if (!is_mut())
        return var_ = Var::create_val(cg, init);

    if (is_address_taken())
        var_ = Var::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), handle(), loc(), symbol().str()));
    else
        var_ = Var::create_mut(cg, handle(), thorin_type, symbol().str()); // TODO

    var_.store(init, loc());
    return var_;
}

Lambda* Fn::emit_head(CodeGen& cg, const Location& loc) const {
    return lambda_ = cg.lambda(cg.convert(fn_type())->as<thorin::FnType>(), loc, fn_symbol().remove_quotation());
}

void Fn::emit_body(CodeGen& cg, const Location& loc) const {
    // setup function nest
    lambda()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, lambda());

    // setup memory + frame
    size_t i = 0;
    const Def* mem_param = lambda()->param(i++);
    mem_param->name = "mem";
    cg.set_mem(mem_param);
    frame_ = cg.create_frame(loc);

    // name params and setup store locations
    for (auto param : params()) {
        auto p = lambda()->param(i++);
        p->name = param->symbol().str();
        cg.emit(param, p);
    }
    assert(i == lambda()->num_params());
    if (lambda()->num_params() != 0 && lambda()->params().back()->type()->isa<thorin::FnType>())
        ret_param_ = lambda()->params().back();

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        const Def* mem = cg.get_mem();

        if (auto tuple = def->type()->isa<thorin::TupleType>()) {
            std::vector<const Def*> args;
            args.push_back(mem);
            for (size_t i = 0, e = tuple->size(); i != e; ++i)
                args.push_back(cg.extract(def, i, loc));
            cg.cur_bb->jump(ret_param(), args, loc.end());
        } else
            cg.cur_bb->jump(ret_param(), {mem, def}, loc.end());
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

Var FnDecl::emit(CodeGen& cg, const Def*) const {
    // create thorin function
    var_ = Var::create_val(cg, emit_head(cg, loc()));
    if (is_extern())
        lambda_->make_external();

    // handle main function
    if (symbol() == "main") {
        lambda()->make_external();
    }

    if (body())
        emit_body(cg, loc());
    return var_;
}

void ExternBlock::emit_item(CodeGen& cg) const {
    for (auto fn : fns()) {
        cg.emit(static_cast<const ValueDecl*>(fn), nullptr); // TODO use init
        auto lambda = fn->lambda();
        if (abi() == "\"C\"")
            lambda->cc() = thorin::CC::C;
        else if (abi() == "\"device\"")
            lambda->cc() = thorin::CC::Device;
        else if (abi() == "\"thorin\"")
            lambda->set_intrinsic();
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
        args[i] = method(i)->lambda();
    }

    for (size_t i = 0, e = args.size(); i != e; ++i)
        method(i)->emit_body(cg, loc());

    def_ = cg.world().tuple(args, loc());
}

Var StaticItem::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world().bottom(cg.convert(type()), loc()) : cg.remit(this->init());
    if (!is_mut())
        return Var::create_val(cg, init);
    return Var::create_ptr(cg, cg.world().global(init, loc(), true, symbol().str()));
}

void StructDecl::emit_item(CodeGen& cg) const {
    cg.convert(type());
}

void TraitDecl::emit_item(CodeGen&) const {}
void Typedef::emit_item(CodeGen&) const {}

/*
 * expressions
 */

Var Expr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
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

Var PathExpr::lemit(CodeGen& cg) const {
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
            assert(var.kind() == Var::PtrRef);
            return var.def();
        }
        case RUN: return cg.world().run(cg.remit(rhs()), loc());
        case HLT: return cg.world().hlt(cg.remit(rhs()), loc());
        default:  return cg.lemit(this).load(loc());
    }
}

Var PrefixExpr::lemit(CodeGen& cg) const {
    if (kind() == MUL)
        return Var::create_ptr(cg, cg.remit(rhs()));
    THORIN_UNREACHABLE; // TODO
}

void PrefixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    if (kind() == NOT && is_type_bool(cg.convert(type())))
        cg.emit_branch(rhs(), f, t);
    else
        cg.branch(cg.remit(rhs()), t, f, loc().end());
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
            const TokenKind op = (TokenKind) kind();

            if (Token::is_assign(op)) {
                Var lvar = cg.lemit(lhs());
                const Def* rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    TokenKind sop = Token::separate_assign(op);
                    rdef = cg.world().binop(Token::to_binop(sop), lvar.load(loc()), rdef, loc());
                }

                lvar.store(rdef, loc());
                return cg.world().tuple({}, loc());
            }

            const Def* ldef = cg.remit(lhs());
            const Def* rdef = cg.remit(rhs());
            return cg.world().binop(Token::to_binop(op), ldef, rdef, loc());
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    Var var = cg.lemit(lhs());
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
    return cg.world().struct_agg(cg.convert(type())->as<thorin::StructType>(), defs, loc());
}

Var MapExpr::lemit(CodeGen& cg) const {
    if (lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<TupleType>() || lhs()->type()->isa<SimdType>()) {
        auto agg = cg.lemit(lhs());
        return Var::create_agg(agg, cg.remit(arg(0)));
    }
    throw std::logic_error("cannot emit lvalue");
}

const Def* MapExpr::remit(CodeGen& cg) const {
    if (auto fn_type = lhs()->type()->isa<FnType>()) {
        auto dst = cg.remit(lhs());
        std::vector<const Def*> defs;
        defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad
        for (auto arg : args())
            defs.push_back(cg.remit(arg));
        defs.front() = cg.get_mem(); // now get the current memory monad

        auto ret_type = args().size() == fn_type->size() ? nullptr : cg.convert(fn_type->return_type());

        auto ret = cg.call(dst, defs, ret_type, loc());
        if (ret_type)
            cg.set_mem(cg.cur_bb->param(0));
        return ret;
    } else if (lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<TupleType>() || lhs()->type()->isa<SimdType>()) {
        auto index = cg.remit(arg(0));
        return cg.extract(cg.remit(lhs()), index, loc());
    }
    THORIN_UNREACHABLE;
}

Var FieldExpr::lemit(CodeGen& cg) const {
    return Var::create_agg(cg.lemit(lhs()), cg.world().literal_qu32(index(), loc()));
}

const Def* FieldExpr::remit(CodeGen& cg) const {
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
        auto fn_mem = w.fn_type({w.mem_type()});
        auto lrun  = w.lambda(w.fn_type({w.mem_type(), fn_mem}), loc(), "run_block");
        auto run = w.run(lrun, loc());
        auto old_bb = cg.cur_bb;
        cg.cur_bb->jump(run, {cg.get_mem(), w.bottom(fn_mem, loc())}, loc());
        cg.cur_bb = lrun;
        cg.set_mem(cg.cur_bb->param(0));
        auto res = BlockExprBase::remit(cg);
        if (cg.is_reachable()) {
            assert(res);
            auto next = w.lambda(fn_mem, loc(), "run_next");
            cg.cur_bb->jump(lrun->param(1), {cg.get_mem()}, loc());
            old_bb->update_arg(1, next);
            cg.cur_bb = next;
            cg.set_mem(cg.cur_bb->param(0));
            assert(res);
            return res;
        }
    }
    return nullptr;
}

#if 0
const Def* RunBlockExpr::remit(CodeGen& cg) const {
    if (cg.is_reachable()) {
        World& w = cg.world();
        auto fn_mem = w.fn_type({w.mem_type()});
        auto lrun  = w.lambda(fn_mem, "run_block");
        auto run = w.run(lrun);
        cg.cur_bb->jump(run, {cg.get_mem()});
        cg.cur_bb = lrun;
        cg.set_mem(cg.cur_bb->param(0));
        auto res = BlockExprBase::remit(cg);
        if (cg.is_reachable()) {
            assert(res);
            auto next = w.lambda(fn_mem, "run_next");
            cg.cur_bb->jump(next, {cg.get_mem()});
            cg.cur_bb = next;
            cg.set_mem(cg.cur_bb->param(0));
            assert(res);
            return res;
        }
    }
    return nullptr;
}
#endif

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
    auto break_lambda = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_lambda, loc().end());
    return cg.world().tuple({}, loc());
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb(cond()->loc().begin(), "while_head"), body_bb(body()->loc().begin(), "while_body");
    auto continue_lambda = cg.create_continuation(continue_decl());

    cg.jump(head_bb, cond()->loc().end());
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_lambda, cond()->loc().end());
    }
    cg.jump(head_bb, cond()->loc().end());
    head_bb.seal();
    cg.enter(exit_bb);
}

const Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const Def*> defs;
    defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_lambda = cg.create_continuation(break_decl());

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
    defs.push_back(break_lambda);
    auto fun = cg.remit(map_expr->lhs());
    if (prefix && prefix->kind() == PrefixExpr::RUN) fun = cg.world().run(fun, loc());
    if (prefix && prefix->kind() == PrefixExpr::HLT) fun = cg.world().hlt(fun, loc());

    defs.front() = cg.get_mem(); // now get the current memory monad
    cg.call(fun, defs, nullptr, map_expr->loc());

    cg.set_continuation(break_lambda);
    if (break_lambda->num_params() == 2)
        return break_lambda->param(1);
    else {
        Array<const Def*> defs(break_lambda->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_lambda->param(i+1);
        return cg.world().tuple(defs, loc());
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto lambda = emit_head(cg, loc());
    emit_body(cg, loc());
    return lambda;
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

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) {
    CodeGen cg(world);
    mod->emit(cg);
    clear_value_numbering_table(world);
}

//------------------------------------------------------------------------------

}
