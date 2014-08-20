#include "impala/ast.h"

#include <iostream>
#include <stdexcept>
#include <vector>

#include "thorin/irbuilder.h"
#include "thorin/lambda.h"
#include "thorin/literal.h"
#include "thorin/memop.h"
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
        , cur_fn(nullptr)
    {}

    const thorin::Enter* frame() const { assert(cur_fn); return cur_fn->frame(); }
    /// Enter \p x and perform \p get_value to collect return value.
    Def converge(const Expr* expr, JumpTarget& x) {
        emit_jump(expr, x);
        if (enter(x))
            return cur_bb->get_value(1, convert(expr->type()));
        return Def();
    }

    void emit_jump(bool val, JumpTarget& x) {
        if (is_reachable()) {
            cur_bb->set_value(1, world().literal(val));
            jump(x);
        }
    }

    Lambda* create_continuation(const LocalDecl* decl) {
        auto result = world().lambda(convert(decl->type()).as<thorin::FnType>(), decl->symbol().str());
        result->param(0)->name = "mem";
        decl->var_ = Var::create_val(*this, result);
        return result;
    }

    void set_continuation(Lambda* lambda) {
        cur_bb = lambda;
        set_mem(lambda->param(0));
    }

    void jump_to_continuation(Lambda* lambda) {
        if (is_reachable())
            cur_bb->jump(lambda, {get_mem()});
        set_continuation(lambda);
    }

    Var lemit(const Expr* expr) { return expr->lemit(*this); }
    Def remit(const Expr* expr) { return expr->remit(*this); }
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
    Var emit(const ValueDecl* decl, Def init) {
        if (!decl->var_)
            decl->var_ = decl->emit(*this, init);
        return decl->var_;
    }
    thorin::Type convert(const Unifiable* uni) {
        auto unifiable = uni->unify();
        if (!unifiable->thorin_type_) {
            for (auto type_var : unifiable->type_vars())    // convert type vars
                type_var->thorin_type_ = world().type_var();

            unifiable->thorin_type_ = unifiable->convert(*this);

            for (auto type_var : unifiable->type_vars())    // bind type vars
                unifiable->thorin_type_->bind(convert(type_var).as<thorin::TypeVar>());
        }
        return unifiable->thorin_type_;
    }
    template<class T> thorin::Type convert(Proxy<T> type) { return convert(type->unify()); }

    void end_eval(Lambda* prev) {
        if (auto run = prev->to()->isa<thorin::Run>())
            prev->update_arg(prev->num_args()-1, world().end_run(prev->args().back(), run));
        else if (auto hlt = prev->to()->isa<thorin::Hlt>())
            prev->update_arg(prev->num_args()-1, world().end_hlt(prev->args().back(), hlt));
    }

    const Fn* cur_fn;
};

/*
 * Type
 */

void Unifiable::convert_args(CodeGen& cg, std::vector<thorin::Type>& nargs) const {
    for (auto arg : args())
        nargs.push_back(cg.convert(arg));
}

thorin::Type PrimTypeNode::convert(CodeGen& cg) const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, ttype) \
        case PrimType_##itype: return cg.world().type_##ttype();
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

thorin::Type NoRetTypeNode::convert(CodeGen& cg) const { return thorin::Type(); }

thorin::Type FnTypeNode::convert(CodeGen& cg) const {
    std::vector<thorin::Type> nargs;

    nargs.push_back(cg.world().mem_type());

    for (auto type_var : type_vars()) {
        Array<thorin::Type> bounds(type_var->num_bounds());
        for (size_t j = 0, e = bounds.size(); j != e; ++j)
            bounds[j] = cg.convert(type_var->bound(j));

        nargs.push_back(cg.world().tuple_type(bounds));
    }

    convert_args(cg, nargs);
    return cg.world().fn_type(nargs);
}

thorin::Type TupleTypeNode::convert(CodeGen& cg) const {
    std::vector<thorin::Type> nargs;
    convert_args(cg, nargs);
    return cg.world().tuple_type(nargs);
}

thorin::Type StructAbsTypeNode::convert(CodeGen& cg) const {
    thorin_type_ = thorin_struct_abs_type_ = cg.world().struct_abs_type(num_args(), struct_decl()->symbol().str());
    size_t i = 0;
    for (auto arg : args())
        thorin_struct_abs_type_->set(i++, cg.convert(arg));
    thorin_type_.clear();               // will be set again by CodeGen's wrapper
    return thorin_struct_abs_type_;
}

thorin::Type StructAppTypeNode::convert(CodeGen& cg) const {
    std::vector<thorin::Type> nargs;
    convert_args(cg, nargs);
    return cg.world().struct_app_type(cg.convert(struct_abs_type()).as<thorin::StructAbsType>(), nargs);
}

thorin::Type TraitAbsNode::convert(CodeGen& cg) const {
    std::vector<thorin::Type> args;

    for (auto super_trait : super_traits())
        args.push_back(cg.convert(super_trait));

    for (auto method : trait_decl()->methods())
        args.push_back(cg.convert(method->type()));

    return cg.world().tuple_type(args);
}

thorin::Type TraitAppNode::convert(CodeGen& cg) const {
    Array<thorin::Type> nargs(num_args());
    for (size_t i = 0, e = nargs.size(); i != e; ++i)
        nargs[i] = cg.convert(arg(i));
    return cg.convert(trait())->instantiate(nargs);
}

thorin::Type ImplNode::convert(CodeGen& cg) const { THORIN_UNREACHABLE; }
thorin::Type PtrTypeNode::convert(CodeGen& cg) const { return cg.world().ptr_type(cg.convert(referenced_type())); }
thorin::Type DefiniteArrayTypeNode::convert(CodeGen& cg) const { return cg.world().definite_array_type(cg.convert(elem_type()), dim()); }
thorin::Type IndefiniteArrayTypeNode::convert(CodeGen& cg) const { return cg.world().indefinite_array_type(cg.convert(elem_type())); }

/*
 * Decls and Function
 */

Var LocalDecl::emit(CodeGen& cg, Def init) const {
    auto thorin_type = cg.convert(type());
    if (!init)
        init = cg.world().bottom(thorin_type);
    if (!is_mut())
        return var_ = Var::create_val(cg, init);

    if (is_address_taken())
        var_ = Var::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), handle(), symbol().str()));
    else
        var_ = Var::create_mut(cg, handle(), thorin_type, symbol().str()); // TODO

    var_.store(init);
    return var_;
}

Lambda* Fn::emit_head(CodeGen& cg) const {
    assert(lambda_ == nullptr);
    auto symbol = fn_symbol();
    std::string str(symbol.str());
    if (!str.empty() && str.front() == '"') { // remove quotation
        assert(str.size() >= 2 && str.back() == '"');
        str = str.substr(1, str.size()-2);
    }
    return lambda_ = cg.world().lambda(cg.convert(fn_type()).as<thorin::FnType>(), str);
}

void Fn::emit_body(CodeGen& cg) const {
    // setup function nest
    lambda()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, lambda());

    // setup memory + frame
    size_t i = 0;
    Def mem_param = lambda()->param(i++);
    mem_param->name = "mem";
    cg.set_mem(mem_param);
    frame_ = cg.world().enter(mem_param);

    // name bounds and memoize type params
    for (auto type_param : type_params()) {
        auto param = lambda()->param(i++);
        param->name = type_param->symbol().str();
        type_param->type_var()->defs_.push(param);
    }

    // name params and setup store locations
    for (auto param : params()) {
        auto p = lambda()->param(i++);
        p->name = param->symbol().str();
        cg.emit(param, p);
    }
    assert(i == lambda()->num_params());
    if (lambda()->num_params() != 0 && lambda()->params().back()->type().isa<thorin::FnType>())
        ret_param_ = lambda()->params().back();

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        Def mem = cg.world().leave(cg.get_mem(), frame());
        cg.set_mem(mem);

        if (auto tuple = def->type().isa<thorin::TupleType>()) {
            std::vector<Def> args;
            args.push_back(mem);
            for (size_t i = 0, e = tuple->num_args(); i != e; ++i)
                args.push_back(cg.world().extract(def, i));
            cg.cur_bb->jump(ret_param(), args);
        } else
            cg.cur_bb->jump(ret_param(), {mem, def});
    }

    // pop type_param stacks
    for (auto type_param : type_params())
        type_param->type_var()->defs_.pop();
}

/*
 * items
 */

void ValueItem::emit_item(CodeGen& cg) const {
    cg.emit(static_cast<const ValueDecl*>(this), Def());
}

void ModContents::emit(CodeGen& cg) const {
    for (auto item : items())
        cg.emit(item);
}

Var FnDecl::emit(CodeGen& cg, Def init) const {
    // create thorin function
    var_ = Var::create_val(cg, emit_head(cg));
    if (is_extern())
        lambda_->attribute().set(Lambda::Extern);

    // handle main function
    if (symbol() == Symbol("main")) {
        lambda()->name += "_impala";
        lambda()->attribute().set(Lambda::Extern);
    }

    if (body())
        emit_body(cg);
    return var_;
}

void ExternBlock::emit_item(CodeGen& cg) const {
    for (auto fn : fns()) {
        cg.emit(static_cast<const ValueDecl*>(fn), Def()); // TODO use init
        auto lambda = fn->lambda();
        if (abi() == Symbol("\"C\""))
            lambda->attribute().set(Lambda::Extern);
        else if (abi() == Symbol("\"device\""))
            lambda->attribute().set(Lambda::Device);
        else if (abi() == Symbol("\"thorin\""))
            lambda->set_intrinsic();
    }
}

void ModDecl::emit_item(CodeGen& cg) const {
}

void ImplItem::emit_item(CodeGen& cg) const {
    if (def_)
        return;

    Array<thorin::Def> args(num_methods());
    for (size_t i = 0, e = args.size(); i != e; ++i) {
        cg.emit(static_cast<const ValueDecl*>(method(i)), Def()); // TODO use init
        args[i] = method(i)->lambda();
    }

    for (size_t i = 0, e = args.size(); i != e; ++i)
        method(i)->emit_body(cg);

    def_ = cg.world().tuple(args);
}

Var StaticItem::emit(CodeGen& cg, Def init) const {
    assert(!init);
    init = cg.remit(this->init());
    if (!is_mut())
        return Var::create_val(cg, init);
    return Var::create_ptr(cg, cg.world().global(init, true, symbol().str()));
}

void StructDecl::emit_item(CodeGen& cg) const {
    cg.convert(type());
}

void TraitDecl::emit_item(CodeGen& cg) const {
    cg.convert(trait_abs());
}

void Typedef::emit_item(CodeGen& cg) const {
}

/*
 * expressions
 */

Var Expr::lemit(CodeGen& cg) const { throw std::logic_error("cannot emit lvalue"); }
Def Expr::remit(CodeGen& cg) const { return lemit(cg).load(); }
void Expr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    if (auto def = cg.remit(this)) {
        assert(cg.is_reachable());
        cg.cur_bb->set_value(1, def);
        cg.jump(x);
    } else
        assert(!cg.is_reachable());
}
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f); }
Def EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}); }

Def LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeKind tkind;

    switch (kind()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: tkind = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: tkind = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world().literal(tkind, box());
}

Def CastExpr::remit(CodeGen& cg) const {
    auto def = cg.remit(lhs());
    auto thorin_type = cg.convert(ast_type()->type());
    if (this->type().isa<PtrType>() && lhs()->type().isa<PtrType>())
        return cg.world().bitcast(def, thorin_type);
    return cg.world().cast(def, thorin_type);
}

Var PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl(), Def());
}

Def PrefixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            Def def = var.load();
            Def one = cg.world().one(def->type());
            Def ndef = cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one);
            var.store(ndef);
            return ndef;
        }
        case ADD: return cg.remit(rhs());
        case SUB: return cg.world().arithop_minus(cg.remit(rhs()));
        case NOT: return cg.world().arithop_not(cg.remit(rhs()));
        case TILDE: {
            auto def = cg.remit(rhs());
            auto mem = cg.get_mem();
            auto ptr = rhs()->extra() ? cg.world().alloc(mem, def->type(), rhs()->extra()) : cg.world().alloc(mem, def->type());
            cg.set_mem(cg.world().store(mem, ptr, def));
            return ptr;
        }
        case AND: {
            auto var = cg.lemit(rhs());
            assert(var.kind() == Var::PtrRef);
            return var.def();
        }
        case RUN: return cg.world().run(cg.remit(rhs()));
        case HLT: return cg.world().hlt(cg.remit(rhs()));
        default:  return cg.lemit(this).load();
    }
}

Var PrefixExpr::lemit(CodeGen& cg) const {
    if (kind() == MUL)
        return Var::create_ptr(cg, cg.remit(rhs()));
    throw std::logic_error("cannot emit lvalue");
}

void PrefixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    if (kind() == NOT && cg.convert(type())->is_bool())
        cg.emit_branch(rhs(), f, t);
    else
        cg.branch(cg.remit(rhs()), t, f);
}

void InfixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget x("and_extra");
            cg.emit_branch(lhs(), x, f);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        case OROR: {
            JumpTarget x("or_extra");
            cg.emit_branch(lhs(), t, x);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        default:
            cg.branch(cg.remit(this), t, f);
            return;
    }
}

Def InfixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget t("and_true"), f("and_false"), x("and_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(lhs(), x);
            if (cg.enter(f)) cg.emit_jump(false, x);
            return cg.converge(this, x);
        }
        case OROR: {
            JumpTarget t("or_true"), f("or_false"), x("or_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(true, x);
            if (cg.enter(f)) cg.emit_jump(rhs(), x);
            return cg.converge(this, x);
        }
        default:
            const TokenKind op = (TokenKind) kind();

            if (Token::is_assign(op)) {
                Var lvar = cg.lemit(lhs());
                Def rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    TokenKind sop = Token::separate_assign(op);
                    rdef = cg.world().binop(Token::to_binop(sop), lvar.load(), rdef);
                }

                lvar.store(rdef);
                return cg.world().tuple({});
            }

            Def ldef = cg.remit(lhs());
            Def rdef = cg.remit(rhs());
            return cg.world().binop(Token::to_binop(op), ldef, rdef);
    }
}

Def PostfixExpr::remit(CodeGen& cg) const {
    Var var = cg.lemit(lhs());
    Def def = var.load();
    Def one = cg.world().one(def->type());
    var.store(cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one));
    return def;
}

Def DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<Def> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().definite_array(cg.convert(type()).as<thorin::DefiniteArrayType>()->elem_type(), thorin_args);
}

Def RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    return Def();
}

Def TupleExpr::remit(CodeGen& cg) const {
    Array<Def> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().tuple(thorin_args);
}

Def IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = cg.remit(dim());
    return cg.world().indefinite_array(cg.convert(type()).as<thorin::IndefiniteArrayType>()->elem_type(), extra_);
}

Def StructExpr::remit(CodeGen& cg) const {
    Array<Def> defs(num_elems());
    for (const auto& elem : elems())
        defs[elem.field_decl()->index()] = cg.remit(elem.expr());
    return cg.world().struct_agg(cg.convert(type()).as<thorin::StructAppType>(), defs);
}

Var MapExpr::lemit(CodeGen& cg) const {
    if (lhs()->type().isa<ArrayType>() || lhs()->type().isa<TupleType>())
        return Var::create_agg(cg.lemit(lhs()), cg.remit(arg(0)));
    throw std::logic_error("cannot emit lvalue");
}

Def MapExpr::remit(CodeGen& cg) const {
    if (auto fn = lhs()->type().isa<FnType>()) {
        Def ldef = cg.remit(lhs());
        assert(fn->num_type_vars() == num_inferred_args());
        std::vector<Def> defs;
        defs.push_back(Def());      // reserve for mem but set later - some other args may update the monad
        for (size_t i = 0, e = fn->num_type_vars(); i != e; ++i) {
            if (auto type_var = inferred_arg(i).isa<TypeVar>())
                defs.push_back(type_var->defs_.top());
            else {
                auto known_type = inferred_arg(i).as<KnownType>();
                std::vector<Def> bounds;
                for (auto bound : fn->type_var(i)->bounds()) {
                    auto impl = known_type->find_impl(bound);
                    cg.emit(impl->impl_item());
                    bounds.push_back(impl->impl_item()->def());
                }
                defs.push_back(cg.world().tuple(bounds));
            }
        }

        for (auto arg : args())
            defs.push_back(cg.remit(arg));
        defs.front() = cg.get_mem(); // now get the current memory monad

        auto ret_type = args().size() == fn->num_args() ? thorin::Type() : cg.convert(fn->return_type());
        auto prev = cg.cur_bb;
        auto ret = cg.call(ldef, defs, ret_type);
        if (ret_type) {
            cg.set_mem(cg.cur_bb->param(0));
            cg.end_eval(prev);
        }
        return ret;
    } else if (lhs()->type().isa<ArrayType>() || lhs()->type().isa<TupleType>()) {
        auto index = cg.remit(arg(0));
        return cg.world().extract(cg.remit(lhs()), index);
    }
    THORIN_UNREACHABLE;
}

Var FieldExpr::lemit(CodeGen& cg) const {
    return Var::create_agg(cg.lemit(lhs()), cg.world().literal_qu32(index()));
}

Def FieldExpr::remit(CodeGen& cg) const {
    return cg.world().extract(cg.remit(lhs()), index());
}

Def BlockExpr::remit(CodeGen& cg) const {
    for (auto stmt : stmts())
        cg.emit(stmt);
    return cg.remit(expr());
}

void IfExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    JumpTarget t("if_then"), f("if_else");
    cg.emit_branch(cond(), t, f);
    if (cg.enter(t))
        cg.emit_jump(then_expr(), x);
    if (cg.enter(f))
        cg.emit_jump(else_expr(), x);
    cg.jump(x);
}

Def IfExpr::remit(CodeGen& cg) const {
    JumpTarget x("next");
    return cg.converge(this, x);
}

Def WhileExpr::remit(CodeGen& cg) const {
    JumpTarget x("next");
    auto break_lambda = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_lambda);
    return cg.world().tuple({});
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb("while_head"), body_bb("while_body");
    auto continue_lambda = cg.create_continuation(continue_decl());

    cg.jump(head_bb);
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_lambda);
    }
    cg.jump(head_bb);
    head_bb.seal();
    cg.enter(exit_bb);
}

Def ForExpr::remit(CodeGen& cg) const {
    std::vector<Def> defs;
    defs.push_back(cg.get_mem());

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
    if (prefix && prefix->kind() == PrefixExpr::RUN) fun = cg.world().run(fun);
    if (prefix && prefix->kind() == PrefixExpr::HLT) fun = cg.world().hlt(fun);

    auto prev = cg.cur_bb;
    cg.call(fun, defs, thorin::Type());
    cg.end_eval(prev);

    cg.set_continuation(break_lambda);
    if (break_lambda->num_params() == 2)
        return break_lambda->param(1);
    else {
        Array<Def> defs(break_lambda->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_lambda->param(i+1);
        return cg.world().tuple(defs);
    }
}

Def FnExpr::remit(CodeGen& cg) const {
    auto lambda = emit_head(cg);
    emit_body(cg);
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
            cg.emit(local(), Def());
    }
}

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) {
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
