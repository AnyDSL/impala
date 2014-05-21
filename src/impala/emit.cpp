#include "impala/ast.h"

#include <iostream>
#include <vector>
#include <map>

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
    Var lemit(const Expr* expr) { return is_reachable() ? expr->lemit(*this) : Var(); }
    Def remit(const Expr* expr) { return is_reachable() ? expr->remit(*this) : Def(); }
    void emit_jump(const Expr* expr, JumpTarget& x) { if (is_reachable()) expr->emit_jump(*this, x); }
    void emit_branch(const Expr* expr, JumpTarget& t, JumpTarget& f) { expr->emit_branch(*this, t, f); }
    void emit(const Stmt* stmt) { if (is_reachable()) stmt->emit(*this); }
    void emit(const Item* item) { item->emit_item(*this); }
    Var emit(const ValueDecl* decl) { 
        if (!decl->var_)
            decl->var_ = decl->emit(*this); 
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

    const Fn* cur_fn;
};

/*
 * Type
 */

void KnownTypeNode::convert_elems(CodeGen& cg, std::vector<thorin::Type>& nelems) const {
    for (auto elem : elems())
        nelems.push_back(cg.convert(elem));
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
    std::vector<thorin::Type> nelems;

    nelems.push_back(cg.world().mem_type());

    for (auto type_var : type_vars()) {
        Array<thorin::Type> bounds(type_var->num_bounds());
        for (size_t j = 0, e = bounds.size(); j != e; ++j)
            bounds[j] = cg.convert(type_var->bound(j));

        nelems.push_back(cg.world().tuple_type(bounds));
    }

    convert_elems(cg, nelems);
    return cg.world().fn_type(nelems); 
}

thorin::Type TupleTypeNode::convert(CodeGen& cg) const { 
    std::vector<thorin::Type> nelems;
    convert_elems(cg, nelems);
    return cg.world().tuple_type(nelems);
}

thorin::Type StructTypeNode::convert(CodeGen& cg) const {
    auto struct_type = cg.world().struct_type(num_elems());

    return struct_type;
}

thorin::Type TraitNode::convert(CodeGen& cg) const {
    std::vector<thorin::Type> elems;

    for (auto super_bound : super_bounds())
        elems.push_back(cg.convert(super_bound));

    for (auto method : trait_decl()->methods())
        elems.push_back(cg.convert(method->type()));

    return cg.world().tuple_type(elems);
}

thorin::Type BoundNode::convert(CodeGen& cg) const {
    Array<thorin::Type> args(num_type_args());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.convert(type_arg(i));
    return cg.convert(trait())->instantiate(args);
}

thorin::Type ImplNode::convert(CodeGen& cg) const { THORIN_UNREACHABLE; }

/*
 * Decls and Function
 */

Var LocalDecl::emit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());
    if (is_address_taken())
        return var_ = Var(cg, cg.world().slot(thorin_type, cg.frame(), handle(), symbol().str()));
    return var_ = Var(cg, handle(), thorin_type, symbol().str());
}

Lambda* Fn::emit_head(CodeGen& cg) const {
    return lambda_ = cg.world().lambda(cg.convert(fn_type()).as<thorin::FnType>(), fn_symbol().str());
}

void Fn::emit_body(CodeGen& cg) const {
    // setup function nest
    lambda()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, lambda());

    // setup memory + frame
    size_t i = 0;
    Def mem = lambda()->param(i++);
    mem->name = "mem";
    cg.set_mem(mem);
    frame_ = cg.world().enter(mem); 

    // name bounds
    for (auto type_param : type_params())
        lambda()->param(i++)->name = type_param->symbol().str();

    // name params and setup store locations
    for (auto param : params()) {
        auto p = lambda()->param(i++);
        p->name = param->symbol().str();
        cg.emit(param).store(p);
    }
    assert(i == lambda()->num_params());
    if (lambda()->num_params() != 0 && lambda()->params().back()->type().isa<thorin::FnType>())
        ret_param_ = lambda()->params().back();

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        mem = cg.world().leave(cg.get_mem(), frame());
        if (auto sigma = def->type().isa<thorin::TupleType>()) {
            std::vector<Def> args;
            args.push_back(mem);
            for (size_t i = 0, e = sigma->size(); i != e; ++i)
                args.push_back(cg.world().extract(def, i));
            cg.cur_bb->jump(ret_param(), args);
        } else
            cg.cur_bb->jump(ret_param(), {mem, def});
    }
}

/*
 * items
 */


void ValueItem::emit_item(CodeGen& cg) const { cg.emit(static_cast<const ValueDecl*>(this)); }

void ModContents::emit(CodeGen& cg) const {
    for (auto item : items())
        cg.emit(item);
}

Var FnDecl::emit(CodeGen& cg) const {
    // create thorin function
    var_ = Var(cg, emit_head(cg));
    if (is_extern())
        lambda_->attribute().set(Lambda::Extern);

    // handle main function
    if (symbol() == Symbol("main")) {
        lambda()->name += "_impala";
        lambda()->attribute().set(Lambda::Extern);
    }

    // setup builtin functions
    if (lambda()->name == "nvvm")
        lambda()->attribute().set(Lambda::NVVM);
    else if (lambda()->name == "spir")
        lambda()->attribute().set(Lambda::SPIR);
    else if (lambda()->name == "array")
        lambda()->attribute().set(Lambda::ArrayInit);
    else if (lambda()->name == "vectorized")
        lambda()->attribute().set(Lambda::Vectorize);
    else if (lambda()->name == "wfv_get_tid")
        lambda()->attribute().set(Lambda::VectorizeTid | Lambda::Extern);

    emit_body(cg);
    return var_;
}

void ForeignMod::emit_item(CodeGen& cg) const {
}

void ModDecl::emit_item(CodeGen& cg) const {
}

void ImplItem::emit_item(CodeGen& cg) const {
    assert(!def_);
    Array<thorin::Def> elems(num_methods());
    for (size_t i = 0, e = elems.size(); i != e; ++i)
        elems[i] = method(i)->emit_head(cg);

    for (size_t i = 0, e = elems.size(); i != e; ++i)
        method(i)->emit_body(cg);

    def_ = cg.world().tuple(elems);
}

Var StaticItem::emit(CodeGen& cg) const {
    return Var(); // TODO
}

Var FieldDecl::emit(CodeGen&) const {
    return Var(); // TODO
}

void StructDecl::emit_item(CodeGen& cg) const {
}

void TraitDecl::emit_item(CodeGen& cg) const {
    cg.convert(trait());
}

void Typedef::emit_item(CodeGen& cg) const {
}

/*
 * expressions
 */

Var Expr::lemit(CodeGen& cg) const { THORIN_UNREACHABLE; }
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

Def BlockExpr::remit(CodeGen& cg) const {
    for (auto stmt : stmts())
        cg.emit(stmt);
    return cg.remit(expr());
}

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

Var PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl());
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
        default: THORIN_UNREACHABLE;
    }
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

Def MapExpr::remit(CodeGen& cg) const {
    Def ldef = cg.remit(lhs());

    if (auto fn = lhs()->type().isa<FnType>()) {
        std::vector<Def> defs;
        defs.push_back(cg.get_mem());
        //for (auto type_var : type_vars()) {
            //for (auto bound : type_var->bounds())
        //}

        for (auto arg : args())
            defs.push_back(cg.remit(arg));
        auto ret_type = args().size() == fn->num_elems() ? thorin::Type() : cg.convert(fn->return_type());
        return cg.call(ldef, defs, ret_type);
    } else {
        assert(false && "TODO");
        return Def();
    }
}

Def ForExpr::remit(CodeGen& cg) const {
    std::vector<Def> defs;
    defs.push_back(cg.get_mem());

    // prepare break continuation
    auto next = cg.world().lambda(cg.world().fn_type({cg.world().mem_type()}), "break");
    break_->var_ = Var(cg, next);

    // emit call
    auto map_expr = expr()->as<MapExpr>();
    for (auto arg : map_expr->args())
        defs.push_back(cg.remit(arg));
    defs.push_back(cg.remit(fn_expr()));
    defs.push_back(next);
    cg.call(cg.remit(map_expr->lhs()), defs, thorin::Type());

    // go to break continuation
    cg.cur_bb = next;
    cg.set_mem(next->param(0));
    return cg.world().tuple({});
}

Def FnExpr::remit(CodeGen& cg) const {
    auto lambda = emit_head(cg);
    emit_body(cg);
    return lambda;
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
        auto var = cg.emit(local());
        if (init()) {
            auto def = cg.remit(init());
            def->name = local()->symbol().str();
            var.store(def);
        }
    }
}

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) { 
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
