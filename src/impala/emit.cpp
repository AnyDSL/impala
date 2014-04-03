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
    {}

    Var lemit(const Expr* expr) { return is_reachable() ? expr->lemit(*this) : Var(); }
    Def remit(const Expr* expr) { return is_reachable() ? expr->remit(*this) : nullptr; }
    void emit_branch(const Expr* expr, JumpTarget& t, JumpTarget& f) { expr->emit_branch(*this, t, f); }
    void emit(const Stmt* stmt, JumpTarget& exit) { if (is_reachable()) stmt->emit(*this, exit); }
    void emit(const Item* item) { item->emit(*this); }

    void split(JumpTarget& jt, JumpTarget& x, std::function<Def()> def) {
        if (auto lambda = enter(jt)) {
            lambda->set_value(1, def());
            jump(x);
        }
    }
    Def converge(JumpTarget& x, const thorin::Type* type, const char* name) {
        if (auto lambda = enter(x))
            return lambda->get_value(1, type, name);
        return Def();
    }

    const Enter* frame;
};

Var LocalDecl::var(CodeGen& cg) const {
    if (!var_) {
        auto thorin_type = type()->convert(cg.world());
        if (is_address_taken())
            return var_ = Var(cg, cg.world().slot(thorin_type, cg.frame, handle(), symbol().str()));
        return var_ = Var(cg, handle(), thorin_type, symbol().str());
    }
    return var_;
}

/*
 * Type
 */

void TypeNode::convert_elems(World& world, std::vector<const thorin::Type*>& nelems) const {
    for (auto elem : elems())
        nelems.push_back(elem->convert(world));
}

const thorin::Type* PrimTypeNode::convert(World& world) const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, ttype) \
        case PrimType_##itype: return world.type_##ttype();
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const thorin::Type* FnTypeNode::convert(World& world) const { 
    std::vector<const thorin::Type*> nelems;
    nelems.push_back(world.mem());
    convert_elems(world, nelems);
    return world.pi(nelems); 
}

const thorin::Type* TupleTypeNode::convert(World& world) const { 
    std::vector<const thorin::Type*> nelems;
    convert_elems(world, nelems);
    return world.sigma(nelems);
}

/*
 * Item
 */

void ModContents::emit(CodeGen& cg) const {
    for (auto item : items())
        cg.emit(item);
}

void FnDecl::emit(CodeGen& cg) const {
    // create thorin function
    auto pi = type()->convert(cg.world())->as<thorin::Pi>();
    lambda_ = cg.world().lambda(pi, symbol().str());
    if (is_extern())
        lambda_->attribute().set(Lambda::Extern);

    // setup function nest
    lambda()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_bb, lambda());

    // handle main function
    if (symbol() == Symbol("main")) {
        lambda()->name += "_impala";
        lambda()->attribute().set(Lambda::Extern);
    }

    // setup builtin functions
    if (lambda()->name == "nvvm")
        lambda()->attribute().set(Lambda::NVVM);
    if (lambda()->name == "spir")
        lambda()->attribute().set(Lambda::SPIR);
    if (lambda()->name == "array")
        lambda()->attribute().set(Lambda::ArrayInit);
    if (lambda()->name == "vectorized")
        lambda()->attribute().set(Lambda::Vectorize);
    if (lambda()->name == "wfv_get_tid")
        lambda()->attribute().set(Lambda::VectorizeTid | Lambda::Extern);

    // setup memory + frame
    auto mem = lambda()->param(0);
    mem->name = "mem";
    cg.set_mem(mem);
    cg.frame = frame_ = cg.world().enter(mem); // TODO

    // name params and setup store locations
    for (size_t i = 0, e = params().size(); i != e; ++i) {
        auto p = lambda()->param(i+1);
        p->name = param(i)->symbol().str();
        param(i)->var(cg).store(p);
    }
    ret_param_ = lambda()->params().back();

    // descent into body
    auto def = cg.remit(body());
    if (def) {
        if (auto sigma = def->type()->isa<thorin::Sigma>()) {
            std::vector<Def> args;
            args.push_back(mem);
            for (size_t i = 0, e = sigma->size(); i != e; ++i)
                args.push_back(cg.world().extract(def, i));
            cg.cur_bb->jump(ret_param_, args);
        } else
            cg.cur_bb->jump(ret_param_, {mem, def});
    }
}

void ForeignMod::emit(CodeGen& cg) const {
}

void ModDecl::emit(CodeGen& cg) const {
}

void Impl::emit(CodeGen& cg) const {
}

void StaticItem::emit(CodeGen& cg) const {
}

void StructDecl::emit(CodeGen& cg) const {
}

void TraitDecl::emit(CodeGen& cg) const {
}

void Typedef::emit(CodeGen& cg) const {
}

/*
 * Expr
 */

Var Expr::lemit(CodeGen& cg) const { THORIN_UNREACHABLE; }
Def Expr::remit(CodeGen& cg) const { return lemit(cg).load(); }
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f); }
Def EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}); }

Def BlockExpr::remit(CodeGen& cg) const {
    for (auto stmt : stmts()) {
        JumpTarget stmt_exit_bb("next");
        cg.emit(stmt, stmt_exit_bb);
        cg.enter(stmt_exit_bb);
    }
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
    return value_decl()->isa<LocalDecl>()->var(cg);
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
    if (kind() == NOT && type()->convert(cg.world())->is_bool())
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
            cg.split(t, x, [&] { return cg.remit(rhs()); });
            cg.split(f, x, [&] { return cg.world().literal(false); });
            return cg.converge(x, cg.world().type_bool(), "and");
        }
        case OROR: {
            JumpTarget t("or_true"), f("or_false"), x("or_exit");
            cg.emit_branch(lhs(), t, f);
            cg.split(t, x, [&] { return cg.world().literal(true); });
            cg.split(f, x, [&] { return cg.remit(rhs()); });
            return cg.converge(x, cg.world().type_bool(), "or");
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

Def IfExpr::remit(CodeGen& cg) const {
    JumpTarget t("if_then"), f("if_else"), x("if_next");
    cg.emit_branch(cond(), t, f);
    cg.split(t, x, [&] { return cg.remit(then_expr()); });
    cg.split(f, x, [&] { return cg.remit(else_expr()); });
    return cg.converge(x, then_expr()->type()->convert(cg.world()), "if");
}

/*
 * Stmt
 */

void ExprStmt::emit(CodeGen& cg, JumpTarget& exit_bb) const {
    if (cg.is_reachable()) {
        cg.remit(expr());
        cg.jump(exit_bb);
    }
}

void ItemStmt::emit(CodeGen& cg, JumpTarget& exit_bb) const { 
    cg.emit(item()); 
    cg.jump(exit_bb); 
}

void LetStmt::emit(CodeGen& cg, JumpTarget& exit_bb) const {
    if (cg.is_reachable()) {
        auto var = local()->var(cg);
        if (init()) {
            auto def = cg.remit(init());
            def->name = local()->symbol().str();
            var.store(def);
        }
        cg.jump(exit_bb);
    }
}

//------------------------------------------------------------------------------

void emit(World& world, const ModContents* mod) { 
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
