#include "impala/ast.h"

#include "thorin/irbuilder.h"
#include "thorin/continuation.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

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
        auto result = continuation(convert(decl->type())->as<thorin::FnType>(), decl->location(), decl->symbol().str());
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
            cur_bb->jump(continuation, {get_mem()}, loc);
        set_continuation(continuation);
    }

    Value lemit(const Expr* expr) { return expr->lemit(*this); }
    const Def* remit(const Expr* expr) { return expr->remit(*this); }
    const Def* remit(const Expr* expr, MapExpr::State state, Location eval_loc) {
        return expr->as<MapExpr>()->remit(*this, state, eval_loc);
    }
    void emit_jump(const Expr* expr, JumpTarget& x) { if (is_reachable()) expr->emit_jump(*this, x); }
    void emit_branch(const Expr* expr, JumpTarget& t, JumpTarget& f) { expr->emit_branch(*this, t, f); }
    void emit(const Stmt* stmt) { if (is_reachable()) stmt->emit(*this); }
    void emit(const Item* item) {
        assert(!item->done_);
        item->emit(*this);
#ifndef NDEBUG
        item->done_ = true;
#endif
    }
    void emit(const Ptrn* ptrn, const thorin::Def* def) { ptrn->emit(*this, def); }
    Value emit(const Decl* decl) {
        assert(decl->value_.kind() != thorin::Value::Empty);
        return decl->value_;
    }
    Value emit(const Decl* decl, const Def* init) {
        if (!decl->value_)
            decl->value_ = decl->emit(*this, init);
        return decl->value_;
    }
    const thorin::Type* convert(const Type* type) {
        if (auto t = thorin_type(type))
            return t;
        auto t = convert_rec(type);
        return thorin_type(type) = t;
    }

    void convert_ops(const Type*, std::vector<const thorin::Type*>& nops);
    const thorin::Type* convert_rec(const Type*);

    const thorin::Type*& thorin_type(const Type* type) { return impala2thorin_[type]; }
    const thorin::StructType*& thorin_struct_type(const StructType* type) { return struct_type_impala2thorin_[type]; }

    const Fn* cur_fn = nullptr;
    TypeMap<const thorin::Type*> impala2thorin_;
    GIDMap<const StructType*, const thorin::StructType*> struct_type_impala2thorin_;
};

/*
 * Type
 */

void CodeGen::convert_ops(const Type* type, std::vector<const thorin::Type*>& nops) {
    for (const auto& op : type->ops())
        nops.push_back(convert(op));
}

const thorin::Type* CodeGen::convert_rec(const Type* type) {
    if (auto lambda = type->isa<Lambda>()) {
        return world().lambda(convert(lambda->body()), lambda->name());
    } else if (auto var = type->isa<Var>()) {
        return world().var(var->depth());
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_kind()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world().type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto fn_type = type->isa<FnType>()) {
        std::vector<const thorin::Type*> nops;
        nops.push_back(world().mem_type());
        convert_ops(fn_type, nops);
        return world().fn_type(nops);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Type*> nops;
        convert_ops(tuple_type, nops);
        return world().tuple_type(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world().struct_type(struct_type->struct_decl()->symbol().str(), struct_type->num_ops());
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (const auto& op : struct_type->ops())
            thorin_struct_type(struct_type)->set(i++, convert(op));
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

Value LocalDecl::emit(CodeGen& cg, const Def* init) const {
    auto thorin_type = cg.convert(type());

    if (is_mut()) {
        if (is_address_taken())
            value_ = Value::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), location(), symbol().str()));
        else
            value_ = Value::create_mut(cg, handle(), thorin_type, symbol().str());

        if (init)
            value_.store(init, location());
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
    for (const auto& param : params()) {
        auto p = continuation()->param(i++);
        p->name = param->symbol().str();
        cg.emit(param.get(), p);
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
            for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
                args.push_back(cg.extract(def, i, loc));
            cg.cur_bb->jump(ret_param(), args, loc.end());
        } else
            cg.cur_bb->jump(ret_param(), {mem, def}, loc.end());
    }
}

/*
 * items
 */

void ValueItem::emit(CodeGen& cg) const {
    cg.emit(this, nullptr);
}

void Module::emit(CodeGen& cg) const {
    for (const auto& item : items())
        cg.emit(item.get());
}

static bool is_primop(const Symbol& name) {
    if      (name == "select")   return true;
    else if (name == "sizeof")   return true;
    else if (name == "bitcast")  return true;
    return false;
}

Value FnDecl::emit(CodeGen& cg, const Def*) const {
    // no code is emitted for primops
    if (is_extern() && abi() == "\"thorin\"" && is_primop(symbol()))
        return value_;

    // create thorin function
    value_ = Value::create_val(cg, emit_head(cg, location()));
    if (is_extern() && abi() == "")
        continuation_->make_external();

    // handle main function
    if (symbol() == "main") {
        continuation()->make_external();
    }

    if (body())
        emit_body(cg, location());
    return value_;
}

void ExternBlock::emit(CodeGen& cg) const {
    for (const auto& fn_decl : fn_decls()) {
        cg.emit(fn_decl.get(), nullptr); // TODO use init
        auto continuation = fn_decl->continuation();
        if (abi() == "\"C\"")
            continuation->cc() = thorin::CC::C;
        else if (abi() == "\"device\"")
            continuation->cc() = thorin::CC::Device;
        else if (abi() == "\"thorin\"" && continuation) // no continuation for primops
            continuation->set_intrinsic();
    }
}

void ModuleDecl::emit(CodeGen&) const {
}

void ImplItem::emit(CodeGen& cg) const {
    if (def_)
        return;

    Array<const Def*> args(num_methods());
    for (size_t i = 0, e = args.size(); i != e; ++i) {
        cg.emit(method(i), nullptr); // TODO use init
        args[i] = method(i)->continuation();
    }

    for (size_t i = 0, e = args.size(); i != e; ++i)
        method(i)->emit_body(cg, location());

    def_ = cg.world().tuple(args, location());
}

Value StaticItem::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world().bottom(cg.convert(type()), location()) : cg.remit(this->init());
    if (!is_mut())
        return Value::create_val(cg, init);
    return Value::create_ptr(cg, cg.world().global(init, location(), true, symbol().str()));
}

void StructDecl::emit(CodeGen& cg) const {
    cg.convert(type());
}

void TraitDecl::emit(CodeGen&) const {}
void Typedef::emit(CodeGen&) const {}

/*
 * expressions
 */

Value Expr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* Expr::remit(CodeGen& cg) const { return lemit(cg).load(location()); }
void Expr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    if (auto def = cg.remit(this)) {
        assert(cg.is_reachable());
        cg.cur_bb->set_value(1, def);
        cg.jump(x, location().end());
    } else
        assert(!cg.is_reachable());
}
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f, location().end()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}, location()); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeKind tkind;

    switch (kind()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: tkind = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: tkind = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world().literal(tkind, box(), location());
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world().literal_pu8(value(), location());
}

Value StrExpr::lemit(CodeGen& cg) const {
    return Value::create_ptr(cg, cg.world().global(cg.remit(this), location()));
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world().literal_pu8(values_[i], location());

    return cg.world().definite_array(args, location());
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = cg.remit(src());
    auto thorin_type = cg.convert(type());
    return cg.world().convert(thorin_type, def, location());
}

Value PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl(), nullptr);
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            const Def* def = var.load(location());
            const Def* one = cg.world().one(def->type(), location());
            const Def* ndef = cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one, location());
            var.store(ndef, location());
            return ndef;
        }
        case ADD: return cg.remit(rhs());
        case SUB: return cg.world().arithop_minus(cg.remit(rhs()), location());
        case NOT: return cg.world().arithop_not(cg.remit(rhs()), location());
        case TILDE: {
            auto def = cg.remit(rhs());
            auto ptr = cg.alloc(def->type(), rhs()->extra(), location());
            cg.store(ptr, def, location());
            return ptr;
        }
        case AND: {
            if (rhs()->is_lvalue()) {
                auto var = cg.lemit(rhs());
                assert(var.kind() == Value::PtrRef);
                return var.def();
            }

            auto def = cg.remit(rhs());
            if (is_const(def))
                return cg.world().global(def, location(), false);

            auto slot = cg.world().slot(cg.convert(rhs()->type()), cg.frame(), location());
            cg.store(slot, def, location());
            return slot;

        }

        case RUN: return cg.remit(rhs(), MapExpr::Run, location());
        case HLT: return cg.remit(rhs(), MapExpr::Hlt, location());
        default:  return cg.lemit(this).load(location());
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
        cg.branch(cg.remit(this), t, f, location().end());
}

void InfixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget x(rhs()->location().begin(), "and_extra");
            cg.emit_branch(lhs(), x, f);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        case OROR: {
            JumpTarget x(rhs()->location().begin(), "or_extra");
            cg.emit_branch(lhs(), t, x);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        default:
            cg.branch(cg.remit(this), t, f, location().end());
            return;
    }
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (kind()) {
        case ANDAND: {
            JumpTarget t(lhs()->location().begin(), "and_true"), f(rhs()->location().begin(), "and_false"), x(location().end(), "and_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(rhs(), x);
            if (cg.enter(f)) cg.emit_jump_boolean(false, x, lhs()->location().end());
            return cg.converge(this, x);
        }
        case OROR: {
            JumpTarget t(lhs()->location().begin(), "or_true"), f(rhs()->location().begin(), "or_false"), x(location().end(), "or_exit");
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump_boolean(true, x, rhs()->location().end());
            if (cg.enter(f)) cg.emit_jump(rhs(), x);
            return cg.converge(this, x);
        }
        default:
            const TokenKind op = (TokenKind) kind();

            if (Token::is_assign(op)) {
                Value lvar = cg.lemit(lhs());
                const Def* rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    TokenKind sop = Token::separate_assign(op);
                    rdef = cg.world().binop(Token::to_binop(sop), lvar.load(location()), rdef, location());
                }

                lvar.store(rdef, location());
                return cg.world().tuple({}, location());
            }

            const Def* ldef = cg.remit(lhs());
            const Def* rdef = cg.remit(rhs());
            return cg.world().binop(Token::to_binop(op), ldef, rdef, location());
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    Value var = cg.lemit(lhs());
    const Def* def = var.load(location());
    const Def* one = cg.world().one(def->type(), location());
    var.store(cg.world().arithop(Token::to_arithop((TokenKind) kind()), def, one, location()), location());
    return def;
}

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, location());
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(count());
    std::fill_n(args.begin(), count(), cg.remit(value()));
    return cg.world().definite_array(args, location());
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().tuple(thorin_args, location());
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = cg.remit(dim());
    return cg.world().indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, location());
}

const Def* SimdExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world().vector(thorin_args, location());
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (const auto& elem : elems())
        defs[elem->field_decl()->index()] = cg.remit(elem->expr());
    return cg.world().struct_agg(cg.convert(type())->as<thorin::StructType>(), defs, location());
}

Value TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }

const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const {
    assert(false && "TODO");
    THORIN_UNREACHABLE;
}

Value MapExpr::lemit(CodeGen& cg) const {
    assert(lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<TupleType>() || lhs()->type()->isa<SimdType>());
    auto agg = cg.lemit(lhs());
    return Value::create_agg(agg, cg.remit(arg(0)));
}

const Def* MapExpr::remit(CodeGen& cg) const { return remit(cg, None, Location()); }

const Def* MapExpr::remit(CodeGen& cg, State state, Location eval_loc) const {
    if (auto fn_type = lhs()->type()->isa<FnType>()) {
        const Def* dst = nullptr;

        // Handle primops here
        if (auto type_expr = lhs()->isa<TypeAppExpr>()) { // Bitcast, sizeof and select are all polymorphic
            if (auto path = type_expr->lhs()->isa<PathExpr>()) {
                if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
                    if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
                        auto name = fn_decl->fn_symbol().remove_quotation();
                        if (name == "bitcast") {
                            return cg.world().bitcast(cg.convert(type_expr->type_arg(0)), cg.remit(arg(0)), eval_loc);
                        } else if (name == "select") {
                            return cg.world().select(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), eval_loc);
                        } else if (name == "sizeof") {
                            return cg.world().size_of(cg.convert(type_expr->type_arg(0)), eval_loc);
                        } else if (name == "reserve_shared") {
                            auto ptr_type = cg.convert(type());
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), cg.world().type_qs32(),
                                cg.world().fn_type({ cg.world().mem_type(), ptr_type }) });
                            auto cont = cg.world().continuation(fn_type, location(), "reserve_shared");
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr_type = cg.convert(arg(1)->type());
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), cg.world().type_pu32(), ptr_type, poly_type,
                                cg.world().fn_type({ cg.world().mem_type(), poly_type }) });
                            auto cont = cg.world().continuation(fn_type, location(), "atomic");
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->referenced_type();
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), ptr_type, poly_type, poly_type,
                                cg.world().fn_type({ cg.world().mem_type(), poly_type, cg.world().type_bool() }) });
                            auto cont = cg.world().continuation(fn_type, location(), "cmpxchg");
                            cont->set_intrinsic();
                            dst = cont;
                        }
                    }
                }
            }
        }

        dst = dst ? dst : cg.remit(lhs());

        std::vector<const Def*> defs;
        defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad
        for (const auto& arg : args())
            defs.push_back(cg.remit(arg.get()));
        defs.front() = cg.get_mem(); // now get the current memory monad

        auto ret_type = args().size() == fn_type->num_ops() ? nullptr : cg.convert(fn_type->return_type());
        auto old_bb = cg.cur_bb;
        auto ret = cg.call(dst, defs, ret_type, location());
        if (ret_type)
            cg.set_mem(cg.cur_bb->param(0));

        if (state != None) {
            auto eval = state == Run ? &thorin::World::run : &thorin::World::hlt;
            auto cont = old_bb->args().back();
            old_bb->update_callee((cg.world().*eval)(old_bb->callee(), cont, eval_loc, ""));
        }

        return ret;
    } else if (lhs()->type()->isa<ArrayType>() || lhs()->type()->isa<TupleType>() || lhs()->type()->isa<SimdType>()) {
        auto index = cg.remit(arg(0));
        return cg.extract(cg.remit(lhs()), index, location());
    }
    THORIN_UNREACHABLE;
}

Value FieldExpr::lemit(CodeGen& cg) const {
    auto value = cg.lemit(lhs());
    return Value::create_agg(value, cg.world().literal_qu32(index(), location()));
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.extract(cg.remit(lhs()), index(), location());
}

const Def* BlockExprBase::remit(CodeGen& cg) const {
    for (const auto& stmt : stmts())
        cg.emit(stmt.get());
    return cg.remit(expr());
}

const Def* RunBlockExpr::remit(CodeGen& cg) const {
    if (cg.is_reachable()) {
        World& w = cg.world();
        auto lrun = w.basicblock(location(), "run_block");
        auto next = w.basicblock(location(), "run_next");
        auto old_bb = cg.cur_bb;
        cg.cur_bb->jump(lrun, {}, location());
        cg.enter(lrun);
        auto res = BlockExprBase::remit(cg);
        if (cg.is_reachable()) {
            assert(res);
            cg.cur_bb->jump(next, {}, location());
            cg.enter(next);
            old_bb->update_callee(w.run(lrun, next, location()));
            return res;
        }

        old_bb->update_callee(w.run(lrun, w.bottom(w.fn_type(), location()), location()));
    }
    return nullptr;
}

void IfExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    JumpTarget t(then_expr()->location().begin(), "if_then"), f(else_expr()->location().begin(), "if_else");
    cg.emit_branch(cond(), t, f);
    if (cg.enter(t))
        cg.emit_jump(then_expr(), x);
    if (cg.enter(f))
        cg.emit_jump(else_expr(), x);
    cg.jump(x, location().end());
}

const Def* IfExpr::remit(CodeGen& cg) const {
    JumpTarget x(location().end(), "next");
    return cg.converge(this, x);
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    JumpTarget x(location().end(), "next");
    auto break_continuation = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_continuation, location().end());
    return cg.world().tuple({}, location());
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb(cond()->location().begin(), "while_head"), body_bb(body()->location().begin(), "while_body");
    auto continue_continuation = cg.create_continuation(continue_decl());

    cg.jump(head_bb, cond()->location().end());
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_continuation, cond()->location().end());
    }
    cg.jump(head_bb, cond()->location().end());
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
    for (const auto& arg : map_expr->args())
        defs.push_back(cg.remit(arg.get()));
    defs.push_back(cg.remit(fn_expr()));
    defs.push_back(break_continuation);
    auto fun = cg.remit(map_expr->lhs());
    if (prefix && prefix->kind() == PrefixExpr::RUN) fun = cg.world().run(fun, break_continuation, location());
    if (prefix && prefix->kind() == PrefixExpr::HLT) fun = cg.world().hlt(fun, break_continuation, location());

    defs.front() = cg.get_mem(); // now get the current memory monad
    cg.call(fun, defs, nullptr, map_expr->location());

    cg.set_continuation(break_continuation);
    if (break_continuation->num_params() == 2)
        return break_continuation->param(1);
    else {
        Array<const Def*> defs(break_continuation->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_continuation->param(i+1);
        return cg.world().tuple(defs, location());
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto continuation = emit_head(cg, location());
    emit_body(cg, location());
    return continuation;
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    init->name = local()->symbol().str();
    cg.emit(local(), init);
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        cg.emit(elem(i), cg.extract(init, i, location()));
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
    if (cg.is_reachable())
        cg.emit(ptrn(), init() ? cg.remit(init()) : cg.world().bottom(cg.convert(ptrn()->type()), ptrn()->location()));
}

void AsmStmt::emit(CodeGen& cg) const {
    Array<const thorin::Type*> outs(num_outputs());
    for (size_t i = 0, e = num_outputs(); i != e; ++i)
        outs[i] = cg.convert(output(i)->expr()->type());

    Array<const Def*> ins(num_inputs());
    for (size_t i = 0, e = num_inputs(); i != e; ++i)
        ins[i] = cg.remit(input(i)->expr());

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
            output_constraints(), input_constraints(), clobbers(), flags, location());

    size_t i = 0;
    cg.set_mem(assembly->out(i++));
    for (const auto& output: outputs())
        cg.lemit(output->expr()).store(assembly->out(i++), location());
}

//------------------------------------------------------------------------------

void emit(World& world, const Module* mod) {
    CodeGen cg(world);
    mod->emit(cg);
    clear_value_numbering_table(world);
}

//------------------------------------------------------------------------------

}
