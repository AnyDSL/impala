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
        , empty_fn_type(world.fn_type({ world.mem_type() }))
    {}

    const Def* frame() const { assert(cur_fn); return cur_fn->frame(); }
    /// Enter \p x and perform \p get_value to collect return value.
    const Def* converge(const Expr* expr, JumpTarget& x) {
        emit_jump(expr, x);
        if (enter(x))
            return cur_bb->get_value(1, convert(expr->type()), { expr->location(), "converge" });
        return nullptr;
    }

    void emit_jump_boolean(bool val, JumpTarget& x, const thorin::Location& loc) {
        if (is_reachable()) {
            cur_bb->set_value(1, world().literal(val, loc));
            jump(x, loc);
        }
    }

    Continuation* create_continuation(const LocalDecl* decl) {
        auto result = continuation(convert(decl->type())->as<thorin::FnType>(), decl->debug());
        result->param(0)->debug().set("mem");
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
        assert(decl->value_.tag() != thorin::Value::Empty);
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

    const thorin::Type* convert_rec(const Type*);

    const thorin::Type*& thorin_type(const Type* type) { return impala2thorin_[type]; }
    const thorin::StructType*& thorin_struct_type(const StructType* type) { return struct_type_impala2thorin_[type]; }
    const thorin::StructType*& thorin_enum_type(const EnumType* type) { return enum_type_impala2thorin_[type]; }

    const Fn* cur_fn = nullptr;
    const thorin::Type* empty_fn_type;
    TypeMap<const thorin::Type*> impala2thorin_;
    GIDMap<const StructType*, const thorin::StructType*> struct_type_impala2thorin_;
    GIDMap<const EnumType*,   const thorin::StructType*> enum_type_impala2thorin_;
};

/*
 * Type
 */

const thorin::Type* CodeGen::convert_rec(const Type* type) {
    if (auto lambda = type->isa<Lambda>()) {
        return world().lambda(convert(lambda->body()), lambda->name());
    } else if (auto var = type->isa<Var>()) {
        return world().var(var->depth());
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world().type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto fn_type = type->isa<FnType>()) {
        return world().fn_type(merge_tuple_type(world().mem_type(),  convert(fn_type->domain())));
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Type*> nops;
        for (const auto& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world().tuple_type(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world().struct_type(struct_type->struct_decl()->symbol(), struct_type->num_ops());
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (const auto& op : struct_type->ops())
            s->set(i++, convert(op));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return s;
    } else if (auto enum_type = type->isa<EnumType>()) {
        auto s = world().struct_type(enum_type->enum_decl()->symbol(), 2);
        thorin_enum_type(enum_type) = s;
        thorin_type(enum_type) = s;

        auto enum_decl = enum_type->enum_decl();
        thorin::TypeSet variants;
        for (const auto& option : enum_decl->option_decls())
            variants.insert(option->variant_type(*this));
        thorin::Array<const thorin::Type*> ops(variants.size());
        std::copy(variants.begin(), variants.end(), ops.begin());

        s->set(0, world().type_qu32());
        s->set(1, world().variant_type(ops));
        thorin_type(enum_type) = nullptr;
        return s;
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world().ptr_type(convert(ptr_type->pointee()), 1, -1, thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world().definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world().indefinite_array_type(convert(indefinite_array_type->elem_type()));
    } else if (auto simd_type = type->isa<SimdType>()) {
        return world().type(convert(simd_type->elem_type())->as<thorin::PrimType>()->primtype_tag(), simd_type->dim());
    }
    THORIN_UNREACHABLE;
}

/*
 * Decls and Function
 */

Value LocalDecl::emit(CodeGen& cg, const Def* init) const {
    auto thorin_type = cg.convert(type());

    auto do_init = [&]() {
        if (init)
            value_.store(init, location());
    };

    if (is_address_taken()) {
        value_ = Value::create_ptr(cg, cg.world().slot(thorin_type, cg.frame(), debug()));
        do_init();
    } else if (is_mut()) {
        value_ = Value::create_mut(cg, handle(), thorin_type);
        do_init();
    } else
        value_ = Value::create_val(cg, init);

    return value_;
}

const thorin::Type* OptionDecl::variant_type(CodeGen& cg) const {
    std::vector<const thorin::Type*> types;
    for (const auto& arg : args())
        types.push_back(cg.convert(arg->type()));
    if (num_args() == 1) return types.back();
    return cg.world().tuple_type(types);
}

Continuation* Fn::emit_head(CodeGen& cg, Location location) const {
    return continuation_ = cg.continuation(cg.convert(fn_type())->as<thorin::FnType>(),
                                           {location, fn_symbol().remove_quotation()});
}

void Fn::emit_body(CodeGen& cg, Location location) const {
    // setup function nest
    continuation()->set_parent(cg.cur_bb);
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, continuation());

    // setup memory + frame
    {
        size_t i = 0;
        const Def* mem_param = continuation()->param(i++);
        mem_param->debug().set("mem");
        cg.set_mem(mem_param);
        frame_ = cg.create_frame(location);

        // name params and setup store locations
        for (const auto& param : params()) {
            auto p = continuation()->param(i++);
            p->debug().set(param->symbol());
            cg.emit(param.get(), p);
        }

        assert(i == continuation()->num_params() || continuation()->type() == cg.empty_fn_type);

        if (continuation()->num_params() != 0
                && continuation()->params().back()->type()->isa<thorin::FnType>())
            ret_param_ = continuation()->params().back();
    }

    // descend into body
    auto def = cg.remit(body());
    if (def) {
        const Def* mem = cg.get_mem();
        // flatten returned values
        if (auto tuple = body()->type()->isa<TupleType>()) {
            Array<const Def*> ret_values(tuple->num_ops() + 1);
            for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
                ret_values[i + 1] = cg.world().extract(def, i);
            ret_values[0] = mem;
            cg.cur_bb->jump(ret_param(), ret_values, location.back());
        } else
            cg.cur_bb->jump(ret_param(), {mem, def}, location.back());
    }

    // now handle the filter
    {
        size_t i = 0;
        auto global = pe_expr() ? cg.remit(pe_expr()) : cg.world().literal_bool(false, location);
        Array<const Def*> filter(continuation()->num_params());
        filter[i++] = global; // mem param

        for (const auto& param : params()) {
            auto pe_expr = param->pe_expr();
            filter[i++] = pe_expr
                          ? cg.world().arithop_or(global, cg.remit(pe_expr), pe_expr->location())
                          : global;
        }

        // HACK for unit
        if (auto tuple_type = continuation()->type()->ops().back()->isa<thorin::TupleType>()) {
            if (tuple_type->num_ops() == 0)
                filter[i++] = global;
        }

        continuation()->set_filter(cg.world().tuple(filter));
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
    else if (name == "insert")   return true;
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

Value OptionDecl::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    auto enum_type = enum_decl()->type()->as<EnumType>();
    auto variant_type = cg.convert(enum_type)->op(1)->as<VariantType>();
    auto id = cg.world().literal_qu32(index(), location());
    if (num_args() == 0) {
        auto bot = cg.world().bottom(variant_type);
        return Value::create_val(cg, cg.world().struct_agg(cg.thorin_enum_type(enum_type), { id, bot }) );
    } else {
        auto continuation = cg.world().continuation(cg.convert(type())->as<thorin::FnType>(), {location(), symbol()});
        auto ret = continuation->param(continuation->num_params() - 1);
        auto mem = continuation->param(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = continuation->num_params(); i + 1 < e; i++)
            defs[i-1] = continuation->param(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world().tuple(defs);
        auto enum_val = cg.world().struct_agg(cg.thorin_enum_type(enum_type), { id, cg.world().variant(variant_type, option_val) });
        continuation->jump(ret, { mem, enum_val }, location());
        return Value::create_val(cg, continuation);
    }
}

Value StaticItem::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world().bottom(cg.convert(type()), location()) : cg.remit(this->init());
    if (!is_mut())
        return Value::create_val(cg, init);
    return Value::create_ptr(cg, cg.world().global(init, true, debug()));
}

void StructDecl::emit(CodeGen& cg) const {
    cg.convert(type());
}

void EnumDecl::emit(CodeGen& cg) const {
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
        cg.jump(x, location().back());
    } else
        assert(!cg.is_reachable());
}
void Expr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const { cg.branch(cg.remit(this), t, f, location().back()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world().tuple({}, location()); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeTag ttag;

    switch (tag()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: ttag = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: ttag = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world().literal(ttag, box(), location());
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world().literal_pu8(value(), location());
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

Value RValueExpr::lemit(CodeGen& cg) const {
    assert(src()->type()->isa<RefType>());
    return cg.lemit(src());
}

const Def* RValueExpr::remit(CodeGen& cg) const {
    if (src()->type()->isa<RefType>())
        return cg.lemit(this).load(location());
    return cg.remit(src());
}

Value PathExpr::lemit(CodeGen& cg) const {
    return cg.emit(value_decl(), nullptr);
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            const Def* def = var.load(location());
            const Def* one = cg.world().one(def->type(), location());
            const Def* ndef = cg.world().arithop(Token::to_arithop((TokenTag) tag()), def, one, location());
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
            if (rhs()->type()->isa<RefType>()) {
                auto var = cg.lemit(rhs());
                assert(var.tag() == Value::PtrRef);
                return var.def();
            }

            auto def = cg.remit(rhs());
            if (is_const(def))
                return cg.world().global(def, /*mutable*/ false, location());

            auto slot = cg.world().slot(cg.convert(rhs()->type()), cg.frame(), location());
            cg.store(slot, def, location());
            return slot;
        }
        case MUT: {
            auto var = cg.lemit(rhs());
            assert(var.tag() == Value::PtrRef);
            return var.def();
        }
        case RUNRUN: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world().run(def, location());
        }
        case HLT: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world().hlt(def, location());
        }
        case KNOWN: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world().known(def, location());
        }
        case OR:
        case OROR:
            THORIN_UNREACHABLE;
        default:  return cg.lemit(this).load(location());
    }
}

Value PrefixExpr::lemit(CodeGen& cg) const {
    if (tag() == MUL)
        return Value::create_ptr(cg, cg.remit(rhs()));
    THORIN_UNREACHABLE; // TODO
}

void PrefixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    if (tag() == NOT && is_type_bool(cg.convert(type())))
        cg.emit_branch(rhs(), f, t);
    else
        cg.branch(cg.remit(this), t, f, location().back());
}

void InfixExpr::emit_branch(CodeGen& cg, JumpTarget& t, JumpTarget& f) const {
    switch (tag()) {
        case ANDAND: {
            JumpTarget x({rhs()->location().front(), "and_extra"});
            cg.emit_branch(lhs(), x, f);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        case OROR: {
            JumpTarget x({rhs()->location().front(), "or_extra"});
            cg.emit_branch(lhs(), t, x);
            if (cg.enter(x))
                cg.emit_branch(rhs(), t, f);
            return;
        }
        default:
            cg.branch(cg.remit(this), t, f, location().back());
            return;
    }
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case ANDAND: {
            JumpTarget t({lhs()->location().front(), "and_true"});
            JumpTarget f({rhs()->location().front(), "and_false"});
            JumpTarget x({location().back(), "and_exit"});
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump(rhs(), x);
            if (cg.enter(f)) cg.emit_jump_boolean(false, x, lhs()->location().back());
            return cg.converge(this, x);
        }
        case OROR: {
            JumpTarget t({lhs()->location().front(), "or_true"});
            JumpTarget f({rhs()->location().front(), "or_false"});
            JumpTarget x({location().back(), "or_exit"});
            cg.emit_branch(lhs(), t, f);
            if (cg.enter(t)) cg.emit_jump_boolean(true, x, rhs()->location().back());
            if (cg.enter(f)) cg.emit_jump(rhs(), x);
            return cg.converge(this, x);
        }
        default:
            const TokenTag op = (TokenTag) tag();

            if (Token::is_assign(op)) {
                Value lvar = cg.lemit(lhs());
                const Def* rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    TokenTag sop = Token::separate_assign(op);
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
    var.store(cg.world().arithop(Token::to_arithop((TokenTag) tag()), def, one, location()), location());
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
    auto agg = cg.lemit(lhs());
    return Value::create_agg(agg, cg.remit(arg(0)));
}

const Def* MapExpr::remit(CodeGen& cg) const {
    auto ltype = unpack_ref_type(lhs()->type());

    if (auto fn_type = ltype->isa<FnType>()) {
        const Def* dst = nullptr;

        // Handle primops here
        if (auto type_expr = lhs()->isa<TypeAppExpr>()) { // Bitcast, sizeof and select are all polymorphic
            auto callee = type_expr->lhs()->skip_rvalue();
            if (auto path = callee->isa<PathExpr>()) {
                if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
                    if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
                        auto name = fn_decl->fn_symbol().remove_quotation();
                        if (name == "bitcast") {
                            return cg.world().bitcast(cg.convert(type_expr->type_arg(0)), cg.remit(arg(0)), location());
                        } else if (name == "select") {
                            return cg.world().select(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), location());
                        } else if (name == "insert") {
                            return cg.world().insert(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), location());
                        } else if (name == "sizeof") {
                            return cg.world().size_of(cg.convert(type_expr->type_arg(0)), location());
                        } else if (name == "undef") {
                            return cg.world().bottom(cg.convert(type_expr->type_arg(0)), location());
                        } else if (name == "reserve_shared") {
                            auto ptr_type = cg.convert(type());
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), cg.world().type_qs32(),
                                cg.world().fn_type({ cg.world().mem_type(), ptr_type }) });
                            auto cont = cg.world().continuation(fn_type, {location(), "reserve_shared"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr_type = cg.convert(arg(1)->type());
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), cg.world().type_pu32(), ptr_type, poly_type,
                                cg.world().fn_type({ cg.world().mem_type(), poly_type }) });
                            auto cont = cg.world().continuation(fn_type, {location(), "atomic"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), ptr_type, poly_type, poly_type,
                                cg.world().fn_type({ cg.world().mem_type(), poly_type, cg.world().type_bool() })
                            });
                            auto cont = cg.world().continuation(fn_type, {location(), "cmpxchg"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto string_type = cg.world().ptr_type(cg.world().indefinite_array_type(cg.world().type_pu8()));
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), string_type, poly_type,
                                cg.world().fn_type({ cg.world().mem_type() }) });
                            auto cont = cg.world().continuation(fn_type, {location(), "pe_info"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_known") {
                            auto poly_type = cg.convert(arg(0)->type());
                            auto fn_type = cg.world().fn_type({
                                cg.world().mem_type(), poly_type,
                                cg.world().fn_type({ cg.world().mem_type(), cg.world().type_bool() }) });
                            auto cont = cg.world().continuation(fn_type, {location(), "pe_known"});
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

        auto ret_type = num_args() == fn_type->num_params() ? nullptr : cg.convert(fn_type->return_type());
        auto ret = cg.call(dst, defs, ret_type, thorin::Debug(location(), dst->name()) + "_cont");
        if (ret_type)
            cg.set_mem(cg.cur_bb->param(0));

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>() || ltype->isa<SimdType>()) {
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

const Def* BlockExpr::remit(CodeGen& cg) const {
    for (const auto& stmt : stmts())
        cg.emit(stmt.get());
    return cg.remit(expr());
}

void IfExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    JumpTarget t({then_expr()->location().front(), "if_then"});
    JumpTarget f({else_expr()->location().front(), "if_else"});
    cg.emit_branch(cond(), t, f);
    if (cg.enter(t))
        cg.emit_jump(then_expr(), x);
    if (cg.enter(f))
        cg.emit_jump(else_expr(), x);
    cg.jump(x, location().back());
}

const Def* IfExpr::remit(CodeGen& cg) const {
    JumpTarget x({location().back(), "next"});
    return cg.converge(this, x);
}

void MatchExpr::emit_jump(CodeGen& cg, JumpTarget& x) const {
    auto matcher = cg.remit(expr());
    auto enum_type = expr()->type()->isa<EnumType>();
    bool is_integer = is_int(expr()->type());
    bool is_simple = enum_type && enum_type->enum_decl()->is_simple();

    if (is_integer || is_simple) {
        // integers: match continuation
        JumpTarget otherwise;
        size_t num_targets = num_arms();
        Array<const Def*> defs(num_targets);
        Array<JumpTarget> targets(num_targets);

        for (size_t i = 0, e = num_targets; i != e; ++i) {
            // last pattern will always be taken
            if (!arm(i)->ptrn()->is_refutable() || i == e - 1) {
                num_targets = i;
                cg.emit(arm(i)->ptrn(), matcher);
                otherwise = JumpTarget({arm(i)->location().front(), "otherwise"});
                break;
            } else {
                if (is_integer) {
                    defs[i] = arm(i)->ptrn()->as<LiteralPtrn>()->emit_literal(cg);
                } else {
                    auto enum_ptrn = arm(i)->ptrn()->as<EnumPtrn>();
                    auto option_decl = enum_ptrn->path()->decl()->as<OptionDecl>();
                    defs[i] = cg.world().literal_qu32(option_decl->index(), arm(i)->ptrn()->location());
                }
                targets[i] = JumpTarget({arm(i)->location().front(), "case"});
            }
        }
        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world().extract(matcher, 0_u32, matcher->debug());
        cg.match(matcher_int, otherwise, defs, targets, {location().front(), "match"});

        for (size_t i = 0; i < num_targets; i++) {
            if (cg.enter(targets[i]))
                cg.emit_jump(arm(i)->expr(), x);
        }
        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise && cg.enter(otherwise))
            cg.emit_jump(arm(num_targets)->expr(), x);
    } else {
        // general case: if/else
        for (size_t i = 0, e = num_arms(); i != e; ++i) {
            JumpTarget  cur({arm(i)->location().front(), "case_true"});
            JumpTarget next({arm(i)->location().front(), "case_false"});

            arm(i)->ptrn()->emit(cg, matcher);
            // last pattern will always be taken
            auto cond = i == e - 1
                ? cg.world().literal_bool(true, arm(i)->ptrn()->location())
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.branch(cond, cur, next);
            if (cg.enter(cur))
                cg.emit_jump(arm(i)->expr(), x);

            cg.enter(next);
        }
    }
    cg.jump(x, location().back());
}

const Def* MatchExpr::remit(CodeGen& cg) const {
    JumpTarget x({location().back(), "next"});
    return cg.converge(this, x);
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    JumpTarget x({location().back(), "next"});
    auto break_continuation = cg.create_continuation(break_decl());

    cg.emit_jump(this, x);
    cg.jump_to_continuation(break_continuation, location().back());
    return cg.world().tuple({}, location());
}

void WhileExpr::emit_jump(CodeGen& cg, JumpTarget& exit_bb) const {
    JumpTarget head_bb({cond()->location().front(), "while_head"});
    JumpTarget body_bb({body()->location().front(), "while_body"});
    auto continue_continuation = cg.create_continuation(continue_decl());

    cg.jump(head_bb, cond()->location().back());
    cg.enter_unsealed(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);
    if (cg.enter(body_bb)) {
        cg.remit(body());
        cg.jump_to_continuation(continue_continuation, cond()->location().back());
    }
    cg.jump(head_bb, cond()->location().back());
    head_bb.seal();
    cg.enter(exit_bb);
}

const Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const Def*> defs;
    defs.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_continuation = cg.create_continuation(break_decl());

    // peel off run and halt
    auto forexpr = expr();

    // emit call
    auto map_expr = forexpr->as<MapExpr>();
    for (const auto& arg : map_expr->args())
        defs.push_back(cg.remit(arg.get()));
    defs.push_back(cg.remit(fn_expr()));
    defs.push_back(break_continuation);
    auto fun = cg.remit(map_expr->lhs());

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
    init->debug().set(local()->symbol());
    cg.emit(local(), init);
}

const thorin::Def* IdPtrn::emit_cond(CodeGen& cg, const thorin::Def*) const {
    return cg.world().literal(true);
}

void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant = cg.world().cast(variant_type, cg.world().extract(init, 1), location());
    for (size_t i = 0, e = num_args(); i != e; ++i) {
        cg.emit(arg(i), num_args() == 1 ? variant : cg.extract(variant, i, location()));
    }
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto cond = cg.world().cmp_eq(cg.world().extract(init, 0_u32, location()), cg.world().literal_qu32(index, location()));
    if (num_args() > 0) {
        auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
        auto variant = cg.world().cast(variant_type, cg.world().extract(init, 1, location()), location());
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? variant : cg.world().extract(variant, i, location()));
            cond = cg.world().arithop_and(cond, arg_cond, location());
        }
    }
    return cond;
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        cg.emit(elem(i), cg.extract(init, i, location()));
}

const thorin::Def* TuplePtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    const Def* cond = nullptr;
    for (size_t i = 0, e = num_elems(); i != e; ++i) {
        if (!elem(i)->is_refutable()) continue;

        auto next = elem(i)->emit_cond(cg, cg.extract(init, i, location()));
        cond = cond ? cg.world().arithop_and(cond, next) : next;
    }
    return cond ? cond : cg.world().literal(true);
}

const thorin::Def* LiteralPtrn::emit_literal(CodeGen& cg) const {
    auto def = cg.remit(literal());
    return has_minus() ? cg.world().arithop_minus(def, def->debug()) : def;
}

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world().cmp_eq(init, emit_literal(cg));
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
        outs[i] = cg.convert(output(i)->expr()->type()->as<RefType>()->pointee());

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
