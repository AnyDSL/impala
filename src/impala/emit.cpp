#include "impala/ast.h"

#include "thorin/continuation.h"
#include "thorin/primop.h"
#include "thorin/type.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

using namespace thorin;

namespace impala {

class CodeGen {
public:
    CodeGen(World& world)
        : world(world)
    {}

    /// Continuation of type cn()
    Continuation* basicblock(Debug dbg) { return world.continuation(world.fn_type(), dbg); }

    /// Continuation of type cn(mem, type) - a point in the program where control flow *j*oins
    Continuation* basicblock(const thorin::Type* type, Debug dbg) {
        auto bb = world.continuation(world.fn_type({world.mem_type(), type}), dbg);
        bb->param(0)->debug().set("mem");
        return bb;
    }

    Continuation* enter(Continuation* bb, const Def* mem) {
        cur_bb = bb;
        cur_mem = mem;
        return bb;
    }

    const Def* enter(Continuation* bb) {
        enter(bb, bb->param(0));
        return bb->param(1);
    }

    const Def* frame() const { assert(cur_fn); return cur_fn->frame(); }

    std::pair<Continuation*, const Def*> call(const Def* callee, Defs args, const thorin::Type* ret_type, Debug dbg) {
        if (ret_type == nullptr) {
            cur_bb->jump(callee, args, dbg);
            auto next = basicblock(dbg + "_unrechable");
            return std::make_pair(next, nullptr);
        }

        std::vector<const thorin::Type*> cont_args;
        cont_args.push_back(world.mem_type());

        // if the return type is a tuple, flatten it
        auto tuple = ret_type->isa<thorin::TupleType>();
        if (tuple) {
            for (auto op : tuple->ops())
                cont_args.push_back(op);
        } else
            cont_args.push_back(ret_type);

        // next is the return continuation
        auto next = world.continuation(world.fn_type(cont_args), dbg);
        next->param(0)->debug().set("mem");

        // create jump to next
        size_t csize = args.size() + 1;
        Array<const Def*> cargs(csize);
        *std::copy(args.begin(), args.end(), cargs.begin()) = next;
        cur_bb->jump(callee, cargs, dbg);

        // determine return value
        const Def* ret = nullptr;
        if (tuple) {
            Array<const Def*> params(next->num_params() - 1);
            for (size_t i = 1, e = next->num_params(); i != e; ++i)
                params[i - 1] = next->param(i);
            ret = world.tuple(params);
        } else
            ret = next->param(1);
        ret->debug().set(callee->name());

        return std::make_pair(next, ret);
    }

    Continuation* create_continuation(const LocalDecl* decl) {
        auto result = world.continuation(convert(decl->type())->as<thorin::FnType>(), decl->debug());
        result->param(0)->debug().set("mem");
        decl->def_ = result;
        return result;
    }

    const Def* load(const Def* ptr, Location location) {
        auto l = world.load(cur_mem, ptr, location);
        cur_mem = world.extract(l, 0_s, location);
        return world.extract(l, 1_s, location);
    }

    void store(const Def* ptr, const Def* val, Location location) {
        cur_mem = world.store(cur_mem, ptr, val, location);
    }

    const Def* alloc(const thorin::Type* type, const Def* extra, Debug dbg) {
        if (!extra)
            extra = world.literal_qu64(0, dbg);
        auto alloc = world.alloc(type, cur_mem, extra, dbg);
        cur_mem = world.extract(alloc, 0_s, dbg);
        return world.extract(alloc, 1, dbg);
    }

    const thorin::Type* convert(const Type* type) {
        if (auto t = thorin_type(type))
            return t;
        auto t = convert_rec(type);
        return thorin_type(type) = t;
    }

    const thorin::Type* convert_rec(const Type*);

    const thorin::Type*& thorin_type(const Type* type) { return impala2thorin_[type]; }

    World& world;
    const Fn* cur_fn = nullptr;
    TypeMap<const thorin::Type*> impala2thorin_;
    Continuation* cur_bb = nullptr;
    const Def* cur_mem = nullptr;
};

/*
 * Type
 */

const thorin::Type* CodeGen::convert_rec(const Type* type) {
    if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world.type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto fn_type = type->isa<FnType>()) {
        std::vector<const thorin::Type*> nops;
        nops.push_back(world.mem_type());
        for (size_t i = 0, e = fn_type->num_params(); i != e; ++i)
            nops.push_back(convert(fn_type->param(i)));
        return world.fn_type(nops);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Type*> nops;
        for (auto&& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world.tuple_type(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        const auto& decl = struct_type->struct_decl();
        auto s = world.struct_type(decl->symbol(), struct_type->num_ops());
        thorin_type(type) = s;
        for(size_t i = 0, n = struct_type->num_ops(); i < n; i++) {
            s->set(i, convert(struct_type->op(i)));
            s->set_op_name(i, decl->field_decl(i)->symbol());
        }
        return s;
    } else if (auto enum_type = type->isa<EnumType>()) {
        const auto& decl = enum_type->enum_decl();
        auto e = world.variant_type(decl->symbol(), enum_type->num_ops());
        thorin_type(enum_type) = e;
        for(size_t i = 0, n = enum_type->num_ops(); i < n; i++) {
            e->set(i, decl->option_decl(i)->variant_type(*this));
            e->set_op_name(i, decl->option_decl(i)->symbol());
        }
        return e;
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world.ptr_type(convert(ptr_type->pointee()), 1, -1, thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world.definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world.indefinite_array_type(convert(indefinite_array_type->elem_type()));
    } else if (auto simd_type = type->isa<SimdType>()) {
        return world.prim_type(convert(simd_type->elem_type())->as<thorin::PrimType>()->primtype_tag(), simd_type->dim());
    } else if (type->isa<NoRetType>()) {
        return nullptr; // TODO use bottom type - once it is available in thorin
    }
    THORIN_UNREACHABLE;
}

/*
 * Decls and Function
 */

void LocalDecl::emit(CodeGen& cg, const Def* init) const {
    assert(def_ == nullptr);

    auto thorin_type = cg.convert(type());
    init = init ? init : cg.world.bottom(thorin_type);

    if (is_mut()) {
        def_ = cg.world.slot(thorin_type, cg.frame(), debug());
        cg.cur_mem = cg.world.store(cg.cur_mem, def_, init, debug());
    } else {
        def_ = init;
    }
}

const thorin::Type* OptionDecl::variant_type(CodeGen& cg) const {
    std::vector<const thorin::Type*> types;
    for (auto&& arg : args())
        types.push_back(cg.convert(arg->type()));
    if (num_args() == 1) return types.back();
    return cg.world.tuple_type(types);
}

Continuation* Fn::fn_emit_head(CodeGen& cg, Location location) const {
    auto t = cg.convert(fn_type())->as<thorin::FnType>();
    return continuation_ = cg.world.continuation(t, {location, fn_symbol().remove_quotation()});
}

void Fn::fn_emit_body(CodeGen& cg, Location location) const {
    // setup function nest
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, continuation());
    auto old_mem = cg.cur_mem;

    // setup memory + frame
    {
        size_t i = 0;
        auto mem_param = continuation()->param(i++);
        mem_param->debug().set("mem");
        auto enter = cg.world.enter(mem_param, location);
        cg.cur_mem = cg.world.extract(enter, 0_s, location);
        frame_ =     cg.world.extract(enter, 1_s, location);

        // name params and setup store locations
        for (auto&& param : params()) {
            auto p = continuation()->param(i++);
            p->debug().set(param->symbol());
            param->emit(cg, p);
        }

        //assert(i == continuation()->num_params() || continuation()->type() == cg.empty_fn_type);

        if (continuation()->num_params() != 0
                && continuation()->params().back()->type()->isa<thorin::FnType>())
            ret_param_ = continuation()->params().back();
    }

    // descend into body
    auto def = body()->remit(cg);
    if (def) {
        // flatten returned values
        if (auto tuple = body()->type()->isa<TupleType>()) {
            Array<const Def*> ret_values(tuple->num_ops() + 1);
            for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
                ret_values[i + 1] = cg.world.extract(def, i);
            ret_values[0] = cg.cur_mem;
            cg.cur_bb->jump(ret_param(), ret_values, location.back());
        } else
            cg.cur_bb->jump(ret_param(), {cg.cur_mem, def}, location.back());
    }

    // now handle the filter
    {
        size_t i = 0;
        auto global = pe_expr() ? pe_expr()->remit(cg) : cg.world.literal_bool(false, location);
        Array<const Def*> filter(continuation()->num_params());
        filter[i++] = global; // mem param

        for (auto&& param : params()) {
            auto pe_expr = param->pe_expr();
            filter[i++] = pe_expr
                          ? cg.world.arithop_or(global, pe_expr->remit(cg), pe_expr->location())
                          : global;
        }

        // HACK for unit
        if (auto tuple_type = continuation()->type()->ops().back()->isa<thorin::TupleType>()) {
            if (tuple_type->num_ops() == 0)
                filter[i++] = global;
        }

        continuation()->set_filter(filter);
    }

    cg.cur_mem = old_mem;
}

/*
 * items
 */

void Module::emit(CodeGen& cg) const {
    for (auto&& item : items()) item->emit_head(cg);
    for (auto&& item : items()) item->emit(cg);
}

static bool is_primop(const Symbol& name) {
    if      (name == "alignof") return true;
    else if (name == "bitcast") return true;
    else if (name == "insert")  return true;
    else if (name == "select")  return true;
    else if (name == "sizeof")  return true;
    return false;
}

void FnDecl::emit_head(CodeGen& cg) const {
    assert(def_ == nullptr);
    // no code is emitted for primops
    if (is_extern() && abi() == "\"thorin\"" && is_primop(symbol()))
        return;

    // create thorin function
    def_ = fn_emit_head(cg, location());
    if (is_extern() && abi() == "")
        continuation_->make_exported();

    // handle main function
    if (symbol() == "main")
        continuation()->make_exported();
}

void FnDecl::emit(CodeGen& cg) const {
    if (body())
        fn_emit_body(cg, location());
}

void ExternBlock::emit_head(CodeGen& cg) const {
    for (auto&& fn_decl : fn_decls()) {
        fn_decl->emit_head(cg);
        auto continuation = fn_decl->continuation();
        if (abi() == "\"C\"")
            continuation->make_imported();
        else if (abi() == "\"device\"")
            continuation->attributes().cc = thorin::CC::Device;
        else if (abi() == "\"thorin\"" && continuation) // no continuation for primops
            continuation->set_intrinsic();
    }
}

void ModuleDecl::emit(CodeGen&) const {}
void ImplItem::emit(CodeGen&) const {}

void StaticItem::emit_head(CodeGen& cg) const {
    def_ = cg.world.global(cg.world.bottom(cg.convert(type()), location()));
}

void StaticItem::emit(CodeGen& cg) const {
    if (init()) {
        auto old_def = def_;
        def_ = cg.world.global(init()->remit(cg), is_mut(), debug());
        old_def->replace(def_);
    }
}

void StructDecl::emit_head(CodeGen& cg) const {
    cg.convert(type());
}

void OptionDecl::emit(CodeGen& cg) const {
    auto enum_type = enum_decl()->type()->as<EnumType>();
    auto variant_type = cg.convert(enum_type)->as<VariantType>();
    if (num_args() == 0) {
        auto bot = cg.world.bottom(variant_type->op(index()));
        def_ = cg.world.variant(variant_type, bot, index());
    } else {
        auto continuation = cg.world.continuation(cg.convert(type())->as<thorin::FnType>(), {location(), symbol()});
        auto ret = continuation->param(continuation->num_params() - 1);
        auto mem = continuation->param(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = continuation->num_params(); i + 1 < e; i++)
            defs[i-1] = continuation->param(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.variant(variant_type, option_val, index());
        continuation->jump(ret, { mem, enum_val }, location());
        def_ = continuation;
    }
}

void EnumDecl::emit_head(CodeGen& cg) const {
    for (auto&& option_decl : option_decls())
        option_decl->emit(cg);
    cg.convert(type());
}

void TraitDecl::emit(CodeGen&) const {}
void Typedef::emit(CodeGen&) const {}

/*
 * expressions
 */

const Def* Expr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* Expr::remit(CodeGen& cg) const { return cg.load(lemit(cg), location()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world.tuple({}, location()); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeTag ttag;

    switch (tag()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: ttag = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: ttag = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world.literal(ttag, box(), location());
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world.literal_pu8(value(), location());
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world.literal_pu8(values_[i], location());

    return cg.world.definite_array(args, location());
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = src()->remit(cg);
    auto thorin_type = cg.convert(type());
    return cg.world.convert(thorin_type, def, location());
}

const Def* RValueExpr::lemit(CodeGen& cg) const {
    assert(src()->type()->isa<RefType>());
    return src()->lemit(cg);
}

const Def* RValueExpr::remit(CodeGen& cg) const {
    if (src()->type()->isa<RefType>())
        return cg.load(lemit(cg), location());
    return src()->remit(cg);
}

const Def* PathExpr::lemit(CodeGen&) const {
    assert(value_decl()->is_mut());
    return value_decl()->def();
}

const Def* PathExpr::remit(CodeGen& cg) const {
    auto def = value_decl()->def();
    // This whole global thing is incorrect.
    // Example:
    // static a = 1;
    // static b = a;
    // Emitting this requires a load. Currently, it works because of the following hack.
    // But the hack no longer works if the order is reversed:
    // static b = a;
    // static a = 1;
    // In this case, during the emission of 'static b = a', the static item 'a' has not been replaced yet and is considered mutable.
    auto global = def->isa<Global>();
    if (global && !global->is_mutable())
        return global->init();
    return value_decl()->is_mut() || global ? cg.load(def, location()) : def;
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = rhs()->lemit(cg);
            auto val = cg.load(var, location());
            auto one = cg.world.one(val->type(), location());
            val = cg.world.arithop(Token::to_arithop((TokenTag) tag()), val, one, location());
            cg.store(var, val, location());
            return val;
        }
        case ADD: return rhs()->remit(cg);
        case SUB: return cg.world.arithop_minus(rhs()->remit(cg), location());
        case NOT: return cg.world.arithop_not(rhs()->remit(cg), location());
        case TILDE: {
            auto def = rhs()->remit(cg);
            auto ptr = cg.alloc(def->type(), rhs()->extra(), location());
            cg.store(ptr, def, location());
            return ptr;
        }
        case AND: {
            if (rhs()->type()->isa<RefType>())
                return rhs()->lemit(cg);

            auto def = rhs()->remit(cg);
            if (is_const(def))
                return cg.world.global(def, /*mutable*/ false, location());

            auto slot = cg.world.slot(cg.convert(rhs()->type()), cg.frame(), location());
            cg.store(slot, def, location());
            return slot;
        }
        case MUT: {
            return rhs()->lemit(cg);
        }
        case RUNRUN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.run(def, location());
        }
        case HLT: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.hlt(def, location());
        }
        case KNOWN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.known(def, location());
        }
        case OR:
        case OROR:
            THORIN_UNREACHABLE;
        default:
            return cg.load(lemit(cg), location());
    }
}

const Def* PrefixExpr::lemit(CodeGen& cg) const {
    if (tag() == RUNRUN)
        return rhs()->lemit(cg);

    assert(tag() == MUL);
    return rhs()->remit(cg);
}

void Expr::emit_branch(CodeGen& cg, Continuation* jump_true, Continuation* jump_false) const {
    auto expr_true  = cg.basicblock({ location().back(), "expr_true"  });
    auto expr_false = cg.basicblock({ location().back(), "expr_false" });
    auto cond = remit(cg);
    cg.cur_bb->branch(cond, expr_true, expr_false, location().back());
    expr_true->jump(jump_true, { cg.cur_mem });
    expr_false->jump(jump_false, { cg.cur_mem });
}

void InfixExpr::emit_branch(CodeGen& cg, Continuation* jump_true, Continuation* jump_false) const {
    auto jump_type = jump_true->type();
    switch (tag()) {
        case OROR: {
                auto or_false = cg.world.continuation(jump_type, { location().back(), "or_false" });
                lhs()->emit_branch(cg, jump_true, or_false);
                cg.enter(or_false, or_false->param(0));
                rhs()->emit_branch(cg, jump_true, jump_false);
            }
            break;
        case ANDAND: {
                auto and_true = cg.world.continuation(jump_type, { location().back(), "and_true" });
                lhs()->emit_branch(cg, and_true, jump_false);
                cg.enter(and_true, and_true->param(0));
                rhs()->emit_branch(cg, jump_true, jump_false);
            }
            break;
        default:
            return Expr::emit_branch(cg, jump_true, jump_false);
    }
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case OROR:
        case ANDAND: {
            auto result     = cg.basicblock(cg.world.type_bool(), { location().back(), "infix_result" });
            auto jump_type  = cg.world.fn_type({ cg.world.mem_type() });
            auto jump_true  = cg.world.continuation(jump_type, { location().back(), "jump_true" });
            auto jump_false = cg.world.continuation(jump_type, { location().back(), "jump_true" });
            emit_branch(cg, jump_true, jump_false);
            jump_true->jump(result, { jump_true->param(0), cg.world.literal(true) });
            jump_false->jump(result, { jump_false->param(0), cg.world.literal(false) });
            return cg.enter(result);
        }
        default:
            const TokenTag op = (TokenTag) tag();

            if (Token::is_assign(op)) {
                auto lvar = lhs()->lemit(cg);
                auto rdef = rhs()->remit(cg);

                if (op != Token::ASGN) {
                    auto sop = Token::separate_assign(op);
                    rdef = cg.world.binop(Token::to_binop(sop), cg.load(lvar, location()), rdef, location());
                }

                cg.store(lvar, rdef, location());
                return cg.world.tuple({}, location());
            }

            auto ldef = lhs()->remit(cg);
            auto rdef = rhs()->remit(cg);
            return cg.world.binop(Token::to_binop(op), ldef, rdef, location());
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    auto var = lhs()->lemit(cg);
    auto def = cg.load(var, location());
    auto one = cg.world.one(def->type(), location());
    cg.store(var, cg.world.arithop(Token::to_arithop((TokenTag) tag()), def, one, location()), location());
    return def;
}

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, location());
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(count());
    std::fill_n(args.begin(), count(), value()->remit(cg));
    return cg.world.definite_array(args, location());
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.tuple(thorin_args, location());
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = dim()->remit(cg);
    return cg.world.indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, location());
}

const Def* SimdExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.vector(thorin_args, location());
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (auto&& elem : elems())
        defs[elem->field_decl()->index()] = elem->expr()->remit(cg);
    return cg.world.struct_agg(cg.convert(type())->as<thorin::StructType>(), defs, location());
}

const Def* TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const { THORIN_UNREACHABLE; }

const Def* MapExpr::lemit(CodeGen& cg) const {
    auto agg = lhs()->lemit(cg);
    return cg.world.lea(agg, arg(0)->remit(cg), location());
}

const Def* MapExpr::remit(CodeGen& cg) const {
    auto ltype = unpack_ref_type(lhs()->type());

    if (auto fn_type = ltype->isa<FnType>()) {
        const Def* dst = nullptr;

        // Handle primops here
        if (auto type_expr = lhs()->isa<TypeAppExpr>()) { // alignof, bitcast, sizeof, and select are all polymorphic
            auto callee = type_expr->lhs()->skip_rvalue();
            if (auto path = callee->isa<PathExpr>()) {
                if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
                    if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
                        auto name = fn_decl->fn_symbol().remove_quotation();
                        auto string_type = cg.world.ptr_type(cg.world.indefinite_array_type(cg.world.type_pu8()));
                        if (name == "alignof") {
                            return cg.world.align_of(cg.convert(type_expr->type_arg(0)), location());
                        } else if (name == "bitcast") {
                            return cg.world.bitcast(cg.convert(type_expr->type_arg(0)), arg(0)->remit(cg), location());
                        } else if (name == "insert") {
                            return cg.world.insert(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), location());
                        } else if (name == "select") {
                            return cg.world.select(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), location());
                        } else if (name == "sizeof") {
                            return cg.world.size_of(cg.convert(type_expr->type_arg(0)), location());
                        } else if (name == "undef") {
                            return cg.world.bottom(cg.convert(type_expr->type_arg(0)), location());
                        } else if (name == "reserve_shared") {
                            auto ptr_type = cg.convert(type());
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), cg.world.type_qs32(),
                                cg.world.fn_type({ cg.world.mem_type(), ptr_type }) });
                            auto cont = cg.world.continuation(fn_type, {location(), "reserve_shared"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr_type = cg.convert(arg(1)->type());
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), cg.world.type_pu32(), ptr_type, poly_type, cg.world.type_pu32(), string_type,
                                cg.world.fn_type({ cg.world.mem_type(), poly_type }) });
                            auto cont = cg.world.continuation(fn_type, {location(), "atomic"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic_load") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), ptr_type, cg.world.type_pu32(), string_type,
                                cg.world.fn_type({ cg.world.mem_type(), poly_type })
                            });
                            auto cont = cg.world.continuation(fn_type, {location(), "atomic_load"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic_store") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), ptr_type, poly_type, cg.world.type_pu32(), string_type,
                                cg.world.fn_type({ cg.world.mem_type() })
                            });
                            auto cont = cg.world.continuation(fn_type, {location(), "atomic_store"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), ptr_type, poly_type, poly_type, cg.world.type_pu32(), string_type,
                                cg.world.fn_type({ cg.world.mem_type(), poly_type, cg.world.type_bool() })
                            });
                            auto cont = cg.world.continuation(fn_type, {location(), "cmpxchg"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), string_type, poly_type,
                                cg.world.fn_type({ cg.world.mem_type() }) });
                            auto cont = cg.world.continuation(fn_type, {location(), "pe_info"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_known") {
                            auto poly_type = cg.convert(arg(0)->type());
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), poly_type,
                                cg.world.fn_type({ cg.world.mem_type(), cg.world.type_bool() }) });
                            auto cont = cg.world.continuation(fn_type, {location(), "pe_known"});
                            cont->set_intrinsic();
                            dst = cont;
                        }
                    }
                }
            }
        }

        dst = dst ? dst : lhs()->remit(cg);

        std::vector<const Def*> defs;
        defs.push_back(nullptr);    // reserve for mem but set later - some other args may update mem
        for (auto&& arg : args())
            defs.push_back(arg.get()->remit(cg));
        defs.front() = cg.cur_mem; // now get the current memory value

        auto ret_type = num_args() == fn_type->num_params() ? nullptr : cg.convert(fn_type->return_type());
        const Def* ret;
        std::tie(cg.cur_bb, ret) = cg.call(dst, defs, ret_type, thorin::Debug(location(), dst->name()) + "_cont");
        if (ret_type)
            cg.cur_mem = cg.cur_bb->param(0);

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>() || ltype->isa<SimdType>()) {
        auto index = arg(0)->remit(cg);
        return cg.world.extract(lhs()->remit(cg), index, location());
    }
    THORIN_UNREACHABLE;
}

const Def* FieldExpr::lemit(CodeGen& cg) const {
    auto value = lhs()->lemit(cg);
    return cg.world.lea(value, cg.world.literal_qu32(index(), location()), location());
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.world.extract(lhs()->remit(cg), index(), location());
}

const Def* BlockExpr::remit(CodeGen& cg) const {
    for (auto&& stmt : stmts()) {
        if (auto item_stmnt = stmt->isa<ItemStmt>())
            item_stmnt->item()->emit_head(cg);
    }

    for (auto&& stmt : stmts()) stmt->emit(cg);

    return expr()->remit(cg);
}

const Def* IfExpr::remit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());

    auto jump_type = cg.world.fn_type({ cg.world.mem_type() });
    auto if_then = cg.world.continuation(jump_type, {then_expr()->location().front(), "if_then"});
    auto if_else = cg.world.continuation(jump_type, {else_expr()->location().front(), "if_else"});
    auto if_join = thorin_type ? cg.basicblock(thorin_type, {location().back(), "if_join"}) : nullptr; // TODO rewrite with bottom type

    cond()->emit_branch(cg, if_then, if_else);

    cg.enter(if_then, if_then->param(0));
    if (auto tdef = then_expr()->remit(cg))
        cg.cur_bb->jump(if_join, {cg.cur_mem, tdef}, location().back());

    cg.enter(if_else, if_else->param(0));
    if (auto fdef = else_expr()->remit(cg))
        cg.cur_bb->jump(if_join, {cg.cur_mem, fdef}, location().back());

    if (thorin_type)
        return cg.enter(if_join);
    return nullptr; // TODO use bottom type
}

const Def* MatchExpr::remit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());

    auto join = thorin_type ? cg.basicblock(thorin_type, {location().back(), "match_join"}) : nullptr; // TODO rewrite with bottom type

    auto matcher = expr()->remit(cg);
    auto enum_type = expr()->type()->isa<EnumType>();
    bool is_integer = is_int(expr()->type());
    bool is_simple = enum_type && enum_type->enum_decl()->is_simple();

    if (is_integer || is_simple) {
        // integers: match continuation
        Continuation* otherwise = nullptr;
        size_t num_targets = num_arms();
        Array<const Def*> defs(num_targets);
        Array<Continuation*> targets(num_targets);

        for (size_t i = 0, e = num_targets; i != e; ++i) {
            // last pattern will always be taken
            if (!arm(i)->ptrn()->is_refutable() || i == e - 1) {
                num_targets = i;
                arm(i)->ptrn()->emit(cg, matcher);
                otherwise = cg.basicblock({arm(i)->location().front(), "otherwise"});
                break;
            } else {
                if (is_integer) {
                    defs[i] = arm(i)->ptrn()->emit(cg);
                } else {
                    auto enum_ptrn = arm(i)->ptrn()->as<EnumPtrn>();
                    auto option_decl = enum_ptrn->path()->decl()->as<OptionDecl>();
                    defs[i] = cg.world.literal_qu64(option_decl->index(), arm(i)->ptrn()->location());
                }
                targets[i] = cg.basicblock({arm(i)->location().front(), "case"});
            }
        }

        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world.variant_index(matcher, matcher->debug());
        cg.cur_bb->match(matcher_int, otherwise, defs, targets, {location().front(), "match"});
        auto mem = cg.cur_mem;

        for (size_t i = 0; i != num_targets; ++i) {
            cg.enter(targets[i], mem);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->jump(join, {cg.cur_mem, def}, location().back());
        }

        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise) {
            cg.enter(otherwise, mem);
            if (auto def = arm(num_targets)->expr()->remit(cg))
                cg.cur_bb->jump(join, {cg.cur_mem, def}, location().back());
        }
    } else {
        // general case: if/else
        for (size_t i = 0, e = num_arms(); i != e; ++i) {
            auto case_true  = cg.basicblock({arm(i)->location().front(), "case_true"});
            auto case_false = cg.basicblock({arm(i)->location().front(), "case_false"});

            arm(i)->ptrn()->emit(cg, matcher);

            // last pattern will always be taken
            auto cond = i == e - 1
                ? cg.world.literal_bool(true, arm(i)->ptrn()->location())
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.cur_bb->branch(cond, case_true, case_false, arm(i)->ptrn()->location().back());

            auto mem = cg.cur_mem;
            cg.enter(case_true, mem);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->jump(join, {cg.cur_mem, def}, arm(i)->location().back());

            cg.enter(case_false, mem);
        }
    }

    if (thorin_type)
        return cg.enter(join);
    return nullptr; // TODO use bottom type
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    auto head_bb = cg.world.continuation(cg.world.fn_type({cg.world.mem_type()}), {location().front(), "while_head"});
    head_bb->param(0)->debug().set("mem");

    auto jump_type = cg.world.fn_type({ cg.world.mem_type() });
    auto body_bb = cg.world.continuation(jump_type, {body()->location().front(), "while_body"});
    auto exit_bb = cg.world.continuation(jump_type, {body()->location().back(),  "while_exit"});

    auto cont_bb = cg.create_continuation(continue_decl());
    auto brk__bb = cg.create_continuation(break_decl());

    cg.cur_bb->jump(head_bb, {cg.cur_mem}, cond()->location().back());

    cg.enter(head_bb, head_bb->param(0));
    cond()->emit_branch(cg, body_bb, exit_bb);

    cg.enter(body_bb, body_bb->param(0));
    body()->remit(cg);
    cg.cur_bb->jump(cont_bb, {cg.cur_mem}, body()->location().back());

    cg.enter(cont_bb, cont_bb->param(0));
    cg.cur_bb->jump(head_bb, {cg.cur_mem}, body()->location().back());

    cg.enter(exit_bb, exit_bb->param(0));
    cg.cur_bb->jump(brk__bb, {cg.cur_mem}, body()->location().back());

    cg.enter(brk__bb, brk__bb->param(0));
    return cg.world.tuple({}, location());
}

const Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const Def*> args;
    args.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_bb = cg.create_continuation(break_decl());

    // emit call
    auto map_expr = expr()->as<MapExpr>();
    for (auto&& arg : map_expr->args())
        args.push_back(arg.get()->remit(cg));
    args.push_back(fn_expr()->remit(cg));
    args.push_back(break_bb);
    auto fun = map_expr->lhs()->remit(cg);

    args.front() = cg.cur_mem; // now get the current memory monad
    cg.call(fun, args, nullptr, map_expr->location());

    cg.enter(break_bb, break_bb->param(0));
    if (break_bb->num_params() == 2)
        return break_bb->param(1);
    else {
        Array<const Def*> args(break_bb->num_params()-1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            args[i] = break_bb->param(i+1);
        return cg.world.tuple(args, location());
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto continuation = fn_emit_head(cg, location());
    fn_emit_body(cg, location());
    return continuation;
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    init->debug().set(local()->symbol());
    local()->emit(cg, init);
}

const thorin::Def* IdPtrn::emit_cond(CodeGen& cg, const thorin::Def*) const {
    return cg.world.literal(true);
}

void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto val = cg.world.variant_extract(init, index, location());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        arg(i)->emit(cg, num_args() == 1 ? val : cg.world.extract(val, i, location()));
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto cond = cg.world.cmp_eq(cg.world.variant_index(init, location()), cg.world.literal_qu64(index, location()));
    if (num_args() > 0) {
        auto val = cg.world.variant_extract(init, index, location());
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? val : cg.world.extract(val, i, location()));
            cond = cg.world.arithop_and(cond, arg_cond, location());
        }
    }
    return cond;
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        elem(i)->emit(cg, cg.world.extract(init, i, location()));
}

const thorin::Def* TuplePtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    const Def* cond = nullptr;
    for (size_t i = 0, e = num_elems(); i != e; ++i) {
        if (!elem(i)->is_refutable()) continue;

        auto next = elem(i)->emit_cond(cg, cg.world.extract(init, i, location()));
        cond = cond ? cg.world.arithop_and(cond, next) : next;
    }
    return cond ? cond : cg.world.literal(true);
}

const thorin::Def* LiteralPtrn::emit(CodeGen& cg) const {
    auto def = literal()->remit(cg);
    return has_minus() ? cg.world.arithop_minus(def, def->debug()) : def;
}

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.cmp_eq(init, emit(cg));
}

const thorin::Def* CharPtrn::emit(CodeGen& cg) const {
    return chr()->remit(cg);
}

void CharPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* CharPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.cmp_eq(init, emit(cg));
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const { expr()->remit(cg); }
void ItemStmt::emit(CodeGen& cg) const { item()->emit(cg); }

void LetStmt::emit(CodeGen& cg) const {
    ptrn()->emit(cg, init() ? init()->remit(cg) : cg.world.bottom(cg.convert(ptrn()->type()), ptrn()->location()));
}

void AsmStmt::emit(CodeGen& cg) const {
    Array<const thorin::Type*> outs(num_outputs());
    for (size_t i = 0, e = num_outputs(); i != e; ++i)
        outs[i] = cg.convert(output(i)->expr()->type()->as<RefType>()->pointee());

    Array<const Def*> ins(num_inputs());
    for (size_t i = 0, e = num_inputs(); i != e; ++i)
        ins[i] = input(i)->expr()->remit(cg);

    thorin::Assembly::Flags flags = thorin::Assembly::Flags::NoFlag;
    for (auto&& option : options()) {
        if (option == "volatile")
            flags |= thorin::Assembly::Flags::HasSideEffects;
        else if (option == "alignstack")
            flags |= thorin::Assembly::Flags::IsAlignStack;
        else if (option == "intel")
            flags |= thorin::Assembly::Flags::IsIntelDialect;
    }

    auto assembly = cg.world.assembly(outs, cg.cur_mem, ins, asm_template(),
            output_constraints(), input_constraints(), clobbers(), flags, location());

    size_t i = 0;
    cg.cur_mem = assembly->out(i++);
    for (auto&& output: outputs())
        cg.store(output->expr()->lemit(cg), assembly->out(i++), location());
}

//------------------------------------------------------------------------------

void emit(World& world, const Module* mod) {
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
