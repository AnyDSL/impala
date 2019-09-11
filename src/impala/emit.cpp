#include "impala/ast.h"

#include "thorin/primop.h"
#include "thorin/util.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

using namespace thorin;

namespace impala {

class CodeGen {
public:
    CodeGen(World& world)
        : world(world)
    {}

    Debug loc2dbg(Loc loc) { return {loc.filename(), loc.front_line(), loc.front_col(), loc.back_line(), loc.back_col()}; }
    Debug loc2dbg(const char* s, Loc loc) { return {s, loc.filename(), loc.front_line(), loc.front_col(), loc.back_line(), loc.back_col()}; }

    /// Lam of type { @c cn(mem) } or { @c cn(mem, type) } depending on whether @p type is @c nullptr.
    Lam* basicblock(const thorin::Def* type, Debug dbg) {
        auto cn = type ? world.cn({world.type_mem(), type}) : world.cn(world.type_mem());
        auto bb = world.lam(cn, Lam::CC::C, Lam::Intrinsic::None, dbg);
        bb->param(0, {"mem"});
        return bb;
    }
    Lam* basicblock(Debug dbg) { return basicblock(nullptr, dbg); }

    Lam* enter(Lam* bb) {
        cur_bb = bb;
        cur_mem = bb->param(0);
        return bb;
    }

    const Def* lit_one(const Type* type, Debug dbg) {
        if (is_int(type)) return world.lit(convert(type), 1, dbg);
        switch (type->tag()) {
            case PrimType_f16: return world.lit_real(1._r16, dbg);
            case PrimType_f32: return world.lit_real(1._r32, dbg);
            case PrimType_f64: return world.lit_real(1._r64, dbg);
            default: THORIN_UNREACHABLE;
        }
    }

    std::pair<Lam*, const Def*> call(const Def* callee, Defs args, const thorin::Def* ret_type, Debug dbg) {
        if (ret_type == nullptr) {
            cur_bb->app(callee, args, dbg);
            auto next = basicblock({"unreachable"});
            return std::make_pair(next, nullptr);
        }

        std::vector<const thorin::Def*> cont_args;
        cont_args.push_back(world.type_mem());

        // if the return type is a sigma, flatten it
        auto sigma = ret_type->isa<thorin::Sigma>();
        if (sigma && !sigma->isa_nominal()) {
            for (auto op : sigma->ops())
                cont_args.push_back(op);
        } else
            cont_args.push_back(ret_type);

        // next is the return lam
        auto next = world.lam(world.cn(cont_args), dbg);
        next->param(0, {"mem"});

        // create jump to next
        size_t csize = args.size() + 1;
        Array<const Def*> cargs(csize);
        *std::copy(args.begin(), args.end(), cargs.begin()) = next;
        cur_bb->app(callee, cargs, dbg);

        // determine return value
        const Def* ret = nullptr;
        if (sigma) {
            Array<const Def*> params(next->num_params() - 1);
            for (size_t i = 1, e = next->num_params(); i != e; ++i)
                params[i - 1] = next->param(i);
            ret = world.tuple(ret_type, params, {callee->name()});
        } else
            ret = next->param(1, {callee->name()});

        return std::make_pair(next, ret);
    }

    Lam* create_lam(const LocalDecl* decl) {
        auto result = world.lam(convert(decl->type())->as<thorin::Pi>(), decl->debug());
        result->param(0, {"mem"});
        decl->def_ = result;
        return result;
    }

    const Def* load(const Def* ptr, Loc loc) {
        auto dbg = loc2dbg(loc);
        auto l = world.load(cur_mem, ptr, dbg);
        cur_mem = world.extract(l, 0_u32, dbg);
        return world.extract(l, 1_u32, dbg);
    }

    const Def* slot(const Def* type, Debug dbg) {
        auto slot = world.slot(type, cur_mem, dbg);
        cur_mem = slot->out_mem();
        return slot->out_ptr();
    }
    void store(const Def* ptr, const Def* val, Loc loc) { cur_mem = world.store(cur_mem, ptr, val, loc2dbg(loc)); }

    const Def* alloc(const thorin::Def* type, Debug dbg) {
        auto alloc = world.alloc(type, cur_mem, dbg);
        cur_mem = world.extract(alloc, 0_u32, dbg);
        auto result = world.extract(alloc, 1, dbg);
        auto ptr = result->type()->as<thorin::Ptr>();
        if (auto variadic = ptr->pointee()->isa<Variadic>())
            return world.bitcast(world.type_ptr(world.unsafe_variadic(variadic->body()), ptr->addr_space()), result);
        return result;
    }

    const thorin::Def* convert(const Type* type) {
        if (auto t = thorin_type(type))
            return t;
        auto t = convert_rec(type);
        return thorin_type(type) = t;
    }

    const thorin::Def* convert_rec(const Type*);

    const thorin::Def*& thorin_type(const Type* type) { return impala2thorin_[type]; }
    const thorin::Sigma*& thorin_struct_type(const StructType* type) { return struct_type_impala2thorin_[type]; }
    const thorin::Sigma*& thorin_enum_type(const EnumType* type) { return enum_type_impala2thorin_[type]; }

    World& world;
    const Fn* cur_fn = nullptr;
    TypeMap<const thorin::Def*> impala2thorin_;
    GIDMap<const StructType*, const thorin::Sigma*> struct_type_impala2thorin_;
    GIDMap<const EnumType*,   const thorin::Sigma*> enum_type_impala2thorin_;
    Lam* cur_bb = nullptr;
    const Def* cur_mem = nullptr;
};

/*
 * Type
 */

const thorin::Def* CodeGen::convert_rec(const Type* type) {
    if (auto lambda = type->isa<Lambda>()) {
        return world.lam(world.kind_star(), convert(lambda->body()), {lambda->name()});
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
            case PrimType_bool: return world.type_bool();
            case PrimType_i8  :
            case PrimType_u8  : return world.type_int( 8);
            case PrimType_i16 :
            case PrimType_u16 : return world.type_int(16);
            case PrimType_i32 :
            case PrimType_u32 : return world.type_int(32);
            case PrimType_i64 :
            case PrimType_u64 : return world.type_int(64);
            case PrimType_f16 : return world.type_real(16);
            case PrimType_f32 : return world.type_real(32);
            case PrimType_f64 : return world.type_real(64);
            default: THORIN_UNREACHABLE;
        }
    } else if (auto cn = type->isa<FnType>()) {
        std::vector<const thorin::Def*> nops;
        nops.push_back(world.type_mem());
        for (size_t i = 0, e = cn->num_params(); i != e; ++i)
            nops.push_back(convert(cn->param(i)));
        return world.cn(nops);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Def*> nops;
        for (auto&& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world.sigma(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world.sigma(struct_type->num_ops(), {struct_type->struct_decl()->symbol().c_str()});
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (auto&& op : struct_type->ops())
            s->set(i++, convert(op));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return s;
    } else if (auto enum_type = type->isa<EnumType>()) {
        auto s = world.sigma(2, {enum_type->enum_decl()->symbol().c_str()});
        thorin_enum_type(enum_type) = s;
        thorin_type(enum_type) = s;

        auto enum_decl = enum_type->enum_decl();
        thorin::DefSet variants;
        for (auto&& option : enum_decl->option_decls())
            variants.insert(option->variant_type(*this));
        thorin::Array<const thorin::Def*> ops(variants.size());
        std::copy(variants.begin(), variants.end(), ops.begin());

        s->set(0, world.type_int(32));
        s->set(1, world.variant_type(ops));
        thorin_type(enum_type) = nullptr;
        return s;
    } else if (auto ptr = type->isa<PtrType>()) {
        return world.type_ptr(convert(ptr->pointee()), ptr->addr_space());
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world.variadic(definite_array_type->dim(), convert(definite_array_type->elem_type()));
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world.unsafe_variadic(convert(indefinite_array_type->elem_type()));
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
    init = init ? init : cg.world.bot(thorin_type);

    if (is_mut()) {
        def_ = cg.slot(thorin_type, debug());
        cg.cur_mem = cg.world.store(cg.cur_mem, def_, init, cg.loc2dbg(loc()));
    } else {
        def_ = init;
    }
}

const thorin::Def* OptionDecl::variant_type(CodeGen& cg) const {
    std::vector<const thorin::Def*> types;
    for (auto&& arg : args())
        types.push_back(cg.convert(arg->type()));
    if (num_args() == 1) return types.back();
    return cg.world.sigma(types);
}

Lam* Fn::fn_emit_head(CodeGen& cg, Loc loc) const {
    auto t = cg.convert(fn_type())->as<thorin::Pi>();
    return lam_ = cg.world.lam(t, cg.loc2dbg(fn_symbol().remove_quotation().c_str(), loc));
}

void Fn::fn_emit_body(CodeGen& cg, Loc loc) const {
    // setup function nest
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, lam());
    auto old_mem = cg.cur_mem;

    // setup memory
    size_t i = 0;
    auto mem_param = lam()->param(i++, {"mem"});
    cg.cur_mem = mem_param;

    // name params and setup store locs
    for (auto&& param : params()) {
        auto p = lam()->param(i++, cg.loc2dbg(param->symbol().c_str(), param->loc()));
        param->emit(cg, p);
    }

    if (lam()->num_params() != 0 && lam()->params().back()->type()->isa<Pi>())
        ret_param_ = lam()->params().back();

    // descend into body
    auto def = body()->remit(cg);
    if (def) {
        // flatten returned values
        if (auto tuple = body()->type()->isa<TupleType>()) {
            Array<const Def*> ret_values(tuple->num_ops() + 1);
            for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
                ret_values[i + 1] = cg.world.extract(def, i);
            ret_values[0] = cg.cur_mem;
            cg.cur_bb->app(ret_param(), ret_values, cg.loc2dbg(loc.back()));
        } else
            cg.cur_bb->app(ret_param(), {cg.cur_mem, def}, cg.loc2dbg(loc.back()));
    }

    lam()->set_filter(filter() ? filter()->remit(cg) : cg.world.lit_false());
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
    if      (name == "select")   return true;
    else if (name == "sizeof")   return true;
    else if (name == "bitcast")  return true;
    else if (name == "insert")   return true;
    return false;
}

void FnDecl::emit_head(CodeGen& cg) const {
    assert(def_ == nullptr);
    // no code is emitted for primops
    if (is_extern() && abi() == "\"thorin\"" && is_primop(symbol()))
        return;

    // create thorin function
    def_ = fn_emit_head(cg, loc());
    if (is_extern() && abi() == "")
        lam_->make_external();

    // handle main function
    if (symbol() == "main")
        lam()->make_external();
}

void FnDecl::emit(CodeGen& cg) const {
    if (body())
        fn_emit_body(cg, loc());
}

void ExternBlock::emit_head(CodeGen& cg) const {
    for (auto&& fn_decl : fn_decls()) {
        fn_decl->emit_head(cg);
        auto lam = fn_decl->lam();
        if (abi() == "\"C\"")
            lam->set_cc(thorin::Lam::CC::C);
        else if (abi() == "\"device\"")
            lam->set_cc(thorin::Lam::CC::Device);
        else if (abi() == "\"thorin\"" && lam) // no lam for primops
            lam->set_intrinsic();
    }
}

void ModuleDecl::emit(CodeGen&) const {}
void ImplItem::emit(CodeGen&) const {}

void StaticItem::emit_head(CodeGen& cg) const {
    def_ = cg.world.global(cg.world.bot(cg.convert(type()), cg.loc2dbg(loc())));
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
    auto variant_type = cg.convert(enum_type)->op(1)->as<VariantType>();
    auto id = cg.world.lit_int(index(), cg.loc2dbg(loc()));
    if (num_args() == 0) {
        auto bot = cg.world.bot(variant_type);
        def_ = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, bot });
    } else {
        auto lam = cg.world.lam(cg.convert(type())->as<thorin::Pi>(), cg.loc2dbg(symbol().c_str(), loc()));
        auto ret = lam->param(lam->num_params() - 1);
        auto mem = lam->param(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = lam->num_params(); i + 1 < e; i++)
            defs[i-1] = lam->param(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, cg.world.variant(variant_type, option_val) });
        lam->app(ret, { mem, enum_val }, cg.loc2dbg(loc()));
        def_ = lam;
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
const Def* Expr::remit(CodeGen& cg) const { return cg.load(lemit(cg), loc()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world.tuple(); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case LIT_bool: return cg.world.lit_bool(get<bool>());
        case LIT_i8  : return cg.world.lit_int (get<  s8>(), cg.loc2dbg(loc()));
        case LIT_i16 : return cg.world.lit_int (get< s16>(), cg.loc2dbg(loc()));
        case LIT_i32 : return cg.world.lit_int (get< s32>(), cg.loc2dbg(loc()));
        case LIT_i64 : return cg.world.lit_int (get< s64>(), cg.loc2dbg(loc()));
        case LIT_u8  : return cg.world.lit_int (get<  u8>(), cg.loc2dbg(loc()));
        case LIT_u16 : return cg.world.lit_int (get< u16>(), cg.loc2dbg(loc()));
        case LIT_u32 : return cg.world.lit_int (get< u32>(), cg.loc2dbg(loc()));
        case LIT_u64 : return cg.world.lit_int (get< u64>(), cg.loc2dbg(loc()));
        case LIT_f16 : return cg.world.lit_real(get< r16>(), cg.loc2dbg(loc()));
        case LIT_f32 : return cg.world.lit_real(get< r32>(), cg.loc2dbg(loc()));
        case LIT_f64 : return cg.world.lit_real(get< r64>(), cg.loc2dbg(loc()));
        default: THORIN_UNREACHABLE;
    }
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world.lit_int<u8>(value(), cg.loc2dbg(loc()));
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world.lit_int<u8>(values_[i], cg.loc2dbg(loc()));

    return cg.world.tuple(args, cg.loc2dbg(loc()));
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = src()->remit(cg);
    auto src_type = src()->type();
    auto dst_type = type();
    auto dst = cg.convert(dst_type);
    auto dbg = cg.loc2dbg(loc());

    if (is_int(src_type) || is_bool(src_type)) {
        if (is_signed(src_type)) {
            if (is_int(dst_type) || is_bool(dst_type)) {
                return cg.world.op<Conv::s2s>(dst, def, dbg);
            } else {
                return cg.world.op<Conv::s2r>(dst, def, dbg);
            }
        } else {
            if (is_int(dst_type) || is_bool(dst_type)) {
                return cg.world.op<Conv::u2u>(dst, def, dbg);
            } else {
                return cg.world.op<Conv::u2r>(dst, def, dbg);
            }
        }
    } else {
        if (is_int(dst_type) || is_bool(dst_type)) {
            if (is_signed(dst_type))
                return cg.world.op<Conv::r2s>(dst, def, dbg);
            else
                return cg.world.op<Conv::r2u>(dst, def, dbg);
        } else {
            return cg.world.op<Conv::r2r>(dst, def, dbg);
        }
    }
    THORIN_UNREACHABLE;
}

const Def* RValueExpr::lemit(CodeGen& cg) const {
    assert(src()->type()->isa<RefType>());
    return src()->lemit(cg);
}

const Def* RValueExpr::remit(CodeGen& cg) const {
    if (src()->type()->isa<RefType>())
        return cg.load(lemit(cg), loc());
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
    return value_decl()->is_mut() || global ? cg.load(def, loc()) : def;
}

static flags_t type2wmode(const Type* type) {
    return is_signed(type) ? WMode::nsw : WMode::none;
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = rhs()->lemit(cg);
            auto val = cg.load(var, loc());
            auto one = cg.lit_one(type(), cg.loc2dbg(loc()));
            if (is_int(type())) {
                auto mode = type2wmode(type());
                if (tag() == INC)
                    val = cg.world.op<WOp::add>(mode, val, one, cg.loc2dbg(loc()));
                else
                    val = cg.world.op<WOp::sub>(mode, val, one, cg.loc2dbg(loc()));
            } else {
                if (tag() == INC)
                    val = cg.world.op<ROp::add>(val, one, cg.loc2dbg(loc()));
                else
                    val = cg.world.op<ROp::sub>(val, one, cg.loc2dbg(loc()));
            }
            cg.store(var, val, loc());
            return val;
        }
        case ADD: return rhs()->remit(cg);
        case SUB:
            if (is_int(type())) {
                auto mode = type2wmode(type());
                return cg.world.op_WOp_minus(mode, rhs()->remit(cg), cg.loc2dbg(loc()));
            } else {
                return cg.world.op_ROp_minus(rhs()->remit(cg), cg.loc2dbg(loc()));
            }
        case NOT: return cg.world.op_IOp_inot(rhs()->remit(cg), cg.loc2dbg(loc()));
        case TILDE: {
            auto def = rhs()->remit(cg);
            auto ptr = cg.alloc(def->type(), cg.loc2dbg(loc()));
            cg.store(ptr, def, loc());
            return ptr;
        }
        case AND: {
            if (rhs()->type()->isa<RefType>())
                return rhs()->lemit(cg);

            auto def = rhs()->remit(cg);
            if (is_const(def))
                return cg.world.global(def, /*mutable*/ false, cg.loc2dbg(loc()));

            auto slot = cg.slot(cg.convert(rhs()->type()), cg.loc2dbg(loc()));
            cg.store(slot, def, loc());
            return slot;
        }
        case MUT: {
            return rhs()->lemit(cg);
        }
        case RUNRUN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.run(def, cg.loc2dbg(loc()));
        }
        case HLT: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.hlt(def, cg.loc2dbg(loc()));
        }
        case KNOWN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.known(def, cg.loc2dbg(loc()));
        }
        case OR:
        case OROR:
            THORIN_UNREACHABLE;
        default:
            return cg.load(lemit(cg), loc());
    }
}

const Def* PrefixExpr::lemit(CodeGen& cg) const {
    assert(tag() == MUL);
    return rhs()->remit(cg);
}

void Expr::emit_branch(CodeGen& cg, Lam* jump_t, Lam* jump_f) const {
    auto expr_t = cg.basicblock(cg.loc2dbg("expr_t", loc().back()));
    auto expr_f = cg.basicblock(cg.loc2dbg("expr_f", loc().back()));
    auto cond = remit(cg);
    cg.cur_bb->branch(cond, expr_t, expr_f, cg.cur_mem, cg.loc2dbg(loc().back()));
    cg.enter(expr_t);
    expr_t->app(jump_t, { cg.cur_mem });
    cg.enter(expr_f);
    expr_f->app(jump_f, { cg.cur_mem });
}

void InfixExpr::emit_branch(CodeGen& cg, Lam* jump_t, Lam* jump_f) const {
    auto jump_type = jump_t->type();
    switch (tag()) {
        case OROR: {
                auto or_f = cg.world.lam(jump_type, cg.loc2dbg("or_f", loc().back()));
                lhs()->emit_branch(cg, jump_t, or_f);
                cg.enter(or_f);
                rhs()->emit_branch(cg, jump_t, jump_f);
            }
            break;
        case ANDAND: {
                auto and_t = cg.world.lam(jump_type, cg.loc2dbg("and_t", loc().back()));
                lhs()->emit_branch(cg, and_t, jump_f);
                cg.enter(and_t);
                rhs()->emit_branch(cg, jump_t, jump_f);
            }
            break;
        default:
            return Expr::emit_branch(cg, jump_t, jump_f);
    }
}

#if 0
const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case OROR:
        case ANDAND: {
            auto result    = cg.basicblock(cg.world.type_bool(), cg.loc2dbg("infix_result", loc().back()));
            auto jump_type = cg.world.cn({ cg.world.type_mem() });
            auto jump_t    = cg.world.lam(jump_type, cg.loc2dbg("jump_t", loc().back()));
            auto jump_f    = cg.world.lam(jump_type, cg.loc2dbg("jump_t", loc().back()));
            emit_branch(cg, jump_t, jump_f);
            jump_t->app(result, { jump_t->param(0), cg.world.lit_true() });
            jump_f->app(result, { jump_f->param(0), cg.world.lit_false() });
            return cg.enter(result)->param(1);
        }
        default: {
            const TokenTag op = (TokenTag) tag();
            auto dbg = cg.loc2dbg(loc());

            if (Token::is_assign(op)) {
                auto lvar = lhs()->lemit(cg);
                auto rdef = rhs()->remit(cg);
                bool s = false; // TODO
                bool r = false; // TODO
                switch (op) {
                    case     ASGN: break;
                    case ADD_ASGN:
                        if (r)
                            rdef = cg.world.op<ROp::radd>(cg.load(lvar, loc()), rdef, dbg);
                        else if (s)
                            rdef = cg.world.op<WOp:: add>(WMode::nsw, cg.load(lvar, loc()), rdef, dbg);
                        else
                            rdef = cg.world.op<WOp:: add>(WMode::none, cg.load(lvar, loc()), rdef, dbg);
                        break;
                    case SUB_ASGN: rdef = cg.world.op<WOp::sub>(mode, cg.load(lvar, loc()), rdef, dbg); break;
                    case MUL_ASGN: rdef = cg.world.op<WOp::mul>(mode, cg.load(lvar, loc()), rdef, dbg); break;
                    case SHL_ASGN: rdef = cg.world.op<WOp::shl>(cg.load(lvar, loc()), rdef, dbg); break;

                    case AND_ASGN: rdef = cg.world.op<IOp::iand>(cg.load(lvar, loc()), rdef, dbg); break;
                    case  OR_ASGN: rdef = cg.world.op<IOp::ior >(cg.load(lvar, loc()), rdef, dbg); break;
                    case XOR_ASGN: rdef = cg.world.op<IOp::ixor>(cg.load(lvar, loc()), rdef, dbg); break;
                    case SHR_ASGN: rdef = cg.world.op<WOp::ishr>(cg.load(lvar, loc()), rdef, dbg); break;
                    case DIV_ASGN: rdef = cg.world.op<WOp::div>(cg.load(lvar, loc()), rdef, dbg); break;
                    case REM_ASGN: rdef = cg.world.op<WOp::rem>(cg.load(lvar, loc()), rdef, dbg); break;
                    default: THORIN_UNREACHABLE;
                }

                cg.store(lvar, rdef, loc());
                return cg.world.tuple();
            }

            auto ldef = lhs()->remit(cg);
            auto rdef = rhs()->remit(cg);

            switch (op) {
                case  EQ: return cg.world.cmp_eq     (ldef, rdef, dbg);
                case  NE: return cg.world.cmp_ne     (ldef, rdef, dbg);
                case  LT: return cg.world.cmp_lt     (ldef, rdef, dbg);
                case  LE: return cg.world.cmp_le     (ldef, rdef, dbg);
                case  GT: return cg.world.cmp_gt     (ldef, rdef, dbg);
                case  GE: return cg.world.cmp_ge     (ldef, rdef, dbg);
                case  OR: return cg.world.arithop_or (ldef, rdef, dbg);
                case XOR: return cg.world.arithop_xor(ldef, rdef, dbg);
                case AND: return cg.world.arithop_and(ldef, rdef, dbg);
                case SHL: return cg.world.arithop_shl(ldef, rdef, dbg);
                case SHR: return cg.world.arithop_shr(ldef, rdef, dbg);
                case ADD: return cg.world.arithop_add(ldef, rdef, dbg);
                case SUB: return cg.world.arithop_sub(ldef, rdef, dbg);
                case MUL: return cg.world.arithop_mul(ldef, rdef, dbg);
                case DIV: return cg.world.arithop_div(ldef, rdef, dbg);
                case REM: return cg.world.arithop_rem(ldef, rdef, dbg);
                default: THORIN_UNREACHABLE;
            }
        }
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    auto var = lhs()->lemit(cg);
    auto def = cg.load(var, loc());
    auto one = cg.lit_one(def->type(), cg.loc2dbg(loc()));
    cg.store(var, cg.world.arithop(tag() == INC ? ArithOp_add : ArithOp_sub, def, one, cg.loc2dbg(loc())), loc());
    return def;
}
#endif

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.tuple(thorin_args, cg.loc2dbg(loc()));
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    return cg.world.pack(count(), value()->remit(cg));
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.tuple(thorin_args, cg.loc2dbg(loc()));
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    auto arity = dim()->remit(cg);
    auto elem = cg.convert(type()->as<IndefiniteArrayType>()->elem_type());
    return cg.world.pack(arity, cg.world.bot(elem), cg.loc2dbg(loc()));
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (auto&& elem : elems())
        defs[elem->field_decl()->index()] = elem->expr()->remit(cg);
    return cg.world.tuple(cg.convert(type())->as<thorin::Sigma>(), defs, cg.loc2dbg(loc()));
}

const Def* TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const { THORIN_UNREACHABLE; }

const Def* MapExpr::lemit(CodeGen& cg) const {
    auto agg = lhs()->lemit(cg);
    return cg.world.unsafe_lea(agg, arg(0)->remit(cg), cg.loc2dbg(loc()));
}

const Def* MapExpr::remit(CodeGen& cg) const {
    auto ltype = unpack_ref_type(lhs()->type());

    if (auto cn = ltype->isa<FnType>()) {
        const Def* dst = nullptr;

        // Handle primops here
        if (auto type_expr = lhs()->isa<TypeAppExpr>()) { // Bitcast, sizeof and select are all polymorphic
            auto callee = type_expr->lhs()->skip_rvalue();
            if (auto path = callee->isa<PathExpr>()) {
                if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
                    if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
                        auto name = fn_decl->fn_symbol().remove_quotation();
                        if (name == "bitcast") {
                            return cg.world.bitcast(cg.convert(type_expr->type_arg(0)), arg(0)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "select") {
                            return cg.world.op_select(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "insert") {
                            return cg.world.unsafe_insert(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "sizeof") {
                            return cg.world.op_sizeof(cg.convert(type_expr->type_arg(0)), cg.loc2dbg(loc()));
                        } else if (name == "undef") {
                            return cg.world.bot(cg.convert(type_expr->type_arg(0)), cg.loc2dbg(loc()));
                        } else if (name == "reserve_shared") {
                            auto ptr = cg.convert(type());
                            auto cn = cg.world.cn({
                                cg.world.type_mem(), cg.world.type_int(32),
                                cg.world.cn({ cg.world.type_mem(), ptr }) });
                            auto cont = cg.world.lam(cn, cg.loc2dbg("reserve_shared", loc()));
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr = cg.convert(arg(1)->type());
                            auto cn = cg.world.cn({
                                cg.world.type_mem(), cg.world.type_int(32), ptr, poly_type,
                                cg.world.cn({ cg.world.type_mem(), poly_type }) });
                            auto cont = cg.world.lam(cn, cg.loc2dbg("atomic", loc()));
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr = cg.convert(arg(0)->type());
                            auto poly_type = ptr->as<thorin::Ptr>()->pointee();
                            auto cn = cg.world.cn({
                                cg.world.type_mem(), ptr, poly_type, poly_type,
                                cg.world.cn({ cg.world.type_mem(), poly_type, cg.world.type_bool() })
                            });
                            auto cont = cg.world.lam(cn, cg.loc2dbg("cmpxchg", loc()));
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto string_type = cg.world.type_ptr(cg.world.unsafe_variadic(cg.world.type_int(8)));
                            auto cn = cg.world.cn({
                                cg.world.type_mem(), string_type, poly_type,
                                cg.world.cn({ cg.world.type_mem() }) });
                            auto cont = cg.world.lam(cn, cg.loc2dbg("pe_info", loc()));
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_known") {
                            auto poly_type = cg.convert(arg(0)->type());
                            auto cn = cg.world.cn({
                                cg.world.type_mem(), poly_type,
                                cg.world.cn({ cg.world.type_mem(), cg.world.type_bool() }) });
                            auto cont = cg.world.lam(cn, cg.loc2dbg("pe_known", loc()));
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

        auto ret_type = num_args() == cn->num_params() ? nullptr : cg.convert(cn->return_type());
        const Def* ret;
        std::tie(cg.cur_bb, ret) = cg.call(dst, defs, ret_type, cg.loc2dbg((dst->name() + "_cont").c_str(), loc()));
        if (ret_type)
            cg.cur_mem = cg.cur_bb->param(0);

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>()) {
        auto index = arg(0)->remit(cg);
        return cg.world.unsafe_extract(lhs()->remit(cg), index, cg.loc2dbg(loc()));
    }
    THORIN_UNREACHABLE;
}

const Def* FieldExpr::lemit(CodeGen& cg) const {
    auto value = lhs()->lemit(cg);
    return cg.world.unsafe_lea(value, cg.world.lit_nat(index(), cg.loc2dbg(loc())), cg.loc2dbg(loc()));
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.world.extract(lhs()->remit(cg), index(), cg.loc2dbg(loc()));
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

    auto jump_type = cg.world.cn({ cg.world.type_mem() });
    auto if_then = cg.world.lam(jump_type, cg.loc2dbg("if_then", then_expr()->loc().front()));
    auto if_else = cg.world.lam(jump_type, cg.loc2dbg("if_else", else_expr()->loc().front()));
    auto if_join = thorin_type ? cg.basicblock(thorin_type, cg.loc2dbg("if_join", loc().back())) : nullptr; // TODO rewrite with bottom type

    cond()->emit_branch(cg, if_then, if_else);

    cg.enter(if_then);
    if (auto tdef = then_expr()->remit(cg))
        cg.cur_bb->app(if_join, {cg.cur_mem, tdef}, cg.loc2dbg(loc().back()));

    cg.enter(if_else);
    if (auto fdef = else_expr()->remit(cg))
        cg.cur_bb->app(if_join, {cg.cur_mem, fdef}, cg.loc2dbg(loc().back()));

    if (thorin_type)
        return cg.enter(if_join)->param(1);
    return nullptr; // TODO use bottom type
}

const Def* MatchExpr::remit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());

    auto join = thorin_type ? cg.basicblock(thorin_type, cg.loc2dbg("match_join", loc().back())) : nullptr; // TODO rewrite with bottom type

    auto matcher = expr()->remit(cg);
    auto enum_type = expr()->type()->isa<EnumType>();
    bool is_integer = is_int(expr()->type());
    bool is_simple = enum_type && enum_type->enum_decl()->is_simple();

    if (is_integer || is_simple) {
        // integers: match lam
        Lam* otherwise = nullptr;
        size_t num_targets = num_arms();
        Array<const Def*> defs(num_targets);
        Array<Lam*> targets(num_targets);

        for (size_t i = 0, e = num_targets; i != e; ++i) {
            // last pattern will always be taken
            if (!arm(i)->ptrn()->is_refutable() || i == e - 1) {
                num_targets = i;
                arm(i)->ptrn()->emit(cg, matcher);
                otherwise = cg.basicblock(cg.loc2dbg("otherwise", arm(i)->loc().front()));
                break;
            } else {
                if (is_integer) {
                    defs[i] = arm(i)->ptrn()->emit(cg);
                } else {
                    auto enum_ptrn = arm(i)->ptrn()->as<EnumPtrn>();
                    auto option_decl = enum_ptrn->path()->decl()->as<OptionDecl>();
                    defs[i] = cg.world.lit_nat(option_decl->index(), cg.loc2dbg(arm(i)->ptrn()->loc()));
                }
                targets[i] = cg.basicblock(cg.loc2dbg("case", arm(i)->loc().front()));
            }
        }

        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world.extract(matcher, 0_u32, matcher->debug());
        cg.cur_bb->match(matcher_int, otherwise, defs, targets, cg.loc2dbg("match", loc().front()));

        for (size_t i = 0; i != num_targets; ++i) {
            cg.enter(targets[i]);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, cg.loc2dbg(loc().back()));
        }

        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise) {
            cg.enter(otherwise);
            if (auto def = arm(num_targets)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, cg.loc2dbg(loc().back()));
        }
    } else {
        // general case: if/else
        for (size_t i = 0, e = num_arms(); i != e; ++i) {
            auto case_t = cg.basicblock(cg.loc2dbg("case_t", arm(i)->loc().front()));
            auto case_f = cg.basicblock(cg.loc2dbg("case_f", arm(i)->loc().front()));

            arm(i)->ptrn()->emit(cg, matcher);

            // last pattern will always be taken
            auto cond = i == e - 1
                ? cg.world.lit_true()
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.cur_bb->branch(cond, case_t, case_f, cg.cur_mem, cg.loc2dbg(arm(i)->ptrn()->loc().back()));

            cg.enter(case_t);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, cg.loc2dbg(arm(i)->loc().back()));

            cg.enter(case_f);
        }
    }

    if (thorin_type)
        return cg.enter(join)->param(1);
    return nullptr; // TODO use bottom type
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    auto head_bb = cg.world.lam(cg.world.cn({cg.world.type_mem()}), Lam::CC::C, Lam::Intrinsic::None, cg.loc2dbg("while_head", loc().front()));
    head_bb->param(0, {"mem"});

    auto jump_type = cg.world.cn({ cg.world.type_mem() });
    auto body_bb = cg.world.lam(jump_type, cg.loc2dbg("while_body", body()->loc().front()));
    auto exit_bb = cg.world.lam(jump_type, cg.loc2dbg("while_exit", body()->loc().back()));
    auto cont_bb = cg.create_lam(continue_decl());
    auto brk__bb = cg.create_lam(break_decl());

    cg.cur_bb->app(head_bb, {cg.cur_mem}, cg.loc2dbg(cond()->loc().back()));

    cg.enter(head_bb);
    cond()->emit_branch(cg, body_bb, exit_bb);

    cg.enter(body_bb);
    body()->remit(cg);
    cg.cur_bb->app(cont_bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().back()));

    cg.enter(cont_bb);
    cg.cur_bb->app(head_bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().back()));

    cg.enter(exit_bb);
    cg.cur_bb->app(brk__bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().back()));

    cg.enter(brk__bb);
    return cg.world.tuple();
}

const Def* ForExpr::remit(CodeGen& cg) const {
    std::vector<const Def*> args;
    args.push_back(nullptr); // reserve for mem but set later - some other args may update the monad

    auto break_bb = cg.create_lam(break_decl());

    // emit call
    auto map_expr = expr()->as<MapExpr>();
    for (auto&& arg : map_expr->args())
        args.push_back(arg.get()->remit(cg));
    args.push_back(fn_expr()->remit(cg));
    args.push_back(break_bb);
    auto fun = map_expr->lhs()->remit(cg);

    args.front() = cg.cur_mem; // now get the current memory monad
    cg.call(fun, args, nullptr, cg.loc2dbg(map_expr->loc()));

    cg.enter(break_bb);
    if (break_bb->num_params() == 2)
        return break_bb->param(1);
    else {
        Array<const Def*> args(break_bb->num_params()-1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            args[i] = break_bb->param(i+1);
        return cg.world.tuple(args, cg.loc2dbg(loc()));
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto lam = fn_emit_head(cg, loc());
    fn_emit_body(cg, loc());
    return lam;
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    local()->emit(cg, init);
}

const thorin::Def* IdPtrn::emit_cond(CodeGen& cg, const thorin::Def*) const { return cg.world.lit_true(); }

#if 0
void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1), cg.loc2dbg(loc()));
    for (size_t i = 0, e = num_args(); i != e; ++i) {
        arg(i)->emit(cg, num_args() == 1 ? variant : cg.world.extract(variant, i, cg.loc2dbg(loc())));
    }
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto cond = cg.world.op<ICmp::e>(cg.world.extract(init, 0_u32, cg.loc2dbg(loc())), cg.world.lit_nat(index, cg.loc2dbg(loc())));
    if (num_args() > 0) {
        auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
        auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1, cg.loc2dbg(loc())), cg.loc2dbg(loc()));
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? variant : cg.world.extract(variant, i, cg.loc2dbg(loc())));
            cond = cg.world.arithop_and(cond, arg_cond, cg.loc2dbg(loc()));
        }
    }
    return cond;
}
#endif

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        elem(i)->emit(cg, cg.world.extract(init, i, cg.loc2dbg(loc())));
}

const thorin::Def* TuplePtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    const Def* cond = nullptr;
    for (size_t i = 0, e = num_elems(); i != e; ++i) {
        if (!elem(i)->is_refutable()) continue;

        auto next = elem(i)->emit_cond(cg, cg.world.extract(init, i, cg.loc2dbg(loc())));
        cond = cond ? cg.world.op<IOp::iand>(cond, next) : next;
    }
    return cond ? cond : cg.world.lit_true();
}

#if 0
const thorin::Def* LiteralPtrn::emit(CodeGen& cg) const {
    auto def = literal()->remit(cg);
    return has_minus() ? cg.world.arithop_minus(def, def->debug()) : def;
}
#endif

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.op<ICmp::e>(init, emit(cg));
}

const thorin::Def* CharPtrn::emit(CodeGen& cg) const {
    return chr()->remit(cg);
}

void CharPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* CharPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.op<ICmp::e>(init, emit(cg));
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const { expr()->remit(cg); }
void ItemStmt::emit(CodeGen& cg) const { item()->emit(cg); }

void LetStmt::emit(CodeGen& cg) const {
    ptrn()->emit(cg, init() ? init()->remit(cg) : cg.world.bot(cg.convert(ptrn()->type()), cg.loc2dbg(ptrn()->loc())));
}

void AsmStmt::emit(CodeGen& /*cg*/) const {
    /*
    Array<const thorin::Def*> outs(num_outputs());
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
            output_constraints(), input_constraints(), clobbers(), flags, cg.loc2dbg(loc()));

    size_t i = 0;
    cg.cur_mem = assembly->out(i++);
    for (auto&& output: outputs())
        cg.store(output->expr()->lemit(cg), assembly->out(i++), loc());
    */
}

//------------------------------------------------------------------------------

void emit(World& world, const Module* mod) {
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
