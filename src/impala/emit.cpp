#include "impala/ast.h"

#include "dialects/math/math.h"
#include "dialects/mem/mem.h"
#include "dialects/core/core.h"
#include "dialects/math/math.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

using namespace thorin;

namespace impala {

class CodeGen {
public:
    CodeGen(World& world)
        : world(world)
    {}

    const Def* loc2dbg(Loc loc) {
        return world.dbg({
            {loc.file(), {loc.begin_row(), loc.begin_col()}, {loc.finis_row(), loc.finis_col()}}
        });
    }
    const Def* loc2dbg(const char* s, Loc loc) {
        return world.dbg({
            s,
            {loc.file(), {loc.begin_row(), loc.begin_col()}, {loc.finis_row(), loc.finis_col()}}
        });
    }

    const Def* debug(const Decl* decl) {
        return world.dbg({
            decl->symbol().c_str(),
            {decl->loc().file(),
                                {decl->loc().begin_row(), decl->loc().begin_col()},
                                {decl->loc().finis_row(), decl->loc().finis_col()}}
        });
    }

    /// Lam of type { @c cn(mem) } or { @c cn(mem, type) } depending on whether @p type is @c nullptr.
    Lam* basicblock(const thorin::Def* type, const Def* dbg) {
        auto cn = type ? world.cn({mem::type_mem(world), type}) : world.cn(mem::type_mem(world));
        auto bb = world.nom_lam(cn, dbg);
        bb->var(0, world.dbg("mem"));
        return bb;
    }
    Lam* basicblock(const Def* dbg) { return basicblock(nullptr, dbg); }

    Lam* enter(Lam* bb) {
        cur_bb = bb;
        cur_mem = bb->var(0_s);
        return bb;
    }

    const Def* lit_one(const Type* type, const Def* dbg) {
        if (is_int(type)) return world.lit(convert(type), 1, dbg);
        switch (type->tag()) {
            case PrimType_f16: return math::lit_f(world, 1._f16, dbg);
            case PrimType_f32: return math::lit_f(world, 1._f32, dbg);
            case PrimType_f64: return math::lit_f(world, 1._f64, dbg);
            default: thorin::unreachable();
        }
    }

    std::pair<Lam*, const Def*> call(const Def* callee, Defs args, const thorin::Def* ret_type, const Def* dbg) {
        if (ret_type == nullptr) {
            cur_bb->app(false, callee, args, dbg);
            auto next = basicblock(world.dbg("unreachable"));
            return std::make_pair(next, nullptr);
        }

        std::vector<const thorin::Def*> cont_args;
        cont_args.push_back(mem::type_mem(world));

        // if the return type is a sigma, flatten it
        auto sigma = ret_type->isa<thorin::Sigma>();
        if (sigma && !sigma->isa_nom()) {
            for (auto op : sigma->ops())
                cont_args.push_back(op);
        } else
            cont_args.push_back(ret_type);

        // next is the return lam
        auto next = world.nom_lam(world.cn(cont_args), dbg);
        next->var(0, world.dbg("mem"));

        // create jump to next
        size_t csize = args.size() + 1;
        Array<const Def*> cargs(csize);
        *std::copy(args.begin(), args.end(), cargs.begin()) = next;
        cur_bb->app(false, callee, cargs, dbg);

        // determine return value
        const Def* ret = nullptr;
        if (sigma) {
            Array<const Def*> vars(next->num_vars() - 1);
            for (size_t i = 1, e = next->num_vars(); i != e; ++i)
                vars[i - 1] = next->var(i);
            ret = world.tuple(ret_type, vars, callee->dbg());
        } else
            ret = next->var(1, callee->dbg());

        return std::make_pair(next, ret);
    }

    Lam* create_lam(const LocalDecl* decl) {
        auto result = world.nom_lam(convert(decl->type())->as<thorin::Pi>(), debug(decl));
        result->var(0, world.dbg("mem"));
        decl->def_ = result;
        return result;
    }

    const Def* handle_mem_res(const Def* mem_res) {
        auto [mem, res] = mem_res->projs<2>();
        cur_mem = mem;
        return res;
    }

    const Def* load(const Def*  ptr, Loc loc) { return handle_mem_res(mem::op_load(cur_mem, ptr, loc2dbg(loc))); }
    const Def* slot(const Def* type, const Def* dbg) { return handle_mem_res(mem::op_slot(type, cur_mem, dbg)); }

    void store(const Def* ptr, const Def* val, Loc loc) { cur_mem = mem::op_store(cur_mem, ptr, val, loc2dbg(loc)); }

    const Def* alloc(const thorin::Def* type, const Def* dbg) {
        auto alloc = mem::op_alloc(type, cur_mem, dbg);
        cur_mem = world.extract(alloc, 2_s, 0_s, dbg);
        auto result = world.extract(alloc, 2, 1, dbg);
        auto ptr = thorin::match<mem::Ptr, true>(result->type());
        auto [pointee, addr_space] = ptr->args<2>();
        if (auto arr = pointee->isa<Arr>())
            return core::op_bitcast(mem::type_ptr(world.arr_unsafe(arr->body()), addr_space), result);
        return result;
    }

    const thorin::Def* rev_diff(const thorin::Def* /*primal*/) { return nullptr; /*world.op_rev_diff(primal);*/ }

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
        auto body = convert(lambda->body());
        auto pi = world.pi(world.type(), body->type());
        return world.lam(pi, body, world.dbg(lambda->name()));
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
            // clang-format off
            case PrimType_bool: return world.type_bool();
            case PrimType_i8  :
            case PrimType_u8  : return world.type_int( 8);
            case PrimType_i16 :
            case PrimType_u16 : return world.type_int(16);
            case PrimType_i32 :
            case PrimType_u32 : return world.type_int(32);
            case PrimType_i64 :
            case PrimType_u64 : return world.type_int(64);
            case PrimType_f16 : return math::type_f16(world);
            case PrimType_f32 : return math::type_f32(world);
            case PrimType_f64 : return math::type_f64(world);
            // clang-format on
            default: thorin::unreachable();
        }
    } else if (auto cn = type->isa<FnType>()) {
        std::vector<const thorin::Def*> nops;
        nops.push_back(mem::type_mem(world));
        for (size_t i = 0, e = cn->num_params(); i != e; ++i)
            nops.push_back(convert(cn->param(i)));
        return world.cn(nops);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Def*> nops;
        for (auto&& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world.sigma(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world.nom_sigma(struct_type->num_ops(), world.dbg(struct_type->struct_decl()->symbol().c_str()));
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (auto&& op : struct_type->ops())
            s->set(i++, convert(op));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return s;
#if 0
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
#endif
    } else if (auto ptr = type->isa<PtrType>()) {
        return mem::type_ptr(convert(ptr->pointee()), ptr->addr_space());
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world.arr(definite_array_type->dim(), convert(definite_array_type->elem_type()));
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world.arr_unsafe(convert(indefinite_array_type->elem_type()));
    } else if (type->isa<NoRetType>()) {
        return nullptr; // TODO use bottom type - once it is available in thorin
    }
    Stream stream;
    type->stream(stream);
    std::cout.flush();
    thorin::unreachable();
}

/*
 * Decls and Function
 */

void LocalDecl::emit(CodeGen& cg, const Def* init) const {
    assert(def_ == nullptr);

    auto thorin_type = cg.convert(type());
    init = init ? init : cg.world.bot(thorin_type);

    if (is_mut()) {
        def_ = cg.slot(thorin_type, cg.debug(this));
        cg.cur_mem = mem::op_store(cg.cur_mem, def_, init, cg.loc2dbg(loc()));
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
    return lam_ = cg.world.nom_lam(t, cg.loc2dbg(fn_symbol().remove_quotation().c_str(), loc));
}

void Fn::fn_emit_body(CodeGen& cg, Loc loc) const {
    // setup function nest
    auto old_fn = cg.cur_fn;
    auto old_bb = cg.cur_bb;
    cg.cur_fn = this;
    cg.cur_bb = lam();

    auto old_mem = cg.cur_mem;

    // setup memory
    size_t i = 0;
    auto mem_param = lam()->var(i++, cg.world.dbg("mem"));
    cg.cur_mem = mem_param;

    // name vars and setup store locs
    for (auto&& param : params()) {
        auto var = lam()->var(i++, cg.loc2dbg(param->symbol().c_str(), param->loc()));
        param->emit(cg, var);
    }

    if (lam()->num_vars() != 0 && lam()->vars().back()->type()->isa<Pi>())
        ret_param_ = lam()->vars().back();

    // descend into body
    auto def = body()->remit(cg);
    if (def) {
        // flatten returned values
        if (auto tuple = body()->type()->isa<TupleType>()) {
            Array<const Def*> ret_values(tuple->num_ops() + 1);
            for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
                ret_values[i + 1] = cg.world.extract(def, e, i);
            ret_values[0] = cg.cur_mem;
            cg.cur_bb->app(false, ret_param(), ret_values, cg.loc2dbg(loc.finis()));
        } else
            cg.cur_bb->app(false, ret_param(), {cg.cur_mem, def}, cg.loc2dbg(loc.finis()));
    }

    lam()->set_filter(filter() ? filter()->remit(cg) : cg.world.lit_ff());
    cg.cur_mem = old_mem;

    cg.cur_fn = old_fn;
    cg.cur_bb = old_bb;
}

/*
 * items
 */

void Module::emit(CodeGen& cg) const {
    for (auto&& item : items()) item->emit_head(cg);
    for (auto&& item : items()) item->emit(cg);
}

static bool is_primop(const Symbol& name) {
    // clang-format off
    if      (name == "select")   return true;
    else if (name == "sizeof")   return true;
    else if (name == "bitcast")  return true;
    else if (name == "insert")   return true;
    else if (name == "rev_diff") return true;
    // clang-format on
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
    if (symbol() == "main" && !lam()->is_external())
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
        if (abi() == "\"C\"") {
            // do nothing
        } else if (abi() == "\"device\"")
            assert(false && "TODO");
        else if (abi() == "\"thorin\"" && lam) {
            assert(false && "TODO");
        }
    }
}

void ModuleDecl::emit(CodeGen&) const {}
void ImplItem::emit(CodeGen&) const {}

void StaticItem::emit_head(CodeGen& cg) const {
    auto t = cg.convert(type());
    auto bot = cg.world.bot(t);
    auto g = cg.world.global(mem::type_ptr(t), is_mut(), cg.loc2dbg(loc()));
    g->set(bot);
    def_ = g;
}

void StaticItem::emit(CodeGen& cg) const {
    if (init()) def_->as_nom<Global>()->set(init()->remit(cg));
}

void StructDecl::emit_head(CodeGen& cg) const {
    cg.convert(type());
}

void OptionDecl::emit(CodeGen& /*cg*/) const {
#if 0
    auto enum_type = enum_decl()->type()->as<EnumType>();
    auto variant_type = cg.convert(enum_type)->op(1)->as<VariantType>();
    auto id = cg.world.lit_idx(index(), cg.loc2dbg(loc()));
    if (num_args() == 0) {
        auto bot = cg.world.bot(variant_type);
        def_ = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, bot });
    } else {
        auto lam = cg.world.nom_lam(cg.convert(type())->as<thorin::Pi>(), cg.loc2dbg(symbol().c_str(), loc()));
        auto ret = lam->var(lam->num_vars() - 1);
        auto mem = lam->var(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = lam->num_vars(); i + 1 < e; i++)
            defs[i-1] = lam->var(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, cg.world.variant(variant_type, option_val) });
        lam->app(ret, { mem, enum_val }, cg.loc2dbg(loc()));
        def_ = lam;
    }
#endif
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

const Def* Expr::lemit(CodeGen&) const { thorin::unreachable(); }
const Def* Expr::remit(CodeGen& cg) const { return cg.load(lemit(cg), loc()); }
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world.tuple(); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        // clang-format off
        case LIT_bool: return cg.world.lit_bool(get<bool>());
        case LIT_i8  : return cg.world.lit_idx (get<  s8>(), cg.loc2dbg(loc()));
        case LIT_i16 : return cg.world.lit_idx (get< s16>(), cg.loc2dbg(loc()));
        case LIT_i32 : return cg.world.lit_idx (get< s32>(), cg.loc2dbg(loc()));
        case LIT_i64 : return cg.world.lit_idx (get< s64>(), cg.loc2dbg(loc()));
        case LIT_u8  : return cg.world.lit_idx (get<  u8>(), cg.loc2dbg(loc()));
        case LIT_u16 : return cg.world.lit_idx (get< u16>(), cg.loc2dbg(loc()));
        case LIT_u32 : return cg.world.lit_idx (get< u32>(), cg.loc2dbg(loc()));
        case LIT_u64 : return cg.world.lit_idx (get< u64>(), cg.loc2dbg(loc()));
        case LIT_f16 : return math::lit_f(cg.world, get<f16>(), cg.loc2dbg(loc()));
        case LIT_f32 : return math::lit_f(cg.world, get<f32>(), cg.loc2dbg(loc()));
        case LIT_f64 : return math::lit_f(cg.world, get<f64>(), cg.loc2dbg(loc()));
        // clang-format on
        default: thorin::unreachable();
    }
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world.lit_idx<u8>(value(), cg.loc2dbg(loc()));
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world.lit_idx<u8>(values_[i], cg.loc2dbg(loc()));

    return cg.world.tuple(args, cg.loc2dbg(loc()));
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = src()->remit(cg);
    auto src_type = src()->type();
    auto dst_type = type();
    auto dst = cg.convert(dst_type);
    auto dbg = cg.loc2dbg(loc());

    if (src_type->isa<PtrType>() || dst_type->isa<PtrType>()) {
        return core::op_bitcast(dst, def, dbg);
    } else if (is_int(src_type) || is_bool(src_type)) {
        if (is_signed(src_type)) {
            if (is_int(dst_type) || is_bool(dst_type)) {
                return op(core::conv::s2s, dst, def, dbg);
            } else {
                return op(math::conv::s2f, dst, def, dbg);
            }
        } else {
            if (is_int(dst_type) || is_bool(dst_type)) {
                return op(core::conv::u2u, dst, def, dbg);
            } else {
                return op(math::conv::u2f, dst, def, dbg);
            }
        }
    } else {
        if (is_int(dst_type) || is_bool(dst_type)) {
            if (is_signed(dst_type))
                return op(math::conv::f2s, dst, def, dbg);
            else
                return op(math::conv::f2u, dst, def, dbg);
        } else if (is_float(src_type) && is_float(dst_type)) {
            return op(math::conv::f2f, dst, def, dbg);
        } else {
            return core::op_bitcast(dst, def, dbg);
        }
    }
    thorin::unreachable();
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
    return is_bool(type) ? core::WMode::nuw : (is_signed(type) ? core::WMode::nsw : core::WMode::none);
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = rhs()->lemit(cg);
            auto val = cg.load(var, loc());
            auto one = cg.lit_one(type(), cg.loc2dbg(loc()));
            if (is_int(type()))
                val = core::op(tag() == INC ? core::wrap ::add : core::wrap ::sub, type2wmode(type()), val, one, cg.loc2dbg(loc()));
            else
                val = core::op(tag() == INC ? math::arith::add : math::arith::sub, math::Mode::none, val, one, cg.loc2dbg(loc()));
            cg.store(var, val, loc());
            return val;
        }
        case ADD: return rhs()->remit(cg);
        case SUB:
            if (is_int(type())) {
                auto mode = type2wmode(type());
                return core::op_wminus(mode, rhs()->remit(cg), cg.loc2dbg(loc()));
            } else {
                return math::op_rminus(core::RMode::none, rhs()->remit(cg), cg.loc2dbg(loc()));
            }
        case NOT:
            return core::op_negate(rhs()->remit(cg), cg.loc2dbg(loc()));
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
            if (def->dep_const()) {
                auto g = cg.world.global(mem::type_ptr(def->type()), /*mutable*/ false, cg.loc2dbg(loc()));
                g->set(def);
                return g;
            }

            auto slot = cg.slot(cg.convert(rhs()->type()), cg.loc2dbg(loc()));
            cg.store(slot, def, loc());
            return slot;
        }
        case MUT: {
            return rhs()->lemit(cg);
        }
        case RUNRUN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return core::op(core::pe::run, def, cg.loc2dbg(loc()));
        }
        case HLT: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return core::op(core::pe::hlt, def, cg.loc2dbg(loc()));
        }
        case KNOWN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return core::op(core::pe::known, def, cg.loc2dbg(loc()));
        }
        case OR:
        case OROR: thorin::unreachable();
        default:
            return cg.load(lemit(cg), loc());
    }
}

const Def* PrefixExpr::lemit(CodeGen& cg) const {
    assert(tag() == MUL);
    return rhs()->remit(cg);
}

void Expr::emit_branch(CodeGen& cg, Lam* jump_t, Lam* jump_f) const {
    auto cond = remit(cg);
    cg.cur_bb->branch(false, cond, jump_t, jump_f, cg.cur_mem, cg.loc2dbg(loc().finis()));
}

void InfixExpr::emit_branch(CodeGen& cg, Lam* jump_t, Lam* jump_f) const {
    auto jump_type = jump_t->type();
    switch (tag()) {
        case OROR: {
                auto or_f = cg.world.nom_lam(jump_type, cg.loc2dbg("or_f", loc().finis()));
                lhs()->emit_branch(cg, jump_t, or_f);
                cg.enter(or_f);
                rhs()->emit_branch(cg, jump_t, jump_f);
            }
            break;
        case ANDAND: {
                auto and_t = cg.world.nom_lam(jump_type, cg.loc2dbg("and_t", loc().finis()));
                lhs()->emit_branch(cg, and_t, jump_f);
                cg.enter(and_t);
                rhs()->emit_branch(cg, jump_t, jump_f);
            }
            break;
        default:
            return Expr::emit_branch(cg, jump_t, jump_f);
    }
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    // clang-format off
    switch (tag()) {
        case OROR:
        case ANDAND: {
            auto result    = cg.basicblock(cg.world.type_bool(), cg.loc2dbg("infix_result", loc().finis()));
            auto jump_type = cg.world.cn({ mem::type_mem(cg.world) });
            auto jump_t    = cg.world.nom_lam(jump_type, cg.loc2dbg("jump_t", loc().finis()));
            auto jump_f    = cg.world.nom_lam(jump_type, cg.loc2dbg("jump_f", loc().finis()));
            emit_branch(cg, jump_t, jump_f);
            jump_t->app(false, result, { jump_t->var(0_s), cg.world.lit_tt() });
            jump_f->app(false, result, { jump_f->var(0_s), cg.world.lit_ff() });
            return cg.enter(result)->var(1);
        }
        default: {
            auto op = tag();
            auto dbg = cg.loc2dbg(loc());

            if (Token::is_assign((TokenTag) op)) {
                auto lvar = lhs()->lemit(cg);
                auto rdef = rhs()->remit(cg);

                if (op == ASGN) {
                    cg.store(lvar, rdef, loc());
                    return cg.world.tuple();
                }

                auto ldef = cg.load(lhs()->lemit(cg), loc());

                if (is_float(rhs()->type())) {
                    switch (op) {
                        case ADD_ASGN: rdef = core::op(math::arith::add, core::RMode::none, ldef, rdef, dbg); break;
                        case SUB_ASGN: rdef = core::op(math::arith::sub, core::RMode::none, ldef, rdef, dbg); break;
                        case MUL_ASGN: rdef = core::op(math::arith::mul, core::RMode::none, ldef, rdef, dbg); break;
                        case DIV_ASGN: rdef = core::op(math::arith::div, core::RMode::none, ldef, rdef, dbg); break;
                        case REM_ASGN: rdef = core::op(math::arith::rem, core::RMode::none, ldef, rdef, dbg); break;
                        default: thorin::unreachable();
                    }
                } else if (is_bool(rhs()->type())) {
                    switch (op) {
                        case AND_ASGN: rdef = core::op(core::bit2::_and, ldef, rdef, dbg); break;
                        case  OR_ASGN: rdef = core::op(core::bit2:: _or, ldef, rdef, dbg); break;
                        case XOR_ASGN: rdef = core::op(core::bit2::_xor, ldef, rdef, dbg); break;
                        default: thorin::unreachable();
                    }
                } else {
                    auto mode = type2wmode(rhs()->type());
                    bool s = is_signed(rhs()->type());

                    switch (op) {
                        case AND_ASGN: rdef = core::op(core::bit2::_and, ldef, rdef, dbg); break;
                        case  OR_ASGN: rdef = core::op(core::bit2:: _or, ldef, rdef, dbg); break;
                        case XOR_ASGN: rdef = core::op(core::bit2::_xor, ldef, rdef, dbg); break;
                        case ADD_ASGN: rdef = core::op(core::wrap:: add, mode, ldef, rdef, dbg); break;
                        case SUB_ASGN: rdef = core::op(core::wrap:: sub, mode, ldef, rdef, dbg); break;
                        case MUL_ASGN: rdef = core::op(core::wrap:: mul, mode, ldef, rdef, dbg); break;
                        case SHL_ASGN: rdef = core::op(core::wrap:: shl, mode, ldef, rdef, dbg); break;
                        case SHR_ASGN: rdef = core::op(s ? core::shr::a : core::shr::l, ldef, rdef, dbg); break;
                        case DIV_ASGN: rdef = cg.handle_mem_res(core::op(s ? core::div::sdiv : core::div::udiv, cg.cur_mem, ldef, rdef, dbg)); break;
                        case REM_ASGN: rdef = cg.handle_mem_res(core::op(s ? core::div::srem : core::div::urem, cg.cur_mem, ldef, rdef, dbg)); break;
                        default: thorin::unreachable();
                    }
                }

                cg.store(lvar, rdef, loc());
                return cg.world.tuple();
            }

            auto ldef = lhs()->remit(cg);
            auto rdef = rhs()->remit(cg);

            if (is_float(rhs()->type())) {
                switch (op) {
                    case  EQ: return core::op(math::cmp::    e, math::Mode::none, ldef, rdef, dbg);
                    case  NE: return core::op(math::cmp::  une, math::Mode::none, ldef, rdef, dbg);
                    case  LT: return core::op(math::cmp::    l, math::Mode::none, ldef, rdef, dbg);
                    case  LE: return core::op(math::cmp::   le, math::Mode::none, ldef, rdef, dbg);
                    case  GT: return core::op(math::cmp::    g, math::Mode::none, ldef, rdef, dbg);
                    case  GE: return core::op(math::cmp::   ge, math::Mode::none, ldef, rdef, dbg);
                    case ADD: return core::op(math::arith::add, math::Mode::none, ldef, rdef, dbg);
                    case SUB: return core::op(math::arith::sub, math::Mode::none, ldef, rdef, dbg);
                    case MUL: return core::op(math::arith::mul, math::Mode::none, ldef, rdef, dbg);
                    case DIV: return core::op(math::arith::div, math::Mode::none, ldef, rdef, dbg);
                    case REM: return core::op(math::arith::rem, math::Mode::none, ldef, rdef, dbg);
                    default: thorin::unreachable();
                }
            } else if (is_bool(rhs()->type())) {
                switch (op) {
                    //
                    case  EQ: return core::op(core::icmp:: e, ldef, rdef, dbg);
                    case  NE: return core::op(core::icmp::ne, ldef, rdef, dbg);
                    case AND: return core::op(core::bit2::_and, ldef, rdef, dbg);
                    case  OR: return core::op(core::bit2:: _or, ldef, rdef, dbg);
                    case XOR: return core::op(core::bit2::_xor, ldef, rdef, dbg);
                    default: thorin::unreachable();
                }
            } else {
                auto mode = type2wmode(lhs()->type());
                bool s = is_signed(lhs()->type());

                if (thorin::match<mem::Ptr>(ldef->type())) ldef = core::op_bitcast(cg.world.type_int(64), ldef);
                if (thorin::match<mem::Ptr>(rdef->type())) rdef = core::op_bitcast(cg.world.type_int(64), rdef);

                switch (op) {
                    case  LT: return core::op(s ? core::icmp::  sl : core::icmp::  ul, ldef, rdef, dbg);
                    case  LE: return core::op(s ? core::icmp:: sle : core::icmp:: ule, ldef, rdef, dbg);
                    case  GT: return core::op(s ? core::icmp::  sg : core::icmp::  ug, ldef, rdef, dbg);
                    case  GE: return core::op(s ? core::icmp:: sge : core::icmp:: uge, ldef, rdef, dbg);
                    case SHR: return core::op(s ? core::shr ::a    : core::shr ::   l, ldef, rdef, dbg);
                    case  EQ: return core::op(core::icmp::   e, ldef, rdef, dbg);
                    case  NE: return core::op(core::icmp::  ne, ldef, rdef, dbg);
                    case  OR: return core::op(core::bit2:: _or, ldef, rdef, dbg);
                    case XOR: return core::op(core::bit2::_xor, ldef, rdef, dbg);
                    case AND: return core::op(core::bit2::_and, ldef, rdef, dbg);
                    case ADD: return core::op(core::wrap:: add, mode, ldef, rdef, dbg);
                    case SUB: return core::op(core::wrap:: sub, mode, ldef, rdef, dbg);
                    case MUL: return core::op(core::wrap:: mul, mode, ldef, rdef, dbg);
                    case SHL: return core::op(core::wrap:: shl, mode, ldef, rdef, dbg);
                    case DIV: return cg.handle_mem_res(core::op(s ? core::div::sdiv : core::div::udiv, cg.cur_mem, ldef, rdef, dbg));
                    case REM: return cg.handle_mem_res(core::op(s ? core::div::srem : core::div::urem, cg.cur_mem, ldef, rdef, dbg));
                    default: thorin::unreachable();
                }
            }
        }
    }
    // clang-format on
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    auto var = lhs()->lemit(cg);
    auto res = cg.load(var, loc());
    auto one = cg.lit_one(type(), cg.loc2dbg(loc()));
    const Def* val = nullptr;

    if (is_int(type()))
        val = core::op(tag() == INC ? core::wrap ::add : core::wrap ::sub, type2wmode(type()), res, one, cg.loc2dbg(loc()));
    else
        val = core::op(tag() == INC ? math::arith::add : math::arith::sub, math::Mode::none, res, one, cg.loc2dbg(loc()));
    cg.store(var, val, loc());
    return res;
}

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
    auto dim_int = op(core::conv::u2u, cg.world.type_int(64), dim()->remit(cg));
    auto arity = core::op_bitcast(cg.world.type_nat(), dim_int);
    auto elem = cg.convert(type()->as<IndefiniteArrayType>()->elem_type());
    return cg.world.pack(arity, cg.world.bot(elem), cg.loc2dbg(loc()));
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (auto&& elem : elems())
        defs[elem->field_decl()->index()] = elem->expr()->remit(cg);
    return cg.world.tuple(cg.convert(type())->as<thorin::Sigma>(), defs, cg.loc2dbg(loc()));
}

const Def* TypeAppExpr::lemit(CodeGen&) const { thorin::unreachable(); }
const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const { thorin::unreachable(); }

const Def* MapExpr::lemit(CodeGen& cg) const {
    auto agg = lhs()->lemit(cg);
    return mem::op_lea_unsafe(agg, arg(0)->remit(cg), cg.loc2dbg(loc()));
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
                            return core::op_bitcast(cg.convert(type_expr->type_arg(0)), arg(0)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "select") {
                            return cg.world.extract(cg.world.tuple({arg(2)->remit(cg), arg(1)->remit(cg)}), arg(0)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "insert") {
                            return core::insert_unsafe(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), cg.loc2dbg(loc()));
                        } else if (name == "alignof") {
                            return core::op_bitcast(cg.world.type_int(32), core::op(core::trait::align, cg.convert(type_expr->type_arg(0)), cg.loc2dbg(loc())));
                        } else if (name == "sizeof") {
                            return core::op_bitcast(cg.world.type_int(32), core::op(core::trait::size , cg.convert(type_expr->type_arg(0)), cg.loc2dbg(loc())));
                        } else if (name == "undef") {
                            return cg.world.bot(cg.convert(type_expr->type_arg(0)), cg.loc2dbg(loc()));
                        } else if (name == "reserve_shared") {
                            auto ptr = cg.convert(type());
                            auto cn = cg.world.cn({
                                mem::type_mem(cg.world), cg.world.type_int(32),
                                cg.world.cn({ mem::type_mem(cg.world), ptr }) });
                            auto cont = cg.world.nom_lam(cn, cg.loc2dbg("reserve_shared", loc()));
                            //cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr = cg.convert(arg(1)->type());
                            auto cn = cg.world.cn({
                                mem::type_mem(cg.world), cg.world.type_int(32), ptr, poly_type,
                                cg.world.cn({ mem::type_mem(cg.world), poly_type }) });
                            auto cont = cg.world.nom_lam(cn, cg.loc2dbg("atomic", loc()));
                            //cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr = thorin::match<mem::Ptr, true>(cg.convert(arg(0)->type()));
                            auto [pointee, addr_space] = ptr->args<2>();
                            auto poly_type = pointee;
                            auto cn = cg.world.cn({
                                mem::type_mem(cg.world), ptr, poly_type, poly_type,
                                cg.world.cn({ mem::type_mem(cg.world), poly_type, cg.world.type_bool() })
                            });
                            auto cont = cg.world.nom_lam(cn, cg.loc2dbg("cmpxchg", loc()));
                            //cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto string_type = mem::type_ptr(cg.world.arr_unsafe(cg.world.type_int(8)));
                            auto cn = cg.world.cn({
                                mem::type_mem(cg.world), string_type, poly_type,
                                cg.world.cn({ mem::type_mem(cg.world) }) });
                            auto cont = cg.world.nom_lam(cn, cg.loc2dbg("pe_info", loc()));
                            //cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_known") {
                            auto poly_type = cg.convert(arg(0)->type());
                            auto cn = cg.world.cn({
                                mem::type_mem(cg.world), poly_type,
                                cg.world.cn({ mem::type_mem(cg.world), cg.world.type_bool() }) });
                            auto cont = cg.world.nom_lam(cn, cg.loc2dbg("pe_known", loc()));
                            //cont->set_intrinsic();
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
        std::tie(cg.cur_bb, ret) = cg.call(dst, defs, ret_type, cg.loc2dbg((dst->debug().name + "_cont").c_str(), loc()));
        if (ret_type)
            cg.cur_mem = cg.cur_bb->var(0_s);

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>()) {
        auto index = arg(0)->remit(cg);
        return core::extract_unsafe(lhs()->remit(cg), index, cg.loc2dbg(loc()));
    }
    thorin::unreachable();
}

const Def* FieldExpr::lemit(CodeGen& cg) const {
    auto value = lhs()->lemit(cg);
    return mem::op_lea_unsafe(value, index(), cg.loc2dbg(loc()));
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    auto tup = lhs()->remit(cg);
    return cg.world.extract(tup, as_lit(tup->arity()), index(), cg.loc2dbg(loc()));
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

    auto jump_type = cg.world.cn({ mem::type_mem(cg.world) });
    auto if_then = cg.world.nom_lam(jump_type, cg.loc2dbg("if_then", then_expr()->loc().begin()));
    auto if_else = cg.world.nom_lam(jump_type, cg.loc2dbg("if_else", else_expr()->loc().begin()));
    auto if_join = thorin_type ? cg.basicblock(thorin_type, cg.loc2dbg("if_join", loc().finis())) : nullptr; // TODO rewrite with bottom type

    cond()->emit_branch(cg, if_then, if_else);

    cg.enter(if_then);
    if (auto tdef = then_expr()->remit(cg))
        cg.cur_bb->app(false, if_join, {cg.cur_mem, tdef}, cg.loc2dbg(loc().finis()));

    cg.enter(if_else);
    if (auto fdef = else_expr()->remit(cg))
        cg.cur_bb->app(false, if_join, {cg.cur_mem, fdef}, cg.loc2dbg(loc().finis()));

    if (thorin_type)
        return cg.enter(if_join)->var(1);
    return nullptr; // TODO use bottom type
}

const Def* MatchExpr::remit(CodeGen& /*cg*/) const {
#if 0
    auto thorin_type = cg.convert(type());

    auto join = thorin_type ? cg.basicblock(thorin_type, cg.loc2dbg("match_join", loc().finis())) : nullptr; // TODO rewrite with bottom type

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
                    defs[i] = cg.world.lit_idx(64, option_decl->index(), cg.loc2dbg(arm(i)->ptrn()->loc()));
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
                cg.cur_bb->app(false, join, {cg.cur_mem, def}, cg.loc2dbg(loc().finis()));
        }

        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise) {
            cg.enter(otherwise);
            if (auto def = arm(num_targets)->expr()->remit(cg))
                cg.cur_bb->app(false, join, {cg.cur_mem, def}, cg.loc2dbg(loc().finis()));
        }
    } else {
        // general case: if/else
        for (size_t i = 0, e = num_arms(); i != e; ++i) {
            auto case_t = cg.basicblock(cg.loc2dbg("case_t", arm(i)->loc().front()));
            auto case_f = cg.basicblock(cg.loc2dbg("case_f", arm(i)->loc().front()));

            arm(i)->ptrn()->emit(cg, matcher);

            // last pattern will always be taken
            auto cond = i == e - 1
                ? cg.world.lit_tt()
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.cur_bb->branch(false, cond, case_t, case_f, cg.cur_mem, cg.loc2dbg(arm(i)->ptrn()->loc().finis()));

            cg.enter(case_t);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(false, join, {cg.cur_mem, def}, cg.loc2dbg(arm(i)->loc().finis()));

            cg.enter(case_f);
        }
    }

    if (thorin_type)
        return cg.enter(join)->var(1);
#endif
    return nullptr; // TODO use bottom type
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    auto head_bb = cg.world.nom_lam(cg.world.cn({mem::type_mem(cg.world)}), cg.loc2dbg("while_head", loc().begin()));
    head_bb->var(0, cg.world.dbg("mem"));

    auto jump_type = cg.world.cn({mem::type_mem(cg.world)});
    auto body_bb = cg.world.nom_lam(jump_type, cg.loc2dbg("while_body", body()->loc().begin()));
    auto exit_bb = cg.world.nom_lam(jump_type, cg.loc2dbg("while_exit", body()->loc().finis()));
    auto cont_bb = cg.create_lam(continue_decl());
    auto brk__bb = cg.create_lam(break_decl());

    cg.cur_bb->app(false, head_bb, {cg.cur_mem}, cg.loc2dbg(cond()->loc().finis()));

    cg.enter(head_bb);
    cond()->emit_branch(cg, body_bb, exit_bb);

    cg.enter(body_bb);
    body()->remit(cg);
    cg.cur_bb->app(false, cont_bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().finis()));

    cg.enter(cont_bb);
    cg.cur_bb->app(false, head_bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().finis()));

    cg.enter(exit_bb);
    cg.cur_bb->app(false, brk__bb, {cg.cur_mem}, cg.loc2dbg(body()->loc().finis()));

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
    if (break_bb->num_vars() == 2)
        return break_bb->var(1);
    else {
        Array<const Def*> args(break_bb->num_vars()-1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            args[i] = break_bb->var(i+1);
        return cg.world.tuple(args, cg.loc2dbg(loc()));
    }
}

const Def* FnExpr::remit(CodeGen& cg) const {
    auto lam = fn_emit_head(cg, loc());
    fn_emit_body(cg, loc());
    return lam;
}

const Def* RevDiffExpr::remit(CodeGen& cg) const {
    return nullptr;
    //return cg.rev_diff(expr()->remit(cg));
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    local()->emit(cg, init);
}

const thorin::Def* IdPtrn::emit_cond(CodeGen& cg, const thorin::Def*) const { return cg.world.lit_tt(); }

void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant = core::op_bitcast(variant_type, cg.world.extract(init, num_args(), 1), cg.loc2dbg(loc()));
    for (size_t i = 0, e = num_args(); i != e; ++i) {
        arg(i)->emit(cg, num_args() == 1 ? variant : cg.world.extract(variant, num_args(), i, cg.loc2dbg(loc())));
    }
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto init_0 = cg.world.extract(init, num_args(), 0_u32, cg.loc2dbg(loc()));
    auto cond = core::op(core::icmp::e, init_0, cg.world.lit_idx(u32(index), cg.loc2dbg(loc())));
    if (num_args() > 0) {
        auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
        auto variant = core::op_bitcast(variant_type, cg.world.extract(init, num_args(), 1, cg.loc2dbg(loc())), cg.loc2dbg(loc()));
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? variant : cg.world.extract(variant, num_args(), i, cg.loc2dbg(loc())));
            cond = core::op(core::bit2::_and, cond, arg_cond, cg.loc2dbg(loc()));
        }
    }
    return cond;
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        elem(i)->emit(cg, cg.world.extract(init, e, i, cg.loc2dbg(loc())));
}

const thorin::Def* TuplePtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    const Def* cond = nullptr;
    for (size_t i = 0, e = num_elems(); i != e; ++i) {
        if (!elem(i)->is_refutable()) continue;

        auto next = elem(i)->emit_cond(cg, cg.world.extract(init, num_elems(), i, cg.loc2dbg(loc())));
        cond = cond ? core::op(core::bit2::_and, cond, next) : next;
    }
    return cond ? cond : cg.world.lit_tt();
}

const thorin::Def* LiteralPtrn::emit(CodeGen& cg) const {
    auto def = literal()->remit(cg);
    if (has_minus()) {
        if (is_float(type()))
            return math::op_rminus(def, def->dbg());
        else
            return core::op_wminus(type2wmode(type()), def, def->dbg());
    } else {
        return def;
    }
}

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return core::op(core::icmp::e, init, emit(cg));
}

const thorin::Def* CharPtrn::emit(CodeGen& cg) const {
    return chr()->remit(cg);
}

void CharPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* CharPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return core::op(core::icmp::e, init, emit(cg));
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
