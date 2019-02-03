#include "impala/ast.h"

#include "thorin/primop.h"
#include "thorin/world.h"
#include "thorin/util/array.h"

using namespace thorin;

namespace impala {

class CodeGen {
public:
    CodeGen(World& world)
        : world(world)
    {}

    /// Lam of type cn()
    Lam* basicblock(Debug dbg) { return world.lam(world.cn(), CC::C, Intrinsic::None, dbg); }

    /// Lam of type cn(mem, type) - a point in the program where control flow *j*oins
    Lam* basicblock(const thorin::Def* type, Debug dbg) {
        auto bb = world.lam(world.cn({world.mem_type(), type}), CC::C, Intrinsic::None, dbg);
        bb->param(0)->debug().set("mem");
        return bb;
    }

    Lam* enter(Lam* bb, const Def* mem) {
        cur_bb = bb;
        cur_mem = mem;
        return bb;
    }

    const Def* enter(Lam* bb) {
        enter(bb, bb->param(0));
        return bb->param(1);
    }

    const Def* frame() const { assert(cur_fn); return cur_fn->frame(); }

    std::pair<Lam*, const Def*> call(const Def* callee, Defs args, const thorin::Def* ret_type, Debug dbg) {
        if (ret_type == nullptr) {
            cur_bb->app(callee, args, dbg);
            auto next = basicblock(dbg + "_unreachable");
            return std::make_pair(next, nullptr);
        }

        std::vector<const thorin::Def*> cont_args;
        cont_args.push_back(world.mem_type());

        // if the return type is a sigma, flatten it
        auto sigma = ret_type->isa<thorin::Sigma>();
        if (sigma && !sigma->is_nominal()) {
            for (auto op : sigma->ops())
                cont_args.push_back(op);
        } else
            cont_args.push_back(ret_type);

        // next is the return lam
        auto next = world.lam(world.cn(cont_args), dbg);
        next->param(0)->debug().set("mem");

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
            ret = world.tuple(ret_type, params);
        } else
            ret = next->param(1);
        ret->debug().set(callee->name());

        return std::make_pair(next, ret);
    }

    Lam* create_lam(const LocalDecl* decl) {
        auto result = world.lam(convert(decl->type())->as<thorin::Pi>(), decl->debug());
        result->param(0)->debug().set("mem");
        decl->def_ = result;
        return result;
    }

    const Def* load(const Def* ptr, Loc loc) {
        auto l = world.load(cur_mem, ptr, loc);
        cur_mem = world.extract(l, 0_u32, loc);
        return world.extract(l, 1_u32, loc);
    }

    void store(const Def* ptr, const Def* val, Loc loc) {
        cur_mem = world.store(cur_mem, ptr, val, loc);
    }

    const Def* alloc(const thorin::Def* type, const Def* extra, Debug dbg) {
        if (!extra)
            extra = world.lit_qu64(0, dbg);
        auto alloc = world.alloc(type, cur_mem, extra, dbg);
        cur_mem = world.extract(alloc, 0_u32, dbg);
        return world.extract(alloc, 1, dbg);
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
    } else if (auto var = type->isa<Var>()) {
        return world.var(world.kind_star(), var->depth());
    } else if (auto prim_type = type->isa<PrimType>()) {
        switch (prim_type->primtype_tag()) {
#define IMPALA_TYPE(itype, ttype) \
            case PrimType_##itype: return world.type_##ttype();
#include "impala/tokenlist.h"
            default: THORIN_UNREACHABLE;
        }
    } else if (auto cn = type->isa<FnType>()) {
        std::vector<const thorin::Def*> nops;
        nops.push_back(world.mem_type());
        for (size_t i = 0, e = cn->num_params(); i != e; ++i)
            nops.push_back(convert(cn->param(i)));
        return world.cn(nops);
    } else if (auto tuple_type = type->isa<TupleType>()) {
        std::vector<const thorin::Def*> nops;
        for (auto&& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world.sigma(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world.sigma(struct_type->num_ops(), struct_type->struct_decl()->symbol());
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (auto&& op : struct_type->ops())
            s->set(i++, convert(op));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return s;
    } else if (auto enum_type = type->isa<EnumType>()) {
        auto s = world.sigma(2, enum_type->enum_decl()->symbol());
        thorin_enum_type(enum_type) = s;
        thorin_type(enum_type) = s;

        auto enum_decl = enum_type->enum_decl();
        thorin::DefSet variants;
        for (auto&& option : enum_decl->option_decls())
            variants.insert(option->variant_type(*this));
        thorin::Array<const thorin::Def*> ops(variants.size());
        std::copy(variants.begin(), variants.end(), ops.begin());

        s->set(0, world.type_qu32());
        s->set(1, world.variant_type(ops));
        thorin_type(enum_type) = nullptr;
        return s;
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world.ptr_type(convert(ptr_type->pointee()), thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world.definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world.indefinite_array_type(convert(indefinite_array_type->elem_type()));
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
        def_ = cg.world.slot(thorin_type, cg.frame(), debug());
        cg.cur_mem = cg.world.store(cg.cur_mem, def_, init, debug());
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
    return lam_ = cg.world.lam(t, {loc, fn_symbol().remove_quotation()});
}

void Fn::fn_emit_body(CodeGen& cg, Loc loc) const {
    // setup function nest
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, lam());
    auto old_mem = cg.cur_mem;

    // setup memory + frame
    {
        size_t i = 0;
        auto mem_param = lam()->param(i++);
        mem_param->debug().set("mem");
        auto enter = cg.world.enter(mem_param, loc);
        cg.cur_mem = cg.world.extract(enter, 0_u32, loc);
        frame_ =     cg.world.extract(enter, 1_u32, loc);

        // name params and setup store locs
        for (auto&& param : params()) {
            auto p = lam()->param(i++);
            p->debug().set(param->symbol());
            param->emit(cg, p);
        }

        //assert(i == lam()->num_params() || lam()->type() == cg.cn);

        if (lam()->num_params() != 0
                && lam()->params().back()->type()->isa<Pi>())
            ret_param_ = lam()->params().back();
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
            cg.cur_bb->app(ret_param(), ret_values, loc.back());
        } else
            cg.cur_bb->app(ret_param(), {cg.cur_mem, def}, loc.back());
    }

    // now handle the filter
    {
        size_t i = 0;
        auto global = pe_expr() ? pe_expr()->remit(cg) : cg.world.lit_bool(false, loc);
        Array<const Def*> filter(lam()->num_params());
        filter[i++] = global; // mem param

        for (auto&& param : params()) {
            auto pe_expr = param->pe_expr();
            filter[i++] = pe_expr
                          ? cg.world.arithop_or(global, pe_expr->remit(cg), pe_expr->loc())
                          : global;
        }

        // HACK for unit
        if (auto tuple_type = lam()->type()->ops().back()->isa<thorin::Sigma>()) {
            if (tuple_type->num_ops() == 0)
                filter[i++] = global;
        }

        lam()->set_filter(cg.world.tuple(filter));
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
            lam->cc() = thorin::CC::C;
        else if (abi() == "\"device\"")
            lam->cc() = thorin::CC::Device;
        else if (abi() == "\"thorin\"" && lam) // no lam for primops
            lam->set_intrinsic();
    }
}

void ModuleDecl::emit(CodeGen&) const {}
void ImplItem::emit(CodeGen&) const {}

void StaticItem::emit_head(CodeGen& cg) const {
    def_ = cg.world.global(cg.world.bot(cg.convert(type()), loc()));
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
    auto id = cg.world.lit_qu32(index(), loc());
    if (num_args() == 0) {
        auto bot = cg.world.bot(variant_type);
        def_ = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, bot });
    } else {
        auto lam = cg.world.lam(cg.convert(type())->as<thorin::Pi>(), {loc(), symbol()});
        auto ret = lam->param(lam->num_params() - 1);
        auto mem = lam->param(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = lam->num_params(); i + 1 < e; i++)
            defs[i-1] = lam->param(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, cg.world.variant(variant_type, option_val) });
        lam->app(ret, { mem, enum_val }, loc());
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
const Def* EmptyExpr::remit(CodeGen& cg) const { return cg.world.tuple({}, loc()); }

const Def* LiteralExpr::remit(CodeGen& cg) const {
    thorin::PrimTypeTag ttag;

    switch (tag()) {
#define IMPALA_LIT(itype, ttype) \
        case LIT_##itype: ttag = thorin::PrimType_##ttype; break;
#include "impala/tokenlist.h"
        case LIT_bool: ttag = thorin::PrimType_bool; break;
        default: THORIN_UNREACHABLE;
    }

    return cg.world.lit(ttag, box(), loc());
}

const Def* CharExpr::remit(CodeGen& cg) const {
    return cg.world.lit_pu8(value(), loc());
}

const Def* StrExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(values_.size());
    for (size_t i = 0, e = args.size(); i != e; ++i)
        args[i] = cg.world.lit_pu8(values_[i], loc());

    return cg.world.definite_array(args, loc());
}

const Def* CastExpr::remit(CodeGen& cg) const {
    auto def = src()->remit(cg);
    auto thorin_type = cg.convert(type());
    return cg.world.convert(thorin_type, def, loc());
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

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = rhs()->lemit(cg);
            auto val = cg.load(var, loc());
            auto one = cg.world.one(val->type(), loc());
            val = cg.world.arithop(Token::to_arithop((TokenTag) tag()), val, one, loc());
            cg.store(var, val, loc());
            return val;
        }
        case ADD: return rhs()->remit(cg);
        case SUB: return cg.world.arithop_minus(rhs()->remit(cg), loc());
        case NOT: return cg.world.arithop_not(rhs()->remit(cg), loc());
        case TILDE: {
            auto def = rhs()->remit(cg);
            auto ptr = cg.alloc(def->type(), rhs()->extra(), loc());
            cg.store(ptr, def, loc());
            return ptr;
        }
        case AND: {
            if (rhs()->type()->isa<RefType>())
                return rhs()->lemit(cg);

            auto def = rhs()->remit(cg);
            if (is_const(def))
                return cg.world.global(def, /*mutable*/ false, loc());

            auto slot = cg.world.slot(cg.convert(rhs()->type()), cg.frame(), loc());
            cg.store(slot, def, loc());
            return slot;
        }
        case MUT: {
            return rhs()->lemit(cg);
        }
        case RUNRUN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.run(def, loc());
        }
        case HLT: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.hlt(def, loc());
        }
        case KNOWN: {
            auto def = rhs()->skip_rvalue()->remit(cg);
            return cg.world.known(def, loc());
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

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case ANDAND: {
            auto t = cg.basicblock({lhs()->loc().front(), "and_lhs_t"});
            auto f = cg.basicblock({rhs()->loc().front(), "and_lhs_f"});
            auto r = cg.basicblock(cg.world.type_bool(), {loc().back(), "and_result"});

            auto lcond = lhs()->remit(cg);
            auto mem = cg.cur_mem;
            cg.cur_bb->branch(lcond, t, f);

            cg.enter(t, mem);
            auto rcond = rhs()->remit(cg);
            cg.cur_bb->app(r, {cg.cur_mem, rcond});

            cg.enter(f, mem)->app(r, {cg.cur_mem, cg.world.lit(false)});
            return cg.enter(r);
        }
        case OROR: {
            auto t = cg.basicblock({lhs()->loc().front(), "or_lhs_t"});
            auto f = cg.basicblock({rhs()->loc().front(), "or_lhs_f"});
            auto r = cg.basicblock(cg.world.type_bool(), {loc().back(), "or_result"});

            auto lcond = lhs()->remit(cg);
            auto mem = cg.cur_mem;
            cg.cur_bb->branch(lcond, t, f);

            cg.enter(f, mem);
            auto rcond = rhs()->remit(cg);
            cg.cur_bb->app(r, {cg.cur_mem, rcond});

            cg.enter(t, mem)->app(r, {cg.cur_mem, cg.world.lit(true)});
            return cg.enter(r);
        }
        default:
            const TokenTag op = (TokenTag) tag();

            if (Token::is_assign(op)) {
                auto lvar = lhs()->lemit(cg);
                auto rdef = rhs()->remit(cg);

                if (op != Token::ASGN) {
                    auto sop = Token::separate_assign(op);
                    rdef = cg.world.binop(Token::to_binop(sop), cg.load(lvar, loc()), rdef, loc());
                }

                cg.store(lvar, rdef, loc());
                return cg.world.tuple({}, loc());
            }

            auto ldef = lhs()->remit(cg);
            auto rdef = rhs()->remit(cg);
            return cg.world.binop(Token::to_binop(op), ldef, rdef, loc());
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    auto var = lhs()->lemit(cg);
    auto def = cg.load(var, loc());
    auto one = cg.world.one(def->type(), loc());
    cg.store(var, cg.world.arithop(Token::to_arithop((TokenTag) tag()), def, one, loc()), loc());
    return def;
}

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, loc());
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(count());
    std::fill_n(args.begin(), count(), value()->remit(cg));
    return cg.world.definite_array(args, loc());
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = arg(i)->remit(cg);
    return cg.world.tuple(thorin_args, loc());
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = dim()->remit(cg);
    return cg.world.indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, loc());
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (auto&& elem : elems())
        defs[elem->field_decl()->index()] = elem->expr()->remit(cg);
    return cg.world.tuple(cg.convert(type())->as<thorin::Sigma>(), defs, loc());
}

const Def* TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }
const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const { THORIN_UNREACHABLE; }

const Def* MapExpr::lemit(CodeGen& cg) const {
    auto agg = lhs()->lemit(cg);
    return cg.world.lea(agg, arg(0)->remit(cg), loc());
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
                            return cg.world.bitcast(cg.convert(type_expr->type_arg(0)), arg(0)->remit(cg), loc());
                        } else if (name == "select") {
                            return cg.world.select(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), loc());
                        } else if (name == "insert") {
                            return cg.world.insert(arg(0)->remit(cg), arg(1)->remit(cg), arg(2)->remit(cg), loc());
                        } else if (name == "sizeof") {
                            return cg.world.size_of(cg.convert(type_expr->type_arg(0)), loc());
                        } else if (name == "undef") {
                            return cg.world.bot(cg.convert(type_expr->type_arg(0)), loc());
                        } else if (name == "reserve_shared") {
                            auto ptr_type = cg.convert(type());
                            auto cn = cg.world.cn({
                                cg.world.mem_type(), cg.world.type_qs32(),
                                cg.world.cn({ cg.world.mem_type(), ptr_type }) });
                            auto cont = cg.world.lam(cn, {loc(), "reserve_shared"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "atomic") {
                            auto poly_type = cg.convert(type());
                            auto ptr_type = cg.convert(arg(1)->type());
                            auto cn = cg.world.cn({
                                cg.world.mem_type(), cg.world.type_pu32(), ptr_type, poly_type,
                                cg.world.cn({ cg.world.mem_type(), poly_type }) });
                            auto cont = cg.world.lam(cn, {loc(), "atomic"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto cn = cg.world.cn({
                                cg.world.mem_type(), ptr_type, poly_type, poly_type,
                                cg.world.cn({ cg.world.mem_type(), poly_type, cg.world.type_bool() })
                            });
                            auto cont = cg.world.lam(cn, {loc(), "cmpxchg"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto string_type = cg.world.ptr_type(cg.world.indefinite_array_type(cg.world.type_pu8()));
                            auto cn = cg.world.cn({
                                cg.world.mem_type(), string_type, poly_type,
                                cg.world.cn({ cg.world.mem_type() }) });
                            auto cont = cg.world.lam(cn, {loc(), "pe_info"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_known") {
                            auto poly_type = cg.convert(arg(0)->type());
                            auto cn = cg.world.cn({
                                cg.world.mem_type(), poly_type,
                                cg.world.cn({ cg.world.mem_type(), cg.world.type_bool() }) });
                            auto cont = cg.world.lam(cn, {loc(), "pe_known"});
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
        std::tie(cg.cur_bb, ret) = cg.call(dst, defs, ret_type, thorin::Debug(loc(), dst->name()) + "_cont");
        if (ret_type)
            cg.cur_mem = cg.cur_bb->param(0);

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>()) {
        auto index = arg(0)->remit(cg);
        return cg.world.extract(lhs()->remit(cg), index, loc());
    }
    THORIN_UNREACHABLE;
}

const Def* FieldExpr::lemit(CodeGen& cg) const {
    auto value = lhs()->lemit(cg);
    return cg.world.lea(value, cg.world.lit_qu32(index(), loc()), loc());
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.world.extract(lhs()->remit(cg), index(), loc());
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

    auto if_then = cg.basicblock({then_expr()->loc().front(), "if_then"});
    auto if_else = cg.basicblock({else_expr()->loc().front(), "if_else"});
    auto if_join = thorin_type ? cg.basicblock(thorin_type, {loc().back(), "if_join"}) : nullptr; // TODO rewrite with bottom type

    auto c = cond()->remit(cg);
    cg.cur_bb->branch(c, if_then, if_else, cond()->loc().back());
    auto head_mem = cg.cur_mem;

    cg.enter(if_then, head_mem);
    if (auto tdef = then_expr()->remit(cg))
        cg.cur_bb->app(if_join, {cg.cur_mem, tdef}, loc().back());

    cg.enter(if_else, head_mem);
    if (auto fdef = else_expr()->remit(cg))
        cg.cur_bb->app(if_join, {cg.cur_mem, fdef}, loc().back());

    if (thorin_type)
        return cg.enter(if_join);
    return nullptr; // TODO use bottom type
}

const Def* MatchExpr::remit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());

    auto join = thorin_type ? cg.basicblock(thorin_type, {loc().back(), "match_join"}) : nullptr; // TODO rewrite with bottom type

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
                otherwise = cg.basicblock({arm(i)->loc().front(), "otherwise"});
                break;
            } else {
                if (is_integer) {
                    defs[i] = arm(i)->ptrn()->as<LiteralPtrn>()->emit_literal(cg);
                } else {
                    auto enum_ptrn = arm(i)->ptrn()->as<EnumPtrn>();
                    auto option_decl = enum_ptrn->path()->decl()->as<OptionDecl>();
                    defs[i] = cg.world.lit_qu32(option_decl->index(), arm(i)->ptrn()->loc());
                }
                targets[i] = cg.basicblock({arm(i)->loc().front(), "case"});
            }
        }

        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world.extract(matcher, 0_u32, matcher->debug());
        cg.cur_bb->match(matcher_int, otherwise, defs, targets, {loc().front(), "match"});
        auto mem = cg.cur_mem;

        for (size_t i = 0; i != num_targets; ++i) {
            cg.enter(targets[i], mem);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, loc().back());
        }

        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise) {
            cg.enter(otherwise, mem);
            if (auto def = arm(num_targets)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, loc().back());
        }
    } else {
        // general case: if/else
        for (size_t i = 0, e = num_arms(); i != e; ++i) {
            auto case_true  = cg.basicblock({arm(i)->loc().front(), "case_true"});
            auto case_false = cg.basicblock({arm(i)->loc().front(), "case_false"});

            arm(i)->ptrn()->emit(cg, matcher);

            // last pattern will always be taken
            auto cond = i == e - 1
                ? cg.world.lit_bool(true, arm(i)->ptrn()->loc())
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.cur_bb->branch(cond, case_true, case_false, arm(i)->ptrn()->loc().back());

            auto mem = cg.cur_mem;
            cg.enter(case_true, mem);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(join, {cg.cur_mem, def}, arm(i)->loc().back());

            cg.enter(case_false, mem);
        }
    }

    if (thorin_type)
        return cg.enter(join);
    return nullptr; // TODO use bottom type
}

const Def* WhileExpr::remit(CodeGen& cg) const {
    auto head_bb = cg.world.lam(cg.world.cn({cg.world.mem_type()}), CC::C, Intrinsic::None, {loc().front(), "while_head"});
    head_bb->param(0)->debug().set("mem");
    auto body_bb = cg.basicblock({body()->loc().front(), "while_body"});
    auto exit_bb = cg.basicblock({body()->loc().back(),  "while_exit"});
    auto cont_bb = cg.create_lam(continue_decl());
    auto brk__bb = cg.create_lam(break_decl());

    cg.cur_bb->app(head_bb, {cg.cur_mem}, cond()->loc().back());

    cg.enter(head_bb, head_bb->param(0));
    auto c = cond()->remit(cg);
    cg.cur_bb->branch(c, body_bb, exit_bb);
    auto head_mem = cg.cur_mem;

    cg.enter(body_bb, cg.cur_mem);
    body()->remit(cg);
    cg.cur_bb->app(cont_bb, {cg.cur_mem}, body()->loc().back());

    cg.enter(cont_bb, cont_bb->param(0));
    cg.cur_bb->app(head_bb, {cg.cur_mem}, body()->loc().back());

    cg.enter(exit_bb, head_mem);
    cg.cur_bb->app(brk__bb, {cg.cur_mem}, body()->loc().back());

    cg.enter(brk__bb, brk__bb->param(0));
    return cg.world.tuple({}, loc());
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
    cg.call(fun, args, nullptr, map_expr->loc());

    cg.enter(break_bb, break_bb->param(0));
    if (break_bb->num_params() == 2)
        return break_bb->param(1);
    else {
        Array<const Def*> args(break_bb->num_params()-1);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            args[i] = break_bb->param(i+1);
        return cg.world.tuple(args, loc());
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
    init->debug().set(local()->symbol());
    local()->emit(cg, init);
}

const thorin::Def* IdPtrn::emit_cond(CodeGen& cg, const thorin::Def*) const {
    return cg.world.lit(true);
}

void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1), loc());
    for (size_t i = 0, e = num_args(); i != e; ++i) {
        arg(i)->emit(cg, num_args() == 1 ? variant : cg.world.extract(variant, i, loc()));
    }
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto cond = cg.world.cmp_eq(cg.world.extract(init, 0_u32, loc()), cg.world.lit_qu32(index, loc()));
    if (num_args() > 0) {
        auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
        auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1, loc()), loc());
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? variant : cg.world.extract(variant, i, loc()));
            cond = cg.world.arithop_and(cond, arg_cond, loc());
        }
    }
    return cond;
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        elem(i)->emit(cg, cg.world.extract(init, i, loc()));
}

const thorin::Def* TuplePtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    const Def* cond = nullptr;
    for (size_t i = 0, e = num_elems(); i != e; ++i) {
        if (!elem(i)->is_refutable()) continue;

        auto next = elem(i)->emit_cond(cg, cg.world.extract(init, i, loc()));
        cond = cond ? cg.world.arithop_and(cond, next) : next;
    }
    return cond ? cond : cg.world.lit(true);
}

const thorin::Def* LiteralPtrn::emit_literal(CodeGen& cg) const {
    auto def = literal()->remit(cg);
    return has_minus() ? cg.world.arithop_minus(def, def->debug()) : def;
}

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.cmp_eq(init, emit_literal(cg));
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const { expr()->remit(cg); }
void ItemStmt::emit(CodeGen& cg) const { item()->emit(cg); }

void LetStmt::emit(CodeGen& cg) const {
    ptrn()->emit(cg, init() ? init()->remit(cg) : cg.world.bot(cg.convert(ptrn()->type()), ptrn()->loc()));
}

void AsmStmt::emit(CodeGen& cg) const {
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
            output_constraints(), input_constraints(), clobbers(), flags, loc());

    size_t i = 0;
    cg.cur_mem = assembly->out(i++);
    for (auto&& output: outputs())
        cg.store(output->expr()->lemit(cg), assembly->out(i++), loc());
}

//------------------------------------------------------------------------------

void emit(World& world, const Module* mod) {
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
