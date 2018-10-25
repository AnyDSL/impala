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
    Continuation* basicblock(Debug dbg) { return world.continuation(world.fn_type(), CC::C, Intrinsic::None, dbg); }

    /// Continuation of type cn(mem, type) - a point in the program where control flow *j*oins
    Continuation* basicblock(const thorin::Type* type, Debug dbg) {
        auto bb = world.continuation(world.fn_type({world.mem_type(), type}), CC::C, Intrinsic::None, dbg);
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

    const Def* lemit(const Expr* expr) { return expr->lemit(*this); }
    const Def* remit(const Expr* expr) { return expr->remit(*this); }
    void emit(const Stmt* stmt) { stmt->emit(*this); }
    void emit(const Item* item) {
        assert(!item->done_);
        item->emit(*this);
#ifndef NDEBUG
        item->done_ = true;
#endif
    }
    void emit(const Ptrn* ptrn, const thorin::Def* def) { ptrn->emit(*this, def); }
    const Def* emit(const Decl* decl) {
        return decl->def_;
    }
    const Def* emit(const Decl* decl, const Def* init) {
        if (!decl->def_)
            decl->def_ = decl->emit(*this, init);
        return decl->def_;
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

    World& world;
    const Fn* cur_fn = nullptr;
    TypeMap<const thorin::Type*> impala2thorin_;
    GIDMap<const StructType*, const thorin::StructType*> struct_type_impala2thorin_;
    GIDMap<const EnumType*,   const thorin::StructType*> enum_type_impala2thorin_;
    Continuation* cur_bb = nullptr;
    const Def* cur_mem = nullptr;
};

/*
 * Type
 */

const thorin::Type* CodeGen::convert_rec(const Type* type) {
    if (auto lambda = type->isa<Lambda>()) {
        return world.lambda(convert(lambda->body()), lambda->name());
    } else if (auto var = type->isa<Var>()) {
        return world.var(var->depth());
    } else if (auto prim_type = type->isa<PrimType>()) {
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
        for (const auto& op : tuple_type->ops())
            nops.push_back(convert(op));
        return world.tuple_type(nops);
    } else if (auto struct_type = type->isa<StructType>()) {
        auto s = world.struct_type(struct_type->struct_decl()->symbol(), struct_type->num_ops());
        thorin_struct_type(struct_type) = s;
        thorin_type(type) = s;
        size_t i = 0;
        for (const auto& op : struct_type->ops())
            s->set(i++, convert(op));
        thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
        return s;
    } else if (auto enum_type = type->isa<EnumType>()) {
        auto s = world.struct_type(enum_type->enum_decl()->symbol(), 2);
        thorin_enum_type(enum_type) = s;
        thorin_type(enum_type) = s;

        auto enum_decl = enum_type->enum_decl();
        thorin::TypeSet variants;
        for (const auto& option : enum_decl->option_decls())
            variants.insert(option->variant_type(*this));
        thorin::Array<const thorin::Type*> ops(variants.size());
        std::copy(variants.begin(), variants.end(), ops.begin());

        s->set(0, world.type_qu32());
        s->set(1, world.variant_type(ops));
        thorin_type(enum_type) = nullptr;
        return s;
    } else if (auto ptr_type = type->isa<PtrType>()) {
        return world.ptr_type(convert(ptr_type->pointee()), 1, -1, thorin::AddrSpace(ptr_type->addr_space()));
    } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
        return world.definite_array_type(convert(definite_array_type->elem_type()), definite_array_type->dim());
    } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
        return world.indefinite_array_type(convert(indefinite_array_type->elem_type()));
    } else if (auto simd_type = type->isa<SimdType>()) {
        return world.type(convert(simd_type->elem_type())->as<thorin::PrimType>()->primtype_tag(), simd_type->dim());
    }
    THORIN_UNREACHABLE;
}

/*
 * Decls and Function
 */

const Def* LocalDecl::emit(CodeGen& cg, const Def* init) const {
    assert(def_ == nullptr);

    auto thorin_type = cg.convert(type());
    init = init ? init : cg.world.bottom(thorin_type);

    if (is_mut()) {
        def_ = cg.world.slot(thorin_type, cg.frame(), debug());
        cg.cur_mem = cg.world.store(cg.cur_mem, def_, init, debug());
    } else {
        def_ = init;
    }

    return def_;
}

const thorin::Type* OptionDecl::variant_type(CodeGen& cg) const {
    std::vector<const thorin::Type*> types;
    for (const auto& arg : args())
        types.push_back(cg.convert(arg->type()));
    if (num_args() == 1) return types.back();
    return cg.world.tuple_type(types);
}

Continuation* Fn::emit_head(CodeGen& cg, Location location) const {
    auto t = cg.convert(fn_type())->as<thorin::FnType>();
    return continuation_ = cg.world.continuation(t, {location, fn_symbol().remove_quotation()});
}

void Fn::emit_body(CodeGen& cg, Location location) const {
    // setup function nest
    THORIN_PUSH(cg.cur_fn, this);
    THORIN_PUSH(cg.cur_bb, continuation());

    // setup memory + frame
    {
        size_t i = 0;
        auto mem_param = continuation()->param(i++);
        mem_param->debug().set("mem");
        auto enter = cg.world.enter(mem_param, location);
        cg.cur_mem = cg.world.extract(enter, 0_s, location);
        frame_ =     cg.world.extract(enter, 1_s, location);

        // name params and setup store locations
        for (const auto& param : params()) {
            auto p = continuation()->param(i++);
            p->debug().set(param->symbol());
            cg.emit(param.get(), p);
        }

        //assert(i == continuation()->num_params() || continuation()->type() == cg.empty_fn_type);

        if (continuation()->num_params() != 0
                && continuation()->params().back()->type()->isa<thorin::FnType>())
            ret_param_ = continuation()->params().back();
    }

    // descend into body
    auto def = cg.remit(body());
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
        auto global = pe_expr() ? cg.remit(pe_expr()) : cg.world.literal_bool(false, location);
        Array<const Def*> filter(continuation()->num_params());
        filter[i++] = global; // mem param

        for (const auto& param : params()) {
            auto pe_expr = param->pe_expr();
            filter[i++] = pe_expr
                          ? cg.world.arithop_or(global, cg.remit(pe_expr), pe_expr->location())
                          : global;
        }

        // HACK for unit
        if (auto tuple_type = continuation()->type()->ops().back()->isa<thorin::TupleType>()) {
            if (tuple_type->num_ops() == 0)
                filter[i++] = global;
        }

        continuation()->set_filter(filter);
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

const Def* FnDecl::emit(CodeGen& cg, const Def*) const {
    assert(def_ == nullptr);
    // no code is emitted for primops
    if (is_extern() && abi() == "\"thorin\"" && is_primop(symbol()))
        return def_;

    // create thorin function
    def_ = emit_head(cg, location());
    if (is_extern() && abi() == "")
        continuation_->make_external();

    // handle main function
    if (symbol() == "main")
        continuation()->make_external();

    if (body())
        emit_body(cg, location());
    return def_;
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

void ModuleDecl::emit(CodeGen&) const {}
void ImplItem::emit(CodeGen&) const {}

const Def* OptionDecl::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    auto enum_type = enum_decl()->type()->as<EnumType>();
    auto variant_type = cg.convert(enum_type)->op(1)->as<VariantType>();
    auto id = cg.world.literal_qu32(index(), location());
    if (num_args() == 0) {
        auto bot = cg.world.bottom(variant_type);
        return cg.world.struct_agg(cg.thorin_enum_type(enum_type), { id, bot });
    } else {
        auto continuation = cg.world.continuation(cg.convert(type())->as<thorin::FnType>(), {location(), symbol()});
        auto ret = continuation->param(continuation->num_params() - 1);
        auto mem = continuation->param(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = continuation->num_params(); i + 1 < e; i++)
            defs[i-1] = continuation->param(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.struct_agg(cg.thorin_enum_type(enum_type), { id, cg.world.variant(variant_type, option_val) });
        continuation->jump(ret, { mem, enum_val }, location());
        return continuation;
    }
}

const Def* StaticItem::emit(CodeGen& cg, const Def* init) const {
    assert(!init);
    init = !this->init() ? cg.world.bottom(cg.convert(type()), location()) : cg.remit(this->init());
    if (!is_mut())
        return init;
    return cg.world.global(init, true, debug());
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
    auto def = cg.remit(src());
    auto thorin_type = cg.convert(type());
    return cg.world.convert(thorin_type, def, location());
}

const Def* RValueExpr::lemit(CodeGen& cg) const {
    assert(src()->type()->isa<RefType>());
    return cg.lemit(src());
}

const Def* RValueExpr::remit(CodeGen& cg) const {
    if (src()->type()->isa<RefType>())
        return cg.load(cg.lemit(this), location());
    return cg.remit(src());
}

const Def* PathExpr::lemit(CodeGen&) const {
    assert(value_decl()->is_mut());
    return value_decl()->def();
}

const Def* PathExpr::remit(CodeGen& cg) const {
    auto def = value_decl()->def();
    return value_decl()->is_mut() ? cg.load(def, location()) : def;
}

const Def* PrefixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case INC:
        case DEC: {
            auto var = cg.lemit(rhs());
            auto val = cg.load(var, location());
            auto one = cg.world.one(val->type(), location());
            val = cg.world.arithop(Token::to_arithop((TokenTag) tag()), val, one, location());
            cg.store(var, val, location());
            return val;
        }
        case ADD: return cg.remit(rhs());
        case SUB: return cg.world.arithop_minus(cg.remit(rhs()), location());
        case NOT: return cg.world.arithop_not(cg.remit(rhs()), location());
        case TILDE: {
            auto def = cg.remit(rhs());
            auto ptr = cg.alloc(def->type(), rhs()->extra(), location());
            cg.store(ptr, def, location());
            return ptr;
        }
        case AND: {
            if (rhs()->type()->isa<RefType>())
                return cg.lemit(rhs());

            auto def = cg.remit(rhs());
            if (is_const(def))
                return cg.world.global(def, /*mutable*/ false, location());

            auto slot = cg.world.slot(cg.convert(rhs()->type()), cg.frame(), location());
            cg.store(slot, def, location());
            return slot;
        }
        case MUT: {
            return cg.lemit(rhs());
        }
        case RUNRUN: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world.run(def, location());
        }
        case HLT: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world.hlt(def, location());
        }
        case KNOWN: {
            auto def = cg.remit(rhs()->skip_rvalue());
            return cg.world.known(def, location());
        }
        case OR:
        case OROR:
            THORIN_UNREACHABLE;
        default:
            return cg.load(cg.lemit(this), location());
    }
}

const Def* PrefixExpr::lemit(CodeGen& cg) const {
    assert(tag() == MUL);
    return cg.remit(rhs());
}

const Def* InfixExpr::remit(CodeGen& cg) const {
    switch (tag()) {
        case ANDAND: {
            auto t = cg.basicblock({lhs()->location().front(), "and_lhs_t"});
            auto f = cg.basicblock({rhs()->location().front(), "and_lhs_f"});
            auto r = cg.basicblock(cg.world.type_bool(), {location().back(), "and_result"});

            auto lcond = cg.remit(lhs());
            auto mem = cg.cur_mem;
            cg.cur_bb->branch(lcond, t, f);

            cg.enter(t, mem);
            auto rcond = cg.remit(lhs());
            cg.cur_bb->jump(r, {cg.cur_mem, rcond});

            cg.enter(f, mem)->jump(r, {cg.cur_mem, cg.world.literal(false)});
            return cg.enter(r);
        }
        case OROR: {
            auto t = cg.basicblock({lhs()->location().front(), "or_lhs_t"});
            auto f = cg.basicblock({rhs()->location().front(), "or_lhs_f"});
            auto r = cg.basicblock(cg.world.type_bool(), {location().back(), "or_result"});

            auto lcond = cg.remit(lhs());
            auto mem = cg.cur_mem;
            cg.cur_bb->branch(lcond, t, f);

            cg.enter(f, mem);
            auto rcond = cg.remit(lhs());
            cg.cur_bb->jump(r, {cg.cur_mem, rcond});

            cg.enter(t, mem)->jump(r, {cg.cur_mem, cg.world.literal(true)});
            return cg.enter(r);
        }
        default:
            const TokenTag op = (TokenTag) tag();

            if (Token::is_assign(op)) {
                auto lvar = cg.lemit(lhs());
                auto rdef = cg.remit(rhs());

                if (op != Token::ASGN) {
                    auto sop = Token::separate_assign(op);
                    rdef = cg.world.binop(Token::to_binop(sop), cg.load(lvar, location()), rdef, location());
                }

                cg.store(lvar, rdef, location());
                return cg.world.tuple({}, location());
            }

            auto ldef = cg.remit(lhs());
            auto rdef = cg.remit(rhs());
            return cg.world.binop(Token::to_binop(op), ldef, rdef, location());
    }
}

const Def* PostfixExpr::remit(CodeGen& cg) const {
    auto var = cg.lemit(lhs());
    auto def = cg.load(var, location());
    auto one = cg.world.one(def->type(), location());
    cg.store(var, cg.world.arithop(Token::to_arithop((TokenTag) tag()), def, one, location()), location());
    return def;
}

const Def* DefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world.definite_array(cg.convert(type())->as<thorin::DefiniteArrayType>()->elem_type(), thorin_args, location());
}

const Def* RepeatedDefiniteArrayExpr::remit(CodeGen& cg) const {
    Array<const Def*> args(count());
    std::fill_n(args.begin(), count(), cg.remit(value()));
    return cg.world.definite_array(args, location());
}

const Def* TupleExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world.tuple(thorin_args, location());
}

const Def* IndefiniteArrayExpr::remit(CodeGen& cg) const {
    extra_ = cg.remit(dim());
    return cg.world.indefinite_array(cg.convert(type())->as<thorin::IndefiniteArrayType>()->elem_type(), extra_, location());
}

const Def* SimdExpr::remit(CodeGen& cg) const {
    Array<const Def*> thorin_args(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        thorin_args[i] = cg.remit(arg(i));
    return cg.world.vector(thorin_args, location());
}

const Def* StructExpr::remit(CodeGen& cg) const {
    Array<const Def*> defs(num_elems());
    for (const auto& elem : elems())
        defs[elem->field_decl()->index()] = cg.remit(elem->expr());
    return cg.world.struct_agg(cg.convert(type())->as<thorin::StructType>(), defs, location());
}

const Def* TypeAppExpr::lemit(CodeGen&) const { THORIN_UNREACHABLE; }

const Def* TypeAppExpr::remit(CodeGen& /*cg*/) const {
    assert(false && "TODO");
    THORIN_UNREACHABLE;
}

const Def* MapExpr::lemit(CodeGen& cg) const {
    auto agg = cg.lemit(lhs());
    return cg.world.lea(agg, cg.remit(arg(0)), location());
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
                            return cg.world.bitcast(cg.convert(type_expr->type_arg(0)), cg.remit(arg(0)), location());
                        } else if (name == "select") {
                            return cg.world.select(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), location());
                        } else if (name == "insert") {
                            return cg.world.insert(cg.remit(arg(0)), cg.remit(arg(1)), cg.remit(arg(2)), location());
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
                                cg.world.mem_type(), cg.world.type_pu32(), ptr_type, poly_type,
                                cg.world.fn_type({ cg.world.mem_type(), poly_type }) });
                            auto cont = cg.world.continuation(fn_type, {location(), "atomic"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "cmpxchg") {
                            auto ptr_type = cg.convert(arg(0)->type());
                            auto poly_type = ptr_type->as<thorin::PtrType>()->pointee();
                            auto fn_type = cg.world.fn_type({
                                cg.world.mem_type(), ptr_type, poly_type, poly_type,
                                cg.world.fn_type({ cg.world.mem_type(), poly_type, cg.world.type_bool() })
                            });
                            auto cont = cg.world.continuation(fn_type, {location(), "cmpxchg"});
                            cont->set_intrinsic();
                            dst = cont;
                        } else if (name == "pe_info") {
                            auto poly_type = cg.convert(arg(1)->type());
                            auto string_type = cg.world.ptr_type(cg.world.indefinite_array_type(cg.world.type_pu8()));
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

        dst = dst ? dst : cg.remit(lhs());

        std::vector<const Def*> defs;
        defs.push_back(nullptr);    // reserve for mem but set later - some other args may update the monad
        for (const auto& arg : args())
            defs.push_back(cg.remit(arg.get()));
        defs.front() = cg.cur_mem; // now get the current memory value

        auto ret_type = num_args() == fn_type->num_params() ? nullptr : cg.convert(fn_type->return_type());
        const Def* ret;
        std::tie(cg.cur_bb, ret) = cg.cur_bb->call(dst, defs, ret_type, thorin::Debug(location(), dst->name()) + "_cont");
        if (ret_type)
            cg.cur_mem = cg.cur_bb->param(0);

        return ret;
    } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>() || ltype->isa<SimdType>()) {
        auto index = cg.remit(arg(0));
        return cg.world.extract(cg.remit(lhs()), index, location());
    }
    THORIN_UNREACHABLE;
}

const Def* FieldExpr::lemit(CodeGen& cg) const {
    auto value = cg.lemit(lhs());
    return cg.world.lea(value, cg.world.literal_qu32(index(), location()), location());
}

const Def* FieldExpr::remit(CodeGen& cg) const {
    return cg.world.extract(cg.remit(lhs()), index(), location());
}

const Def* BlockExpr::remit(CodeGen& cg) const {
    for (const auto& stmt : stmts())
        cg.emit(stmt.get());
    return cg.remit(expr());
}

const Def* IfExpr::remit(CodeGen& cg) const {
    auto thorin_type = cg.convert(type());

    auto t = cg.basicblock({then_expr()->location().front(), "if_then"});
    auto f = cg.basicblock({else_expr()->location().front(), "if_else"});
    auto j = cg.basicblock(thorin_type, {location().back(), "if_join"});

    auto c = cg.remit(cond());
    cg.cur_bb->branch(c, t, f, cond()->location().back());
    auto mem = cg.cur_mem;

    cg.enter(t, mem);
    auto tdef = cg.remit(then_expr());
    cg.cur_bb->jump(j, {cg.cur_mem, tdef}, location().back());

    cg.enter(f, mem);
    auto fdef = cg.remit(else_expr());
    cg.cur_bb->jump(j, {cg.cur_mem, fdef}, location().back());

    return cg.enter(j);
}

#if 0
void MatchExpr::emit_jump(CodeGen& cg, Continuation* x) const {
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
                    defs[i] = cg.world.literal_qu32(option_decl->index(), arm(i)->ptrn()->location());
                }
                targets[i] = JumpTarget({arm(i)->location().front(), "case"});
            }
        }
        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world.extract(matcher, 0_u32, matcher->debug());
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
                ? cg.world.literal_bool(true, arm(i)->ptrn()->location())
                : arm(i)->ptrn()->emit_cond(cg, matcher);

            cg.branch(cond, cur, next);
            if (cg.enter(cur))
                cg.emit_jump(arm(i)->expr(), x);

            cg.enter(next);
        }
    }
    cg.jump(x, location().back());
}

#endif
const Def* MatchExpr::remit(CodeGen& cg) const {
    //JumpTarget x({location().back(), "next"});
    return cg.world.tuple({});
}


const Def* WhileExpr::remit(CodeGen& cg) const {
#if 0
    auto head_bb = cg.world.basicblock({cond()->location().front(), "while_head"});
    auto body_bb = cg.world.basicblock({body()->location().front(), "while_body"});
    auto exit_bb = cg.world.basicblock({body()->location().front(), "while_exit"});
    auto continue_continuation = cg.create_continuation(continue_decl());

    cg.cur_bb->jump(head_bb, {cg.cur_mem}, cond()->location().back());

    cg.enter(head_bb);
    cg.emit_branch(cond(), body_bb, exit_bb);

    cg.enter(body_bb);
    cg.remit(body());
    cg.jump_to_continuation(continue_continuation, cond()->location().back());
    cg.cur_bb->jump(head_bb, {cg.cur_mem}, cond()->location().back());

    cg.enter(exit_bb);
#endif
    return cg.world.tuple({});
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
    //TODO
    //auto fun = cg.remit(map_expr->lhs());

    defs.front() = cg.cur_mem; // now get the current memory monad
    //TODO
    //cg.call(fun, defs, nullptr, map_expr->location());

    cg.enter(break_continuation);
    if (break_continuation->num_params() == 2)
        return break_continuation->param(1);
    else {
        Array<const Def*> defs(break_continuation->num_params()-1);
        for (size_t i = 0, e = defs.size(); i != e; ++i)
            defs[i] = break_continuation->param(i+1);
        return cg.world.tuple(defs, location());
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
    return cg.world.literal(true);
}

void EnumPtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    if (num_args() == 0) return;
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1), location());
    for (size_t i = 0, e = num_args(); i != e; ++i) {
        cg.emit(arg(i), num_args() == 1 ? variant : cg.world.extract(variant, i, location()));
    }
}

const thorin::Def* EnumPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    auto index = path()->decl()->as<OptionDecl>()->index();
    auto cond = cg.world.cmp_eq(cg.world.extract(init, 0_u32, location()), cg.world.literal_qu32(index, location()));
    if (num_args() > 0) {
        auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
        auto variant = cg.world.cast(variant_type, cg.world.extract(init, 1, location()), location());
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (!arg(i)->is_refutable()) continue;
            auto arg_cond = arg(i)->emit_cond(cg, num_args() == 1 ? variant : cg.world.extract(variant, i, location()));
            cond = cg.world.arithop_and(cond, arg_cond, location());
        }
    }
    return cond;
}

void TuplePtrn::emit(CodeGen& cg, const thorin::Def* init) const {
    for (size_t i = 0, e = num_elems(); i != e; ++i)
        cg.emit(elem(i), cg.world.extract(init, i, location()));
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

const thorin::Def* LiteralPtrn::emit_literal(CodeGen& cg) const {
    auto def = cg.remit(literal());
    return has_minus() ? cg.world.arithop_minus(def, def->debug()) : def;
}

void LiteralPtrn::emit(CodeGen&, const thorin::Def*) const {}

const thorin::Def* LiteralPtrn::emit_cond(CodeGen& cg, const thorin::Def* init) const {
    return cg.world.cmp_eq(init, emit_literal(cg));
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen& cg) const {
    cg.remit(expr());
}

void ItemStmt::emit(CodeGen& cg) const {
    cg.emit(item());
}

void LetStmt::emit(CodeGen& cg) const {
    cg.emit(ptrn(), init() ? cg.remit(init()) : cg.world.bottom(cg.convert(ptrn()->type()), ptrn()->location()));
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

    auto assembly = cg.world.assembly(outs, cg.cur_mem, ins, asm_template(),
            output_constraints(), input_constraints(), clobbers(), flags, location());

    size_t i = 0;
    cg.cur_mem = assembly->out(i++);
    for (const auto& output: outputs())
        cg.store(cg.lemit(output->expr()), assembly->out(i++), location());
}

//------------------------------------------------------------------------------

void emit(World& world, const Module* mod) {
    CodeGen cg(world);
    mod->emit(cg);
}

//------------------------------------------------------------------------------

}
