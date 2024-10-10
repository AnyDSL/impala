#include "mim/world.h"

#include "impala/ast.h"
#include "mim/plug/core/core.h"
#include "mim/plug/math/math.h"
#include "mim/plug/mem/mem.h"

using namespace mim;

namespace impala {

class CodeGen {
public:
  CodeGen(World &world) : world(world) {}

  Dbg dbg(const Decl *decl) {
    return {decl->loc(), world.sym(decl->symbol().str())};
  }

  /// Lam of type { @c cn(mem) } or { @c cn(mem, type) } depending on whether @p
  /// type is @c nullptr.
  Lam *basicblock(const mim::Def *type) {
    auto cn = type ? world.cn({world.annex<mem::M>(), type})
                   : world.cn(world.annex<mem::M>());
    auto bb = world.mut_lam(cn);
    bb->var(0_s)->set("mem");
    return bb;
  }
  Lam *basicblock() { return basicblock(nullptr); }

  Lam *enter(Lam *bb) {
    cur_bb = bb;
    cur_mem = bb->var(0_s);
    return bb;
  }

  const Def *lit_one(const Type *type) {
    if (is_int(type))
      return world.lit(convert(type), 1);
    switch (type->tag()) {
    case PrimType_f16:
      return math::lit_f(world, 1._f16);
    case PrimType_f32:
      return math::lit_f(world, 1._f32);
    case PrimType_f64:
      return math::lit_f(world, 1._f64);
    default:
      fe::unreachable();
    }
  }

  std::pair<Lam *, const Def *> call(const Def *callee, Defs args,
                                     const mim::Def *ret_type) {
    if (ret_type == nullptr) {
      cur_bb->app(false, callee, args);
      auto next = basicblock()->set("unreachable");
      return std::make_pair(next, nullptr);
    }

    std::vector<const mim::Def *> cont_args;
    cont_args.push_back(world.annex<mem::M>());

    auto sigma = ret_type->isa_imm<mim::Sigma>(); // if the return type is a
                                                  // sigma, flatten it
    auto arr = ret_type->isa<mim::Arr>(); // ditto: for arr with known arity
    auto arity = arr ? arr->isa_lit_arity() : std::nullopt;
    if (sigma)
      for (auto op : sigma->ops())
        cont_args.push_back(op);
    else if (arity)
      for (size_t i = 0, e = *arity; i != e; ++i)
        cont_args.push_back(arr->body());
    else
      cont_args.push_back(ret_type);

    // next is the return lam
    auto next = world.mut_lam(world.cn(cont_args));
    next->var(0_s)->set("mem");

    // create jump to next
    size_t csize = args.size() + 1;
    Array<const Def *> cargs(csize);
    *std::copy(args.begin(), args.end(), cargs.begin()) = next;
    cur_bb->app(false, callee, cargs);

    // determine return value
    const Def *ret = nullptr;
    if (sigma || arity) {
      Array<const Def *> vars(next->num_vars() - 1);
      for (size_t i = 1, e = next->num_vars(); i != e; ++i)
        vars[i - 1] = next->var(i);
      ret = world.tuple(ret_type, vars)->set(callee->dbg());
    } else {
      ret = next->var(1_s)->set(callee->dbg());
    }

    return std::make_pair(next, ret);
  }

  Lam *create_lam(const LocalDecl *decl) {
    auto result =
        world.mut_lam(convert(decl->type())->as<mim::Pi>())->set(dbg(decl));
    result->var(0)->set("mem");
    decl->def_ = result;
    return result;
  }

  const Def *handle_mem_res(const Def *mem_res) {
    auto [mem, res] = mem_res->projs<2>();
    cur_mem = mem;
    return res;
  }

  const Def *load(const Def *ptr) {
    return handle_mem_res(world.call<mem::load>(Defs{cur_mem, ptr}));
  }
  const Def *slot(const Def *type) {
    return handle_mem_res(mem::op_slot(type, cur_mem));
  }

  const Def *store(const Def *ptr, const Def *val) {
    return cur_mem = world.call<mem::store>(Defs{cur_mem, ptr, val});
  }

  const Def *alloc(const mim::Def *type) {
    auto alloc = mem::op_alloc(type, cur_mem);
    cur_mem = world.extract(alloc, 2_s, 0_s);
    auto result = world.extract(alloc, 2, 1);
    auto ptr = mim::force<mem::Ptr>(result->type());
    auto [pointee, as] = ptr->args<2>();
    if (auto arr = pointee->isa<Arr>())
      return world.call<core::bitcast>(
          world.call<mem::Ptr>(Defs{world.arr_unsafe(arr->body()), as}),
          result);
    return result;
  }

  const mim::Def *rev_diff(const mim::Def * /*primal*/) {
    return nullptr; /*world.op_rev_diff(primal);*/
  }

  const mim::Def *convert(const Type *type) {
    if (auto t = thorin_type(type))
      return t;
    auto t = convert_rec(type);
    return thorin_type(type) = t;
  }

  const mim::Def *convert_rec(const Type *);

  const mim::Def *&thorin_type(const Type *type) {
    return impala2thorin_[type];
  }
  const mim::Sigma *&thorin_struct_type(const StructType *type) {
    return struct_type_impala2thorin_[type];
  }
  const mim::Sigma *&thorin_enum_type(const EnumType *type) {
    return enum_type_impala2thorin_[type];
  }

  World &world;
  const Fn *cur_fn = nullptr;
  TypeMap<const mim::Def *> impala2thorin_;
  GIDMap<const StructType *, const mim::Sigma *> struct_type_impala2thorin_;
  GIDMap<const EnumType *, const mim::Sigma *> enum_type_impala2thorin_;
  Lam *cur_bb = nullptr;
  const Def *cur_mem = nullptr;
};

/*
 * Type
 */

const mim::Def *CodeGen::convert_rec(const Type *type) {
  if (auto lambda = type->isa<Lambda>()) {
    auto body = convert(lambda->body());
    auto pi = world.pi(world.type(), body->type());
    return world.lam(pi, true, body)->set(lambda->name());
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
            case PrimType_f16 : return world.annex<math::F16>();
            case PrimType_f32 : return world.annex<math::F32>();
            case PrimType_f64 : return world.annex<math::F64>();
    // clang-format on
    default:
      fe::unreachable();
    }
  } else if (auto cn = type->isa<FnType>()) {
    std::vector<const mim::Def *> nops;
    nops.push_back(world.annex<mem::M>());
    for (size_t i = 0, e = cn->num_params(); i != e; ++i)
      nops.push_back(convert(cn->param(i)));
    return world.cn(nops);
  } else if (auto tuple_type = type->isa<TupleType>()) {
    std::vector<const mim::Def *> nops;
    for (auto &&op : tuple_type->ops())
      nops.push_back(convert(op));
    return world.sigma(nops);
  } else if (auto struct_type = type->isa<StructType>()) {
    auto s = world.mut_sigma(struct_type->num_ops())
                 ->set(struct_type->struct_decl()->symbol().str());
    thorin_struct_type(struct_type) = s;
    thorin_type(type) = s;
    size_t i = 0;
    for (auto &&op : struct_type->ops())
      s->set(i++, convert(op));
    thorin_type(type) = nullptr; // will be set again by CodeGen's wrapper
    return s;
#if 0
    } else if (auto enum_type = type->isa<EnumType>()) {
        auto s = world.sigma(2, {enum_type->enum_decl()->symbol().c_str()});
        thorin_enum_type(enum_type) = s;
        thorin_type(enum_type) = s;

        auto enum_decl = enum_type->enum_decl();
        mim::DefSet variants;
        for (auto&& option : enum_decl->option_decls())
            variants.insert(option->variant_type(*this));
        mim::Array<const mim::Def*> ops(variants.size());
        std::copy(variants.begin, variants.end(), ops.begin);

        s->set(0, world.type_int(32));
        s->set(1, world.variant_type(ops));
        thorin_type(enum_type) = nullptr;
        return s;
#endif
  } else if (auto ptr = type->isa<PtrType>()) {
    auto as = world.lit_nat((nat_t)mem::AddrSpace(ptr->addr_space()));
    return world.call<mem::Ptr>(Defs{convert(ptr->pointee()), as});
  } else if (auto definite_array_type = type->isa<DefiniteArrayType>()) {
    return world.arr(definite_array_type->dim(),
                     convert(definite_array_type->elem_type()));
  } else if (auto indefinite_array_type = type->isa<IndefiniteArrayType>()) {
    return world.arr_unsafe(convert(indefinite_array_type->elem_type()));
  } else if (type->isa<NoRetType>()) {
    return nullptr; // TODO use bottom type - once it is available in thorin
  }
  Stream stream;
  type->stream(stream);
  std::cout.flush();
  fe::unreachable();
}

/*
 * Decls and Function
 */

void LocalDecl::emit(CodeGen &cg, const Def *init) const {
  assert(def_ == nullptr);

  auto thorin_type = cg.convert(type());
  init = init ? init : *cg.world.bot(thorin_type);

  if (is_mut()) {
    def_ = cg.slot(thorin_type)->set(cg.dbg(this));
    cg.cur_mem =
        cg.world.call<mem::store>(Defs{cg.cur_mem, def_, init})->set(loc());
  } else {
    def_ = init;
  }
}

const mim::Def *OptionDecl::variant_type(CodeGen &cg) const {
  std::vector<const mim::Def *> types;
  for (auto &&arg : args())
    types.push_back(cg.convert(arg->type()));
  if (num_args() == 1)
    return types.back();
  return cg.world.sigma(types);
}

Lam *Fn::fn_emit_head(CodeGen &cg, Loc loc) const {
  auto t = cg.convert(fn_type())->as<mim::Pi>();
  return lam_ = cg.world.mut_lam(t)->set(loc, fn_symbol().remove_quotation());
}

void Fn::fn_emit_body(CodeGen &cg, Loc loc) const {
  // setup function nest
  auto old_fn = cg.cur_fn;
  auto old_bb = cg.cur_bb;
  cg.cur_fn = this;
  cg.cur_bb = lam();

  auto old_mem = cg.cur_mem;

  // setup memory
  size_t i = 0;
  auto mem_param = lam()->var(i++)->set("mem");
  cg.cur_mem = mem_param;

  // name vars and setup store locs
  for (auto &&param : params()) {
    auto var = lam()->var(i++)->set(param->loc(), param->symbol().str());
    param->emit(cg, var);
  }

  if (lam()->num_vars() != 0 && lam()->vars().back()->type()->isa<Pi>())
    ret_param_ = lam()->vars().back();

  // descend into body
  auto def = body()->remit(cg);
  if (def) {
    // flatten returned values
    if (auto tuple = body()->type()->isa<TupleType>()) {
      Array<const Def *> ret_values(tuple->num_ops() + 1);
      for (size_t i = 0, e = tuple->num_ops(); i != e; ++i)
        ret_values[i + 1] = cg.world.extract(def, e, i);
      ret_values[0] = cg.cur_mem;
      cg.cur_bb->app(false, ret_param(), ret_values)->set(loc.finis);
    } else
      cg.cur_bb->app(false, ret_param(), {cg.cur_mem, def})->set(loc.finis);
  }

  lam()->reset(
      {filter() ? filter()->remit(cg) : cg.world.lit_ff(), lam()->body()});
  cg.cur_mem = old_mem;

  cg.cur_fn = old_fn;
  cg.cur_bb = old_bb;
}

/*
 * items
 */

void Module::emit(CodeGen &cg) const {
  for (auto &&item : items())
    item->emit_head(cg);
  for (auto &&item : items())
    item->emit(cg);
}

static bool is_primop(const Symbol &name) {
  // clang-format off
    if      (name == "select")   return true;
    else if (name == "sizeof")   return true;
    else if (name == "bitcast")  return true;
    else if (name == "insert")   return true;
    else if (name == "rev_diff") return true;
  // clang-format on
  return false;
}

void FnDecl::emit_head(CodeGen &cg) const {
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

void FnDecl::emit(CodeGen &cg) const {
  if (body())
    fn_emit_body(cg, loc());
}

void ExternBlock::emit_head(CodeGen &cg) const {
  for (auto &&fn_decl : fn_decls()) {
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

void ModuleDecl::emit(CodeGen &) const {}
void ImplItem::emit(CodeGen &) const {}

void StaticItem::emit_head(CodeGen &cg) const {
  auto t = cg.convert(type());
  auto bot = cg.world.bot(t);
  auto g = cg.world.global(cg.world.call<mem::Ptr0>(t), is_mut())->set(loc());
  g->set(bot);
  def_ = g;
}

void StaticItem::emit(CodeGen &cg) const {
  if (init()) {
    auto global = def_->as_mut<Global>();
    if (global->is_set())
      global->unset();
    global->set(init()->remit(cg));
  }
}

void StructDecl::emit_head(CodeGen &cg) const { cg.convert(type()); }

void OptionDecl::emit(CodeGen & /*cg*/) const {
#if 0
    auto enum_type = enum_decl()->type()->as<EnumType>();
    auto variant_type = cg.convert(enum_type)->op(1)->as<VariantType>();
    auto id = cg.world.lit_idx(index())->set(loc());
    if (num_args() == 0) {
        auto bot = cg.world.bot(variant_type);
        def_ = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, bot });
    } else {
        auto lam = cg.world.mut_lam(cg.convert(type())->as<mim::Pi>())->set(symbol().c_str(), loc());
        auto ret = lam->var(lam->num_vars() - 1);
        auto mem = lam->var(0);
        Array<const Def*> defs(num_args());
        for (size_t i = 1, e = lam->num_vars(); i + 1 < e; i++)
            defs[i-1] = lam->var(i);
        auto option_val = num_args() == 1 ? defs.back() : cg.world.tuple(defs);
        auto enum_val = cg.world.tuple(cg.thorin_enum_type(enum_type), { id, cg.world.variant(variant_type, option_val) });
        lam->app(ret, { mem, enum_val })->set(loc());
        def_ = lam;
    }
#endif
}

void EnumDecl::emit_head(CodeGen &cg) const {
  for (auto &&option_decl : option_decls())
    option_decl->emit(cg);
  cg.convert(type());
}

void TraitDecl::emit(CodeGen &) const {}
void Typedef::emit(CodeGen &) const {}

/*
 * expressions
 */

const Def *Expr::lemit(CodeGen &) const { fe::unreachable(); }
const Def *Expr::remit(CodeGen &cg) const { return cg.load(lemit(cg)); }
const Def *EmptyExpr::remit(CodeGen &cg) const { return cg.world.tuple(); }

const Def *LiteralExpr::remit(CodeGen &cg) const {
  switch (tag()) {
    // clang-format off
        case LIT_bool: return cg.world.lit_bool(get<bool>());
        case LIT_i8  : return cg.world.lit_idx (get<  s8>())->set(loc());
        case LIT_i16 : return cg.world.lit_idx (get< s16>())->set(loc());
        case LIT_i32 : return cg.world.lit_idx (get< s32>())->set(loc());
        case LIT_i64 : return cg.world.lit_idx (get< s64>())->set(loc());
        case LIT_u8  : return cg.world.lit_idx (get<  u8>())->set(loc());
        case LIT_u16 : return cg.world.lit_idx (get< u16>())->set(loc());
        case LIT_u32 : return cg.world.lit_idx (get< u32>())->set(loc());
        case LIT_u64 : return cg.world.lit_idx (get< u64>())->set(loc());
        case LIT_f16 : return math::lit_f(cg.world, get<f16>())->set(loc());
        case LIT_f32 : return math::lit_f(cg.world, get<f32>())->set(loc());
        case LIT_f64 : return math::lit_f(cg.world, get<f64>())->set(loc());
  // clang-format on
  default:
    fe::unreachable();
  }
}

const Def *CharExpr::remit(CodeGen &cg) const {
  return cg.world.lit_idx<u8>(value())->set(loc());
}

const Def *StrExpr::remit(CodeGen &cg) const {
  Array<const Def *> args(values_.size());
  for (size_t i = 0, e = args.size(); i != e; ++i)
    args[i] = cg.world.lit_idx<u8>(values_[i])->set(loc());

  return cg.world.tuple(args)->set(loc());
}

const Def *CastExpr::remit(CodeGen &cg) const {
  auto def = src()->remit(cg);
  auto src_type = src()->type();
  auto dst_type = type();
  auto dst = cg.convert(dst_type);

  if (src_type->isa<PtrType>() || dst_type->isa<PtrType>()) {
    return cg.world.call<core::bitcast>(dst, def)->set(loc());
  } else if (is_int(src_type) || is_bool(src_type)) {
    if (is_signed(src_type)) {
      if (is_int(dst_type) || is_bool(dst_type)) {
        return cg.world.call(core::conv::s, Idx::size(dst), def)->set(loc());
      } else {
        return cg.world.call(math::conv::s2f, match<math::F>(dst)->arg(), def)
            ->set(loc());
      }
    } else {
      if (is_int(dst_type) || is_bool(dst_type)) {
        return cg.world.call(core::conv::u, Idx::size(dst), def)->set(loc());
      } else {
        return cg.world.call(math::conv::u2f, match<math::F>(dst)->arg(), def)
            ->set(loc());
      }
    }
  } else {
    if (is_int(dst_type) || is_bool(dst_type)) {
      if (is_signed(dst_type))
        return cg.world.call(math::conv::f2s, Idx::size(dst), def)->set(loc());
      else
        return cg.world.call(math::conv::f2u, Idx::size(dst), def)->set(loc());
    } else if (is_float(src_type) && is_float(dst_type)) {
      return cg.world.call(math::conv::f2f, match<math::F>(dst)->arg(), def)
          ->set(loc());
    } else if (auto src_tup = src_type->isa<TupleType>()) {
      if (auto dst_tup = dst_type->isa<TupleType>()) {
        if (src_tup->num_ops() && dst_tup->num_ops()) {
          size_t n = src_tup->num_ops();
          DefArray new_ops(n, [&](size_t i) {
            return cg.world
                .call<core::bitcast>(dst->proj(n, i), def->proj(n, i))
                ->set(loc());
          });
          return cg.world.tuple(new_ops);
        }
      }
    }

    return cg.world.call<core::bitcast>(dst, def)->set(loc());
  }
  fe::unreachable();
}

const Def *RValueExpr::lemit(CodeGen &cg) const {
  assert(src()->type()->isa<RefType>());
  return src()->lemit(cg);
}

const Def *RValueExpr::remit(CodeGen &cg) const {
  if (src()->type()->isa<RefType>())
    return cg.load(lemit(cg))->set(loc());
  return src()->remit(cg);
}

const Def *PathExpr::lemit(CodeGen &) const {
  assert(value_decl()->is_mut());
  return value_decl()->def();
}

const Def *PathExpr::remit(CodeGen &cg) const {
  auto def = value_decl()->def();
  // This whole global thing is incorrect.
  // Example:
  // static a = 1;
  // static b = a;
  // Emitting this requires a load. Currently, it works because of the following
  // hack. But the hack no longer works if the order is reversed: static b = a;
  // static a = 1;
  // In this case, during the emission of 'static b = a', the static item 'a'
  // has not been replaced yet and is considered mutable.
  auto global = def->isa<Global>();
  if (global && !global->is_mutable())
    return global->init();
  return value_decl()->is_mut() || global ? cg.load(def)->set(loc()) : def;
}

static core::Mode type2wmode(const Type *type) {
  return is_bool(type) ? core::Mode::nuw
                       : (is_signed(type) ? core::Mode::nsw : core::Mode::none);
}

const Def *PrefixExpr::remit(CodeGen &cg) const {
  switch (tag()) {
  case INC:
  case DEC: {
    auto var = rhs()->lemit(cg);
    auto val = cg.load(var)->set(loc());
    auto one = cg.lit_one(type())->set(loc());
    if (is_int(type()))
      val = cg.world
                .call(tag() == INC ? core::wrap ::add : core::wrap ::sub,
                      type2wmode(type()), Defs{val, one})
                ->set(loc());
    else
      val = cg.world
                .call(tag() == INC ? math::arith::add : math::arith::sub,
                      math::Mode::none, Defs{val, one})
                ->set(loc());
    cg.store(var, val)->set(loc());
    return val;
  }
  case ADD:
    return rhs()->remit(cg);
  case SUB:
    if (is_int(type())) {
      auto mode = type2wmode(type());
      return cg.world.call<core::minus>(mode, rhs()->remit(cg))->set(loc());
    } else {
      return cg.world.call<math::minus>(math::Mode::none, rhs()->remit(cg))
          ->set(loc());
    }
  case NOT:
    return cg.world.call(core::bit1::neg, 0, rhs()->remit(cg))->set(loc());
  case TILDE: {
    auto def = rhs()->remit(cg);
    auto ptr = cg.alloc(def->type())->set(loc());
    cg.store(ptr, def)->set(loc());
    return ptr;
  }
  case AND: {
    if (rhs()->type()->isa<RefType>())
      return rhs()->lemit(cg);

    auto def = rhs()->remit(cg);
    if (def->dep_const()) {
      auto g =
          cg.world
              .global(cg.world.call<mem::Ptr0>(def->type()), /*mutable*/ false)
              ->set(loc());
      g->set(def);
      return g;
    }

    auto slot = cg.slot(cg.convert(rhs()->type()))->set(loc());
    cg.store(slot, def)->set(loc());
    return slot;
  }
  case MUT: {
    return rhs()->lemit(cg);
  }
  case RUNRUN: {
    auto def = rhs()->skip_rvalue()->remit(cg);
    return core::op(core::pe::run, def)->set(loc());
  }
  case HLT: {
    auto def = rhs()->skip_rvalue()->remit(cg);
    return core::op(core::pe::hlt, def)->set(loc());
  }
  case KNOWN: {
    auto def = rhs()->skip_rvalue()->remit(cg);
    return core::op(core::pe::known, def)->set(loc());
  }
  case OR:
  case OROR:
    fe::unreachable();
  default:
    return cg.load(lemit(cg))->set(loc());
  }
}

const Def *PrefixExpr::lemit(CodeGen &cg) const {
  assert(tag() == MUL);
  return rhs()->remit(cg);
}

void Expr::emit_branch(CodeGen &cg, Lam *jump_t, Lam *jump_f) const {
  auto cond = remit(cg);
  cg.cur_bb->branch(false, cond, jump_t, jump_f, cg.cur_mem)->set(loc().finis);
}

void InfixExpr::emit_branch(CodeGen &cg, Lam *jump_t, Lam *jump_f) const {
  auto jump_type = jump_t->type();
  switch (tag()) {
  case OROR: {
    auto or_f = cg.world.mut_lam(jump_type)->set(loc().finis, "or_f");
    lhs()->emit_branch(cg, jump_t, or_f);
    cg.enter(or_f);
    rhs()->emit_branch(cg, jump_t, jump_f);
  } break;
  case ANDAND: {
    auto and_t = cg.world.mut_lam(jump_type)->set(loc().finis, "and_t");
    lhs()->emit_branch(cg, and_t, jump_f);
    cg.enter(and_t);
    rhs()->emit_branch(cg, jump_t, jump_f);
  } break;
  default:
    return Expr::emit_branch(cg, jump_t, jump_f);
  }
}

const Def *InfixExpr::remit(CodeGen &cg) const {
  auto &w = cg.world;
  // clang-format off
    switch (tag()) {
        case OROR:
        case ANDAND: {
            auto result    = cg.basicblock(w.type_bool())->set(loc().finis, "infix_result");
            auto jump_type = w.cn(w.annex<mem::M>());
            auto jump_t    = w.mut_lam(jump_type)->set(loc().finis, "jump_t");
            auto jump_f    = w.mut_lam(jump_type)->set(loc().finis, "jump_f");
            emit_branch(cg, jump_t, jump_f);
            jump_t->app(false, result, { jump_t->var(0_s), w.lit_tt() });
            jump_f->app(false, result, { jump_f->var(0_s), w.lit_ff() });
            return cg.enter(result)->var(1);
        }
        default: {
            auto op = tag();

            if (Token::is_assign((TokenTag) op)) {
                auto lvar = lhs()->lemit(cg);
                auto rdef = rhs()->remit(cg);

                if (op == ASGN) {
                    cg.store(lvar, rdef)->set(loc());
                    return w.tuple();
                }

                auto ldef = cg.load(lhs()->lemit(cg))->set(loc());

                if (is_float(rhs()->type())) {
                    switch (op) {
                        case ADD_ASGN: rdef = w.call(math::arith::add, math::Mode::none, Defs{ldef, rdef})->set(loc()); break;
                        case SUB_ASGN: rdef = w.call(math::arith::sub, math::Mode::none, Defs{ldef, rdef})->set(loc()); break;
                        case MUL_ASGN: rdef = w.call(math::arith::mul, math::Mode::none, Defs{ldef, rdef})->set(loc()); break;
                        case DIV_ASGN: rdef = w.call(math::arith::div, math::Mode::none, Defs{ldef, rdef})->set(loc()); break;
                        case REM_ASGN: rdef = w.call(math::arith::rem, math::Mode::none, Defs{ldef, rdef})->set(loc()); break;
                        default: fe::unreachable();
                    }
                } else if (is_bool(rhs()->type())) {
                    switch (op) {
                        case AND_ASGN: rdef = w.call(core::bit2::and_, 0, Defs{ldef, rdef})->set(loc()); break;
                        case  OR_ASGN: rdef = w.call(core::bit2:: or_, 0, Defs{ldef, rdef})->set(loc()); break;
                        case XOR_ASGN: rdef = w.call(core::bit2::xor_, 0, Defs{ldef, rdef})->set(loc()); break;
                        default: fe::unreachable();
                    }
                } else {
                    auto mode = type2wmode(rhs()->type());
                    bool s = is_signed(rhs()->type());

                    switch (op) {
                        case AND_ASGN: rdef = w.call(core::bit2::and_, 0, Defs{ldef, rdef})->set(loc()); break;
                        case  OR_ASGN: rdef = w.call(core::bit2:: or_, 0, Defs{ldef, rdef})->set(loc()); break;
                        case XOR_ASGN: rdef = w.call(core::bit2::xor_, 0, Defs{ldef, rdef})->set(loc()); break;
                        case ADD_ASGN: rdef = w.call(core::wrap:: add, mode, Defs{ldef, rdef})->set(loc()); break;
                        case SUB_ASGN: rdef = w.call(core::wrap:: sub, mode, Defs{ldef, rdef})->set(loc()); break;
                        case MUL_ASGN: rdef = w.call(core::wrap:: mul, mode, Defs{ldef, rdef})->set(loc()); break;
                        case SHL_ASGN: rdef = w.call(core::wrap:: shl, mode, Defs{ldef, rdef})->set(loc()); break;
                        case SHR_ASGN: rdef = w.call(s ? core::shr::a : core::shr::l, Defs{ldef, rdef})->set(loc()); break;
                        case DIV_ASGN: rdef = cg.handle_mem_res(w.call(s ? core::div::sdiv : core::div::udiv, Defs{cg.cur_mem, w.tuple({ldef, rdef})})->set(loc())); break;
                        case REM_ASGN: rdef = cg.handle_mem_res(w.call(s ? core::div::srem : core::div::urem, Defs{cg.cur_mem, w.tuple({ldef, rdef})})->set(loc())); break;
                        default: fe::unreachable();
                    }
                }

                cg.store(lvar, rdef)->set(loc());
                return w.tuple();
            }

            auto ldef = lhs()->remit(cg);
            auto rdef = rhs()->remit(cg);

            if (is_float(rhs()->type())) {
                switch (op) {
                    case  EQ: return w.call(math::cmp::    e, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case  NE: return w.call(math::cmp::  une, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case  LT: return w.call(math::cmp::    l, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case  LE: return w.call(math::cmp::   le, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case  GT: return w.call(math::cmp::    g, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case  GE: return w.call(math::cmp::   ge, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case ADD: return w.call(math::arith::add, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case SUB: return w.call(math::arith::sub, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case MUL: return w.call(math::arith::mul, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case DIV: return w.call(math::arith::div, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    case REM: return w.call(math::arith::rem, math::Mode::none, Defs{ldef, rdef})->set(loc());
                    default: fe::unreachable();
                }
            } else if (is_bool(rhs()->type())) {
                switch (op) {
                    //
                    case  EQ: return w.call(core::icmp::   e, Defs{ldef, rdef})->set(loc());
                    case  NE: return w.call(core::icmp::  ne, Defs{ldef, rdef})->set(loc());
                    case AND: return w.call(core::bit2::and_, 0, Defs{ldef, rdef})->set(loc());
                    case  OR: return w.call(core::bit2:: or_, 0, Defs{ldef, rdef})->set(loc());
                    case XOR: return w.call(core::bit2::xor_, 0, Defs{ldef, rdef})->set(loc());
                    default: fe::unreachable();
                }
            } else {
                auto mode = type2wmode(lhs()->type());
                bool s = is_signed(lhs()->type());

                if (mim::match<mem::Ptr>(ldef->type())) ldef = w.call<core::bitcast>(w.type_int(64), ldef);
                if (mim::match<mem::Ptr>(rdef->type())) rdef = w.call<core::bitcast>(w.type_int(64), rdef);

                switch (op) {
                    case  LT: return w.call(s ? core::icmp::  sl : core::icmp::  ul, Defs{ldef, rdef})->set(loc());
                    case  LE: return w.call(s ? core::icmp:: sle : core::icmp:: ule, Defs{ldef, rdef})->set(loc());
                    case  GT: return w.call(s ? core::icmp::  sg : core::icmp::  ug, Defs{ldef, rdef})->set(loc());
                    case  GE: return w.call(s ? core::icmp:: sge : core::icmp:: uge, Defs{ldef, rdef})->set(loc());
                    case SHR: return w.call(s ? core::shr::a : core::shr::l, Defs{ldef, rdef})->set(loc());
                    case  EQ: return w.call(core::icmp::   e, Defs{ldef, rdef})->set(loc());
                    case  NE: return w.call(core::icmp::  ne, Defs{ldef, rdef})->set(loc());
                    case  OR: return w.call(core::bit2:: or_, 0, Defs{ldef, rdef})->set(loc());
                    case XOR: return w.call(core::bit2::xor_, 0, Defs{ldef, rdef})->set(loc());
                    case AND: return w.call(core::bit2::and_, 0, Defs{ldef, rdef})->set(loc());
                    case ADD: return w.call(core::wrap:: add, mode, Defs{ldef, rdef})->set(loc());
                    case SUB: return w.call(core::wrap:: sub, mode, Defs{ldef, rdef})->set(loc());
                    case MUL: return w.call(core::wrap:: mul, mode, Defs{ldef, rdef})->set(loc());
                    case SHL: return w.call(core::wrap:: shl, mode, Defs{ldef, rdef})->set(loc());
                    case DIV: return cg.handle_mem_res(w.call(s ? core::div::sdiv : core::div::udiv, Defs{cg.cur_mem, w.tuple({ldef, rdef})})->set(loc()));
                    case REM: return cg.handle_mem_res(w.call(s ? core::div::srem : core::div::urem, Defs{cg.cur_mem, w.tuple({ldef, rdef})})->set(loc()));
                    default: fe::unreachable();
                }
            }
        }
    }
  // clang-format on
}

const Def *PostfixExpr::remit(CodeGen &cg) const {
  auto var = lhs()->lemit(cg);
  auto res = cg.load(var)->set(loc());
  auto one = cg.lit_one(type())->set(loc());
  const Def *val = nullptr;

  if (is_int(type()))
    val = cg.world
              .call(tag() == INC ? core::wrap::add : core::wrap::sub,
                    type2wmode(type()), Defs{res, one})
              ->set(loc());
  else
    val = cg.world
              .call(tag() == INC ? math::arith::add : math::arith::sub,
                    math::Mode::none, Defs{res, one})
              ->set(loc());
  cg.store(var, val)->set(loc());
  return res;
}

const Def *DefiniteArrayExpr::remit(CodeGen &cg) const {
  Array<const Def *> thorin_args(num_args());
  for (size_t i = 0, e = num_args(); i != e; ++i)
    thorin_args[i] = arg(i)->remit(cg);
  return cg.world.tuple(thorin_args)->set(loc());
}

const Def *RepeatedDefiniteArrayExpr::remit(CodeGen &cg) const {
  return cg.world.pack(count(), value()->remit(cg));
}

const Def *TupleExpr::remit(CodeGen &cg) const {
  Array<const Def *> thorin_args(num_args());
  for (size_t i = 0, e = num_args(); i != e; ++i)
    thorin_args[i] = arg(i)->remit(cg);
  return cg.world.tuple(thorin_args)->set(loc());
}

const Def *IndefiniteArrayExpr::remit(CodeGen &cg) const {
  auto dim_int = cg.world.call(core::conv::u, /*2^64*/ 0, dim()->remit(cg));
  auto arity = cg.world.call<core::bitcast>(cg.world.type_nat(), dim_int);
  auto elem = cg.convert(type()->as<IndefiniteArrayType>()->elem_type());
  return cg.world.pack(arity, cg.world.bot(elem))->set(loc());
}

const Def *StructExpr::remit(CodeGen &cg) const {
  Array<const Def *> defs(num_elems());
  for (auto &&elem : elems())
    defs[elem->field_decl()->index()] = elem->expr()->remit(cg);
  return cg.world.tuple(cg.convert(type())->as<mim::Sigma>(), defs)->set(loc());
}

const Def *TypeAppExpr::lemit(CodeGen &) const { fe::unreachable(); }
const Def *TypeAppExpr::remit(CodeGen & /*cg*/) const { fe::unreachable(); }

const Def *MapExpr::lemit(CodeGen &cg) const {
  auto agg = lhs()->lemit(cg);
  return mem::op_lea_unsafe(agg, arg(0)->remit(cg))->set(loc());
}

const Def *MapExpr::remit(CodeGen &cg) const {
  auto ltype = unpack_ref_type(lhs()->type());
  auto &w = cg.world;

  if (auto cn = ltype->isa<FnType>()) {
    const Def *dst = nullptr;

    // Handle primops here
    if (auto type_expr =
            lhs()->isa<TypeAppExpr>()) { // Bitcast, sizeof and select are all
                                         // polymorphic
      auto callee = type_expr->lhs()->skip_rvalue();
      if (auto path = callee->isa<PathExpr>()) {
        if (auto fn_decl = path->value_decl()->isa<FnDecl>()) {
          if (fn_decl->is_extern() && fn_decl->abi() == "\"thorin\"") {
            auto name = fn_decl->fn_symbol().remove_quotation();
            if (name == "bitcast") {
              return w
                  .call<core::bitcast>(cg.convert(type_expr->type_arg(0)),
                                       arg(0)->remit(cg))
                  ->set(loc());
            } else if (name == "select") {
              return w
                  .extract(w.tuple({arg(2)->remit(cg), arg(1)->remit(cg)}),
                           arg(0)->remit(cg))
                  ->set(loc());
            } else if (name == "insert") {
              return core::insert_unsafe(arg(0)->remit(cg), arg(1)->remit(cg),
                                         arg(2)->remit(cg))
                  ->set(loc());
            } else if (name == "alignof") {
              return w
                  .call<core::bitcast>(
                      w.type_int(32),
                      w.call(core::trait::align,
                             cg.convert(type_expr->type_arg(0))))
                  ->set(loc());
            } else if (name == "sizeof") {
              return w
                  .call<core::bitcast>(
                      w.type_int(32),
                      w.call(core::trait::size,
                             cg.convert(type_expr->type_arg(0))))
                  ->set(loc());
            } else if (name == "undef") {
              return w.bot(cg.convert(type_expr->type_arg(0)))->set(loc());
            } else if (name == "reserve_shared") {
              auto ptr = cg.convert(type());
              auto cn = w.cn({w.annex<mem::M>(), w.type_int(32),
                              w.cn({w.annex<mem::M>(), ptr})});
              auto cont = w.mut_lam(cn)->set(loc(), "reserve_shared");
              // cont->set_intrinsic();
              dst = cont;
            } else if (name == "atomic") {
              auto poly_type = cg.convert(type());
              auto ptr = cg.convert(arg(1)->type());
              auto cn = w.cn({w.annex<mem::M>(), w.type_int(32), ptr, poly_type,
                              w.cn({w.annex<mem::M>(), poly_type})});
              auto cont = w.mut_lam(cn)->set(loc(), "atomic");
              // cont->set_intrinsic();
              dst = cont;
            } else if (name == "cmpxchg") {
              auto ptr = mim::force<mem::Ptr>(cg.convert(arg(0)->type()));
              auto [pointee, addr_space] = ptr->args<2>();
              auto poly_type = pointee;
              auto cn =
                  w.cn({w.annex<mem::M>(), ptr, poly_type, poly_type,
                        w.cn({w.annex<mem::M>(), poly_type, w.type_bool()})});
              auto cont = w.mut_lam(cn)->set(loc(), "cmpxchg");
              // cont->set_intrinsic();
              dst = cont;
            } else if (name == "pe_info") {
              auto poly_type = cg.convert(arg(1)->type());
              auto string_type =
                  cg.world.call<mem::Ptr0>(w.arr_unsafe(w.type_int(8)));
              auto cn = w.cn({w.annex<mem::M>(), string_type, poly_type,
                              w.cn(w.annex<mem::M>())});
              auto cont = w.mut_lam(cn)->set(loc(), "pe_info");
              // cont->set_intrinsic();
              dst = cont;
            } else if (name == "pe_known") {
              auto poly_type = cg.convert(arg(0)->type());
              auto cn = w.cn({w.annex<mem::M>(), poly_type,
                              w.cn({w.annex<mem::M>(), w.type_bool()})});
              auto cont = w.mut_lam(cn)->set(loc(), "pe_known");
              // cont->set_intrinsic();
              dst = cont;
            }
          }
        }
      }
    }

    dst = dst ? dst : lhs()->remit(cg);

    std::vector<const Def *> defs;
    defs.push_back(nullptr); // reserve for mem but set later - some other args
                             // may update mem
    for (auto &&arg : args())
      defs.push_back(arg.get()->remit(cg));
    defs.front() = cg.cur_mem; // now get the current memory value

    auto ret_type = num_args() == cn->num_params()
                        ? nullptr
                        : cg.convert(cn->return_type());
    const Def *ret;
    std::tie(cg.cur_bb, ret) = cg.call(dst, defs, ret_type);
    cg.cur_bb->set(loc(), dst->sym().str() + "_cont");
    if (ret_type)
      cg.cur_mem = cg.cur_bb->var(0_s);

    return ret;
  } else if (ltype->isa<ArrayType>() || ltype->isa<TupleType>()) {
    auto index = arg(0)->remit(cg);
    return core::extract_unsafe(lhs()->remit(cg), index)->set(loc());
  }
  fe::unreachable();
}

const Def *FieldExpr::lemit(CodeGen &cg) const {
  auto value = lhs()->lemit(cg);
  return mem::op_lea_unsafe(value, index())->set(loc());
}

const Def *FieldExpr::remit(CodeGen &cg) const {
  auto tup = lhs()->remit(cg);
  return cg.world.extract(tup, Lit::as(tup->arity()), index())->set(loc());
}

const Def *BlockExpr::remit(CodeGen &cg) const {
  for (auto &&stmt : stmts()) {
    if (auto item_stmnt = stmt->isa<ItemStmt>())
      item_stmnt->item()->emit_head(cg);
  }

  for (auto &&stmt : stmts())
    stmt->emit(cg);

  return expr()->remit(cg);
}

const Def *IfExpr::remit(CodeGen &cg) const {
  auto thorin_type = cg.convert(type());

  auto jump_type = cg.world.cn(cg.world.annex<mem::M>());
  auto if_then =
      cg.world.mut_lam(jump_type)->set(then_expr()->loc().begin, "if_then");
  auto if_else =
      cg.world.mut_lam(jump_type)->set(else_expr()->loc().begin, "if_else");
  auto if_join = thorin_type
                     ? cg.basicblock(thorin_type)->set(loc().finis, "if_join")
                     : nullptr; // TODO rewrite with bottom type

  cond()->emit_branch(cg, if_then, if_else);

  cg.enter(if_then);
  if (auto tdef = then_expr()->remit(cg))
    cg.cur_bb->app(false, if_join, {cg.cur_mem, tdef})->set(loc().finis);

  cg.enter(if_else);
  if (auto fdef = else_expr()->remit(cg))
    cg.cur_bb->app(false, if_join, {cg.cur_mem, fdef})->set(loc().finis);

  if (thorin_type)
    return cg.enter(if_join)->var(1);
  return nullptr; // TODO use bottom type
}

const Def *MatchExpr::remit(CodeGen & /*cg*/) const {
#if 0
    auto thorin_type = cg.convert(type());

    auto join = thorin_type ? cg.basicblock(thorin_type)->set(cg.loc("match_join", loc().finis)) : nullptr; // TODO rewrite with bottom type

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
                    defs[i] = cg.world.lit_idx(64, option_decl->index())->set(cg.loc(arm(i)->ptrn()->loc()));
                }
                targets[i] = cg.basicblock(cg.loc2dbg("case", arm(i)->loc().front()));
            }
        }

        targets.shrink(num_targets);
        defs.shrink(num_targets);

        auto matcher_int = is_integer ? matcher : cg.world.extract(matcher, 0_u32, matcher->debug());
        cg.cur_bb->match(matcher_int, otherwise, defs, targets)->set(cg.loc("match", loc().front()));

        for (size_t i = 0; i != num_targets; ++i) {
            cg.enter(targets[i]);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(false, join, {cg.cur_mem, def})->set(cg.loc(loc().finis));
        }

        bool no_otherwise = num_arms() == num_targets;
        if (!no_otherwise) {
            cg.enter(otherwise);
            if (auto def = arm(num_targets)->expr()->remit(cg))
                cg.cur_bb->app(false, join, {cg.cur_mem, def})->set(cg.loc(loc().finis));
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

            cg.cur_bb->branch(false, cond, case_t, case_f, cg.cur_mem)->set(cg.loc(arm(i)->ptrn()->loc().finis));

            cg.enter(case_t);
            if (auto def = arm(i)->expr()->remit(cg))
                cg.cur_bb->app(false, join, {cg.cur_mem, def})->set(cg.loc(arm(i)->loc().finis));

            cg.enter(case_f);
        }
    }

    if (thorin_type)
        return cg.enter(join)->var(1);
#endif
  return nullptr; // TODO use bottom type
}

const Def *WhileExpr::remit(CodeGen &cg) const {
  auto head_bb = cg.world.mut_lam(cg.world.cn(cg.world.annex<mem::M>()))
                     ->set(loc().begin, "while_head");
  head_bb->var(0)->set("mem");

  auto jump_type = cg.world.cn(cg.world.annex<mem::M>());
  auto body_bb =
      cg.world.mut_lam(jump_type)->set(body()->loc().begin, "while_body");
  auto exit_bb =
      cg.world.mut_lam(jump_type)->set(body()->loc().finis, "while_exit");
  auto cont_bb = cg.create_lam(continue_decl());
  auto brk__bb = cg.create_lam(break_decl());

  cg.cur_bb->app(false, head_bb, {cg.cur_mem})->set(cond()->loc().finis);

  cg.enter(head_bb);
  cond()->emit_branch(cg, body_bb, exit_bb);

  cg.enter(body_bb);
  body()->remit(cg);
  cg.cur_bb->app(false, cont_bb, {cg.cur_mem})->set(body()->loc().finis);

  cg.enter(cont_bb);
  cg.cur_bb->app(false, head_bb, {cg.cur_mem})->set(body()->loc().finis);

  cg.enter(exit_bb);
  cg.cur_bb->app(false, brk__bb, {cg.cur_mem})->set(body()->loc().finis);

  cg.enter(brk__bb);
  return cg.world.tuple();
}

const Def *ForExpr::remit(CodeGen &cg) const {
  std::vector<const Def *> args;
  args.push_back(nullptr); // reserve for mem but set later - some other args
                           // may update the monad

  auto break_bb = cg.create_lam(break_decl());

  // emit call
  auto map_expr = expr()->as<MapExpr>();
  for (auto &&arg : map_expr->args())
    args.push_back(arg.get()->remit(cg));
  args.push_back(fn_expr()->remit(cg));
  args.push_back(break_bb);
  auto fun = map_expr->lhs()->remit(cg);

  args.front() = cg.cur_mem; // now get the current memory monad
  cg.call(fun, args, nullptr).first->set(map_expr->loc());

  cg.enter(break_bb);
  if (break_bb->num_vars() == 2)
    return break_bb->var(1);
  else {
    Array<const Def *> args(break_bb->num_vars() - 1);
    for (size_t i = 0, e = args.size(); i != e; ++i)
      args[i] = break_bb->var(i + 1);
    return cg.world.tuple(args)->set(loc());
  }
}

const Def *FnExpr::remit(CodeGen &cg) const {
  auto lam = fn_emit_head(cg, loc());
  fn_emit_body(cg, loc());
  return lam;
}

const Def *RevDiffExpr::remit(CodeGen &cg) const {
  return nullptr;
  // return cg.rev_diff(expr()->remit(cg));
}

/*
 * patterns
 */

void IdPtrn::emit(CodeGen &cg, const mim::Def *init) const {
  local()->emit(cg, init);
}

const mim::Def *IdPtrn::emit_cond(CodeGen &cg, const mim::Def *) const {
  return cg.world.lit_tt();
}

void EnumPtrn::emit(CodeGen &cg, const mim::Def *init) const {
  if (num_args() == 0)
    return;
  auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
  auto variant = cg.world
                     .call<core::bitcast>(variant_type,
                                          cg.world.extract(init, num_args(), 1))
                     ->set(loc());
  for (size_t i = 0, e = num_args(); i != e; ++i) {
    arg(i)->emit(cg,
                 num_args() == 1
                     ? variant
                     : cg.world.extract(variant, num_args(), i)->set(loc()));
  }
}

const mim::Def *EnumPtrn::emit_cond(CodeGen &cg, const mim::Def *init) const {
  auto index = path()->decl()->as<OptionDecl>()->index();
  auto init_0 = cg.world.extract(init, num_args(), 0_u32)->set(loc());
  auto cond =
      cg.world.call(core::icmp::e, Defs{init_0, cg.world.lit_idx(u32(index))})
          ->set(loc());
  if (num_args() > 0) {
    auto variant_type = path()->decl()->as<OptionDecl>()->variant_type(cg);
    auto variant =
        cg.world
            .call<core::bitcast>(
                variant_type, cg.world.extract(init, num_args(), 1)->set(loc()))
            ->set(loc());
    for (size_t i = 0, e = num_args(); i != e; ++i) {
      if (!arg(i)->is_refutable())
        continue;
      auto arg_cond = arg(i)->emit_cond(
          cg, num_args() == 1
                  ? variant
                  : cg.world.extract(variant, num_args(), i)->set(loc()));
      cond =
          cg.world.call(core::bit2::and_, 0, Defs{cond, arg_cond})->set(loc());
    }
  }
  return cond;
}

void TuplePtrn::emit(CodeGen &cg, const mim::Def *init) const {
  for (size_t i = 0, e = num_elems(); i != e; ++i)
    elem(i)->emit(cg, cg.world.extract(init, e, i)->set(loc()));
}

const mim::Def *TuplePtrn::emit_cond(CodeGen &cg, const mim::Def *init) const {
  const Def *cond = nullptr;
  for (size_t i = 0, e = num_elems(); i != e; ++i) {
    if (!elem(i)->is_refutable())
      continue;

    auto next = elem(i)->emit_cond(
        cg, cg.world.extract(init, num_elems(), i)->set(loc()));
    cond = cond ? cg.world.call(core::bit2::and_, 0, Defs{cond, next}) : next;
  }
  return cond ? cond : cg.world.lit_tt();
}

const mim::Def *LiteralPtrn::emit(CodeGen &cg) const {
  auto def = literal()->remit(cg);
  if (has_minus()) {
    if (is_float(type()))
      return cg.world.call<math::minus>(0_n, def)->set(def->dbg());
    else
      return cg.world.call<core::minus>(type2wmode(type()), def)
          ->set(def->dbg());
  } else {
    return def;
  }
}

void LiteralPtrn::emit(CodeGen &, const mim::Def *) const {}

const mim::Def *LiteralPtrn::emit_cond(CodeGen &cg,
                                       const mim::Def *init) const {
  return cg.world.call(core::icmp::e, Defs{init, emit(cg)});
}

const mim::Def *CharPtrn::emit(CodeGen &cg) const { return chr()->remit(cg); }

void CharPtrn::emit(CodeGen &, const mim::Def *) const {}

const mim::Def *CharPtrn::emit_cond(CodeGen &cg, const mim::Def *init) const {
  return cg.world.call(core::icmp::e, Defs{init, emit(cg)});
}

/*
 * statements
 */

void ExprStmt::emit(CodeGen &cg) const { expr()->remit(cg); }
void ItemStmt::emit(CodeGen &cg) const { item()->emit(cg); }

void LetStmt::emit(CodeGen &cg) const {
  ptrn()->emit(
      cg, init()
              ? init()->remit(cg)
              : cg.world.bot(cg.convert(ptrn()->type()))->set(ptrn()->loc()));
}

void AsmStmt::emit(CodeGen & /*cg*/) const {
  /*
  Array<const mim::Def*> outs(num_outputs());
  for (size_t i = 0, e = num_outputs(); i != e; ++i)
      outs[i] = cg.convert(output(i)->expr()->type()->as<RefType>()->pointee());

  Array<const Def*> ins(num_inputs());
  for (size_t i = 0, e = num_inputs(); i != e; ++i)
      ins[i] = input(i)->expr()->remit(cg);

  mim::Assembly::Flags flags = mim::Assembly::Flags::NoFlag;
  for (auto&& option : options()) {
      if (option == "volatile")
          flags |= mim::Assembly::Flags::HasSideEffects;
      else if (option == "alignstack")
          flags |= mim::Assembly::Flags::IsAlignStack;
      else if (option == "intel")
          flags |= mim::Assembly::Flags::IsIntelDialect;
  }

  auto assembly = cg.world.assembly(outs, cg.cur_mem, ins, asm_template(),
          output_constraints(), input_constraints(), clobbers(),
  flags)->set(loc());

  size_t i = 0;
  cg.cur_mem = assembly->out(i++);
  for (auto&& output: outputs())
      cg.store(output->expr()->lemit(cg), assembly->out(i++), loc());
  */
}

//------------------------------------------------------------------------------

void emit(World &world, const Module *mod) {
  CodeGen cg(world);
  mod->emit(cg);
}

//------------------------------------------------------------------------------

} // namespace impala
