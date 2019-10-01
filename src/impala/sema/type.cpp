#include "impala/sema/type.h"

#include <algorithm>
#include <sstream>
#include <stack>

#include "impala/ast.h"

namespace impala {

using thorin::Stream;

//------------------------------------------------------------------------------

bool is(const Type* type, PrimTypeTag tag) {
    return type->isa<PrimType>() && type->as<PrimType>()->primtype_tag() == tag;
}

bool is_void(const Type* type) {
    if (auto t = type->isa<TupleType>())
        return t->empty();
    return false;
}

const Type* FnType::param(size_t i) const {
    return op(0)->isa<TupleType>() ? op(0)->op(i) : op(i);
}

size_t FnType::num_params() const {
    return op(0)->isa<TupleType>() ? op(0)->num_ops() : 1;
}

const Type* FnType::last_param() const {
    return op(0)->isa<TupleType>() ? op(0)->ops().back() : op(0);
}


bool FnType::is_returning() const {
    if (auto tuple = op(0)->isa<TupleType>()) {
        if (tuple->num_ops() == 0)
            return false;
        if (auto fn_type = tuple->ops().back()->isa<FnType>())
            return !fn_type->is_returning();
        return false;
    } else {
        return op(0)->isa<FnType>() && !op(0)->as<FnType>()->is_returning();
    }
}

const Type* FnType::return_type() const {
    auto last_param = op(0);
    if (last_param->isa<TupleType>()) {
         if (last_param->empty())
            return table().type_noret();
        last_param = last_param->ops().back();
    }
    if (auto fn = last_param->isa<FnType>())
        return fn->op(0);
    return table().type_noret();
}

bool is_subtype(const Type* dst, const Type* src) {
    if (dst == src)
        return true;

    if (dst->isa<StructType>() || dst->isa<EnumType>())
        // structs and enums are the only nominal types
        return false;

    if (auto dst_borrowed_ptr_type = dst->isa<BorrowedPtrType>()) {
        if (auto src_owned_ptr_type = src->isa<OwnedPtrType>()) {
            return src_owned_ptr_type->addr_space() == dst_borrowed_ptr_type->addr_space()
                && is_subtype(dst_borrowed_ptr_type->pointee(), src_owned_ptr_type->pointee());
        } else if (auto src_borrowed_ptr_type = src->isa<BorrowedPtrType>()) {
            return src_borrowed_ptr_type->addr_space() == dst_borrowed_ptr_type->addr_space()
                && (src_borrowed_ptr_type->is_mut() || !dst_borrowed_ptr_type->is_mut())
                && is_subtype(dst_borrowed_ptr_type->pointee(), src_borrowed_ptr_type->pointee());
        }
    } else if (auto dst_indefinite_array_type = dst->isa<IndefiniteArrayType>()) {
        if (auto src_definite_array_type = src->isa<DefiniteArrayType>())
            return is_subtype(dst_indefinite_array_type->elem_type(), src_definite_array_type->elem_type());
    }

    if (dst->tag() == src->tag() && dst->num_ops() == src->num_ops()) {
        bool result = true;

        // special cases for DefiniteArrays, SimdTypes and PtrTypes
        if (auto dst_def_array = dst->isa<DefiniteArrayType>())
            result &= src->as<DefiniteArrayType>()->dim() == dst_def_array->dim();
        else if (auto dst_simd_type = dst->isa<SimdType>())
            result &= src->as<SimdType>()->dim() == dst_simd_type->dim();
        else if (auto dst_ref_type = dst->isa<RefTypeBase>())
            result &=  src->as<RefTypeBase>()->is_mut() == dst_ref_type->is_mut()
                    && src->as<RefTypeBase>()->addr_space() == dst_ref_type->addr_space();

        if (auto dst_fn = dst->isa<FnType>()) {
            auto src_fn = src->as<FnType>();
            auto ret = dst_fn->return_type();
            size_t nparams = dst_fn->num_params();
            if (!ret->isa<NoRetType>()) {
                result &= is_subtype(ret, src_fn->return_type());
                nparams--;
            }
            result &= is_subtype(src_fn->op(0), dst_fn->op(0));
        } else {
            for (size_t i = 0, e = dst->num_ops(); result && i != e; ++i)
                result &= is_subtype(dst->op(i), src->op(i));
        }

        return result;
    }

    return false;
}

bool is_strict_subtype(const Type* dst, const Type* src) {
    return dst != src && is_subtype(dst, src);
}

//------------------------------------------------------------------------------

/*
 * hash
 */

uint32_t RefTypeBase::vhash() const {
    return thorin::hash_combine(Type::vhash(), ((uint32_t)addr_space() << uint32_t(1)) | uint32_t(is_mut()));
}

uint32_t Var::vhash() const {
    return thorin::murmur3(uint32_t(tag()) << uint32_t(24) | uint32_t(depth()));
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool RefTypeBase::equal(const Type* other) const {
    return Type::equal(other)
        && this->is_mut() == other->as<RefTypeBase>()->is_mut()
        && this->addr_space() == other->as<RefTypeBase>()->addr_space();
}

bool Var::equal(const Type* other) const {
    return other->isa<Var>() ? this->as<Var>()->depth() == other->as<Var>()->depth() : false;
}

bool UnknownType::equal(const Type* other) const { return this == other; }

//------------------------------------------------------------------------------

/*
 * stream
 */

Stream& Lambda::stream(Stream& os) const { return os.fmt("[{}].{}", name(), body()); }
Stream& UnknownType::stream(Stream& os) const { return os << '?' << gid(); }

Stream& PrimType::stream(Stream& os) const {
    switch (primtype_tag()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

Stream& NoRetType::stream(Stream& os) const { return os << "<no-return>"; }
Stream& InferError::stream(Stream& os) const { return os.fmt("<infer error: {}, {}>", dst(), src()); }
Stream& TypeError::stream(Stream& os) const { return os << "<type error>"; }

Stream& FnType::stream(Stream& os) const {
    os << "fn";
    if (auto tuple = op(0)->isa<TupleType>())
        os.fmt("{}", tuple);
    else
        os.fmt("({})", op(0));
    auto ret_type = return_type();
    return !ret_type->isa<NoRetType>() ? os.fmt(" -> {}", ret_type) : os;
}

Stream& Var::stream(Stream& os) const {
    return os.fmt("<{}>", depth());
}

Stream& App::stream(Stream& os) const { return os.fmt("{}[{}]", callee(), arg()); }

Stream& RefTypeBase::stream(Stream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << pointee();
}

Stream& DefiniteArrayType::stream(Stream& os) const { return os.fmt("[{} * {}]", elem_type(), dim()); }
Stream& IndefiniteArrayType::stream(Stream& os) const { return os.fmt("[{}]", elem_type()); }
Stream& SimdType::stream(Stream& os) const { return os.fmt("simd[{} * {}]", elem_type(), dim()); }
Stream& StructType::stream(Stream& os) const { return os << struct_decl()->symbol(); }
Stream& EnumType::stream(Stream& os) const { return os << enum_decl()->symbol(); }
Stream& TupleType::stream(Stream& os) const { return os.list(ops(), [&](auto t) { os << t; }, "(", ")"); }

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Type* PrimType           ::vrebuild(TypeTable& to, Types    ) const { return to.prim_type(primtype_tag()); }
const Type* FnType             ::vrebuild(TypeTable& to, Types ops) const { return to.fn_type(ops); }
const Type* App                ::vrebuild(TypeTable& to, Types ops) const { return to.app(ops[0], ops[1]); }
const Type* Lambda             ::vrebuild(TypeTable& to, Types ops) const { return to.lambda(ops[0], name()); }
const Type* Var                ::vrebuild(TypeTable& to, Types    ) const { return to.var(depth()); }
const Type* TupleType          ::vrebuild(TypeTable& to, Types ops) const { return to.tuple_type(ops); }
const Type* StructType         ::vrebuild(TypeTable&   , Types    ) const { return this; }
const Type* EnumType           ::vrebuild(TypeTable&   , Types    ) const { return this; }
const Type* DefiniteArrayType  ::vrebuild(TypeTable& to, Types ops) const { return to.  definite_array_type(ops[0], dim()); }
const Type* SimdType           ::vrebuild(TypeTable& to, Types ops) const { return to.            simd_type(ops[0], dim()); }
const Type* IndefiniteArrayType::vrebuild(TypeTable& to, Types ops) const { return to.indefinite_array_type(ops[0]); }
const Type* BorrowedPtrType    ::vrebuild(TypeTable& to, Types ops) const { return to.borrowed_ptr_type(ops[0], is_mut(), addr_space()); }
const Type* OwnedPtrType       ::vrebuild(TypeTable& to, Types ops) const { return to.   owned_ptr_type(ops[0], addr_space()); }
const Type* RefType            ::vrebuild(TypeTable& to, Types ops) const { return to.      ref_type(ops[0], is_mut(), addr_space()); }
const Type* InferError         ::vrebuild(TypeTable& to, Types ops) const { return to.infer_error(ops[0], ops[1]); }
const Type* NoRetType          ::vrebuild(TypeTable&,    Types    ) const { return this; }
const Type* UnknownType        ::vrebuild(TypeTable&,    Types    ) const { return this; }
const Type* TypeError          ::vrebuild(TypeTable&,    Types    ) const { return this; }

//------------------------------------------------------------------------------

/*
 * reduce
 */

const Type* Lambda::vreduce(int depth, const Type* type, Type2Type& map) const {
    return table().lambda(body()->reduce(depth+1, type, map), name());
}

const Type* Var::vreduce(int depth, const Type* type, Type2Type&) const {
    if (this->depth() == depth)
        return type;
    else if (this->depth() > depth)
        return table().var(this->depth()-1);  // this is a free variable - shift by one
    else
        return this;                          // this variable is not free - don't adjust
}

const Type* StructType::vreduce(int depth, const Type* type, Type2Type& map) const {
    auto struct_type = table().struct_type(struct_decl(), num_ops());
    map[this] = struct_type;
    for (size_t i = 0, e = num_ops(); i != e; ++i)
        struct_type->set(i, op(i)->reduce(depth, type, map));
    return struct_type;
}

const Type* EnumType::vreduce(int depth, const Type* type, Type2Type& map) const {
    auto enum_type = table().enum_type(enum_decl(), num_ops());
    map[this] = enum_type;
    for (size_t i = 0, e = num_ops(); i != e; ++i)
        enum_type->set(i, op(i)->reduce(depth, type, map));
    return enum_type;
}

//------------------------------------------------------------------------------

TypeTable::TypeTable()
    : unit_(unify(new TupleType(*this, {})))
    , type_noret_(unify(new NoRetType(*this)))
    , type_error_(unify(new TypeError(*this)))
#define IMPALA_TYPE(itype, atype) , itype##_(unify(new PrimType(*this, PrimType_##itype)))
#include "impala/tokenlist.h"
{}

const Type* TypeTable::app(const Type* callee, const Type* op) {
    auto app = unify(new App(*this, callee, op));

    if (auto cache = app->cache_)
        return cache;
    if (auto lambda = app->callee()->isa<Lambda>()) {
        Type2Type map;
        return app->cache_ = lambda->body()->reduce(1, op, map);
    } else {
        return app->cache_ = app;
    }

    return app;
}

const StructType* TypeTable::struct_type(const StructDecl* decl, size_t size) {
    auto type = new StructType(*this, decl, size);
    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    return type;
}

const EnumType* TypeTable::enum_type(const EnumDecl* decl, size_t size) {
    auto type = new EnumType(*this, decl, size);
    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    return type;
}

const PrimType* TypeTable::prim_type(const PrimTypeTag tag) {
    switch (tag) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return itype##_;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

const InferError* TypeTable::infer_error(const Type* dst, const Type* src) {
    if (auto di = dst->isa<InferError>()) {
        if (di->src() == src)
            return di;
    }

    if (auto si = src->isa<InferError>()) {
        if (si->dst() == dst)
            return si;
    }

    return unify(new InferError(*this, dst, src));
}

}
