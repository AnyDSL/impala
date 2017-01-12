#include "impala/sema/type.h"

#include <algorithm>
#include <sstream>
#include <stack>

#include "impala/ast.h"
#include "impala/sema/typetable.h"

namespace impala {

using thorin::streamf;

#define HENK_STRUCT_EXTRA_NAME struct_decl
#define HENK_STRUCT_EXTRA_TYPE  const StructDecl*
#define HENK_TABLE_TYPE TypeTable
#define HENK_TABLE_NAME typetable
#include "thorin/henk.cpp.h"

//------------------------------------------------------------------------------

bool is(const Type* type, PrimTypeTag tag) {
    return type->isa<PrimType>() && type->as<PrimType>()->primtype_tag() == tag;
}

bool is_void(const Type* type) {
    if (auto t = type->isa<TupleType>())
        return t->empty();
    return false;
}

bool FnType::is_returning() const {
    bool ret = false;
    for (auto op : ops()) {
        switch (op->order()) {
            case 0: continue;
            case 1:
                if (!ret) {
                    ret = true;
                    continue;
                } // else fall-through
            default:
                return false;
        }
    }
    return true;
}

const Type* FnType::return_type() const {
    if (!empty()) {
        if (auto fn = ops().back()->isa<FnType>()) {
            if (fn->num_ops() == 1)
                return fn->ops().front();
            return typetable().tuple_type(fn->ops());
        }
    }
    return typetable().type_noret();
}

bool is_subtype(const Type* dst, const Type* src) {
    assert(dst->is_known() && src->is_known());

    if (dst == src)
        return true;

    if (auto dst_borrowed_ptr_type = dst->isa<BorrowedPtrType>()) {
        if (auto src_owned_ptr_type = src->isa<OwnedPtrType>())
            return src_owned_ptr_type->addr_space() == dst_borrowed_ptr_type->addr_space()
                && is_subtype(dst_borrowed_ptr_type->pointee(), src_owned_ptr_type->pointee());
    }

    if (auto dst_indefinite_array_type = dst->isa<IndefiniteArrayType>()) {
        if (auto src_definite_array_type = src->isa<DefiniteArrayType>())
            return is_subtype(dst_indefinite_array_type->elem_type(), src_definite_array_type->elem_type());
    }

    if (dst->tag() == src->tag() && dst->num_ops() == src->num_ops()) {
        bool result = true;
        for (size_t i = 0, e = dst->num_ops(); result && i != e; ++i)
            result &= is_subtype(dst->op(i), src->op(i));

        // Special case for DefiniteArrays, SimdTypes and PtrTypes
        if (auto dst_def_array = dst->isa<DefiniteArrayType>()) {
            result &= src->as<DefiniteArrayType>()->dim() == dst_def_array->dim();
        }

        if (auto dst_simd_type = dst->isa<SimdType>()) {
            result &= src->as<SimdType>()->dim() == dst_simd_type->dim();
        }

        if (auto dst_ptr_type = dst->isa<PtrType>()) {
            result &= src->as<PtrType>()->addr_space() == dst_ptr_type->addr_space();
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

uint64_t RefType::vhash() const {
    return thorin::hash_combine(Type::vhash(), (uint64_t)addr_space());
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool RefType::equal(const Type* other) const {
    if (!Type::equal(other))
        return false;
    auto ptr = other->as<PtrType>();
    return ptr->addr_space() == addr_space();
}

bool UnknownType::equal(const Type* other) const { return this == other; }

//------------------------------------------------------------------------------

/*
 * prefix
 */

//------------------------------------------------------------------------------

/*
 * stream
 */

std::ostream& Lambda::stream(std::ostream& os) const { return streamf(os, "[%].%", name(), body()); }
std::ostream& UnknownType::stream(std::ostream& os) const { return os << '?' << gid(); }

std::ostream& PrimType::stream(std::ostream& os) const {
    switch (primtype_tag()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& TypeError::stream(std::ostream& os) const { return os << "<type error>"; }
std::ostream& NoRetType::stream(std::ostream& os) const { return os << "<no-return>"; }

std::ostream& FnType::stream(std::ostream& os) const {
    os << "fn";
    //auto ret_type = return_type();
    //if (ret_type->isa<NoRetType>())
        return stream_list(os, ops(), [&](const Type* type) { os << type; }, "(", ")");

    //return streamf(os, "(%) -> %", stream_list(ops().skip_back(), [&](const Type* type) { os << type; }), ret_type);
}

std::ostream& Var::stream(std::ostream& os) const {
    return streamf(os, "<%>", depth());
}

std::ostream& App::stream(std::ostream& os) const { return streamf(os, "%[%]", callee(), arg()); }

std::ostream& RefType::stream(std::ostream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << pointee();
}

std::ostream& DefiniteArrayType::stream(std::ostream& os) const { return streamf(os, "[% * %]", elem_type(), dim()); }
std::ostream& IndefiniteArrayType::stream(std::ostream& os) const { return streamf(os, "[%]", elem_type()); }
std::ostream& SimdType::stream(std::ostream& os) const { return streamf(os, "simd[% * %]", elem_type(), dim()); }
std::ostream& StructType::stream(std::ostream& os) const { return os << struct_decl()->symbol(); }

//std::ostream& TypedefAbsNode::stream(std::ostream& os) const {
    //assert(num_type_params() > 0); // otherwise no TypedefAbsNode should have been used in the first place
    //return stream_type_params(os << "type") << " = " << type();
//}

std::ostream& TupleType::stream(std::ostream& os) const {
    return stream_list(os, ops(), [&](const Type* type) { os << type; }, "(", ")");
}

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Type* PrimType           ::vrebuild(TypeTable& to, Types    ) const { return to.prim_type(primtype_tag()); }
const Type* FnType             ::vrebuild(TypeTable& to, Types ops) const { return to.fn_type(ops); }
const Type* DefiniteArrayType  ::vrebuild(TypeTable& to, Types ops) const { return to.  definite_array_type(ops[0], dim()); }
const Type* SimdType           ::vrebuild(TypeTable& to, Types ops) const { return to.            simd_type(ops[0], dim()); }
const Type* IndefiniteArrayType::vrebuild(TypeTable& to, Types ops) const { return to.indefinite_array_type(ops[0]); }
const Type* BorrowedPtrType    ::vrebuild(TypeTable& to, Types ops) const { return to.borrowed_ptr_type(ops[0], addr_space()); }
const Type* MutPtrType         ::vrebuild(TypeTable& to, Types ops) const { return to.     mut_ptr_type(ops[0], addr_space()); }
const Type* OwnedPtrType       ::vrebuild(TypeTable& to, Types ops) const { return to.   owned_ptr_type(ops[0], addr_space()); }
const Type* LValueType         ::vrebuild(TypeTable& to, Types ops) const { return to.      lvalue_type(ops[0], addr_space()); }
const Type* NoRetType          ::vrebuild(TypeTable&,    Types    ) const { return this; }
const Type* UnknownType        ::vrebuild(TypeTable&,    Types    ) const { return this; }

//------------------------------------------------------------------------------

/*
 * reduce
 */

const Type* DefiniteArrayType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().definite_array_type(elem_type()->reduce(depth, type, map), dim());
}

const Type* SimdType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().simd_type(elem_type()->reduce(depth, type, map), dim());
}

const Type* IndefiniteArrayType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().indefinite_array_type(elem_type()->reduce(depth, type, map));
}

const Type* BorrowedPtrType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().borrowed_ptr_type(pointee()->reduce(depth, type, map), addr_space());
}

const Type* MutPtrType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().mut_ptr_type(pointee()->reduce(depth, type, map), addr_space());
}

const Type* OwnedPtrType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().owned_ptr_type(pointee()->reduce(depth, type, map), addr_space());
}

const Type* LValueType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().lvalue_type(pointee()->reduce(depth, type, map), addr_space());
}

const Type* FnType::vreduce(int depth, const Type* type, Type2Type& map) const {
    return typetable().fn_type(reduce_ops(depth, type, map));
}

const Type* PrimType   ::vreduce(int, const Type*, Type2Type&) const { return this; }
const Type* NoRetType  ::vreduce(int, const Type*, Type2Type&) const { return this; }
const Type* UnknownType::vreduce(int, const Type*, Type2Type&) const { return this; }

//------------------------------------------------------------------------------

}
