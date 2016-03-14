#include "impala/sema/type.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stack>

#include "impala/ast.h"
#include "impala/sema/typetable.h"


namespace impala {

#define HENK_STRUCT_UNIFIER_NAME struct_decl
#define HENK_TABLE_TYPE TypeTable
#define HENK_TABLE_NAME typetable
#include "thorin/henk.cpp.h"

//------------------------------------------------------------------------------

bool is(const Type* type, PrimTypeKind kind) {
    return type->isa<PrimType>() && type->as<PrimType>()->primtype_kind() == kind;
}

bool FnType::is_returning() const {
    bool ret = false;
    for (auto arg : args()) {
        switch (arg->order()) {
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
        if (auto fn = args().back()->isa<FnType>()) {
            if (fn->size() == 1)
                return fn->args().front();
            return typetable().tuple_type(fn->args());
        }
    }
    return typetable().type_noret();
}

#if 0
static Type2Type type2type(const Type* type, Types args) {
    assert(type->num_type_params() == args.size());
    Type2Type map;
    for (size_t i = 0, e = args.size(); i != e; ++i)
        map[type->type_param(i)] = args[i];
    assert(map.size() == args.size());
    return map;
}
#endif

//------------------------------------------------------------------------------

/*
 * hash
 */

uint64_t PtrType::vhash() const {
    return thorin::hash_combine(Type::vhash(), (uint64_t)addr_space());
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool PtrType::equal(const Type* other) const {
    if (!Type::equal(other))
        return false;
    auto ptr = other->as<PtrType>();
    return ptr->addr_space() == addr_space();
}

bool UnknownType::equal(const Type* other) const { return this == other; }

//------------------------------------------------------------------------------

/*
 * stream
 */

const Type* stream_type_params(std::ostream& os, const Type* type) {
    std::vector<const TypeParam*> type_params;
    while (auto type_abs = type->isa<TypeAbs>()) {
        type_params.push_back(type_abs->type_param());
        type = type_abs->body();
    }

    streamf(os, "[%]", stream_list(type_params, [&](const TypeParam* type_param) {
        if (type_param)
            os << type_param;
        else
            os << "<null>";
    }));

    return type;
}

std::ostream& UnknownType::stream(std::ostream& os) const { return os << '?' << gid(); }

std::ostream& PrimType::stream(std::ostream& os) const {
    switch (primtype_kind()) {
#define IMPALA_TYPE(itype, atype) case PrimType_##itype: return os << #itype;
#include "impala/tokenlist.h"
        default: THORIN_UNREACHABLE;
    }
}

std::ostream& TypeError::stream(std::ostream& os) const { return os << "<type error>"; }
std::ostream& NoRetType::stream(std::ostream& os) const { return os << "<no-return>"; }

std::ostream& FnType::stream(std::ostream& os) const {
    stream_type_params(os << "fn", this);
    auto ret_type = return_type();
    if (ret_type->isa<NoRetType>())
        return stream_list(os, args(), [&](const Type* type) { os << type; }, "(", ")");

    return streamf(os, "(%) -> %", stream_list(args().skip_back(), [&](const Type* type) { os << type; }), ret_type);
}

std::ostream& TypeParam::stream(std::ostream& os) const { return os << name(); }

std::ostream& PtrType::stream(std::ostream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << referenced_type();
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
    return stream_list(os, stream_type_params(os, this)->args(), [&](const Type* type) { os << type; }, "(", ")");
}

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Type* Type::rebuild(TypeTable& to, Types args) const {
    assert(size() == args.size());
    if (args.empty())
        return this;
    return vrebuild(to, args);
}

const Type* PrimType           ::vrebuild(TypeTable& to, Types     ) const { return to.prim_type(primtype_kind()); }
const Type* FnType             ::vrebuild(TypeTable& to, Types args) const { return to.   fn_type(args); }
const Type* TupleType          ::vrebuild(TypeTable& to, Types args) const { return to.tuple_type(args); }
const Type* DefiniteArrayType  ::vrebuild(TypeTable& to, Types args) const { return to.  definite_array_type(args[0], dim()); }
const Type* SimdType           ::vrebuild(TypeTable& to, Types args) const { return to.            simd_type(args[0], dim()); }
const Type* IndefiniteArrayType::vrebuild(TypeTable& to, Types args) const { return to.indefinite_array_type(args[0]); }
const Type* BorrowedPtrType    ::vrebuild(TypeTable& to, Types args) const { return to.borrowed_ptr_type(args[0], addr_space()); }
const Type* MutPtrType         ::vrebuild(TypeTable& to, Types args) const { return to.     mut_ptr_type(args[0], addr_space()); }
const Type* OwnedPtrType       ::vrebuild(TypeTable& to, Types args) const { return to.   owned_ptr_type(args[0], addr_space()); }
const Type* TypeParam          ::vrebuild(TypeTable& to, Types     ) const { return this; }
const Type* NoRetType          ::vrebuild(TypeTable& to, Types     ) const { return this; }
const Type* TypeError          ::vrebuild(TypeTable& to, Types     ) const { return this; }
const Type* UnknownType        ::vrebuild(TypeTable& to, Types     ) const { return this; }

//------------------------------------------------------------------------------

/*
 * specialize and instantiate
 */

#if 0

const Type* Type::instantiate(Types types) const {
    assert(types.size() == num_type_params());
    Type2Type map;
    for (size_t i = 0, e = types.size(); i != e; ++i)
        map[type_param(i)] = types[i];
    return instantiate(map);
}

const Type* Type::instantiate(Type2Type& map) const {
#ifndef NDEBUG
    for (auto type_param : type_params())
        assert(map.contains(type_param));
#endif
    return vinstantiate(map);
}

const Type* Type::specialize(Type2Type& map) const {
    if (auto result = find(map, this))
        return result;

    Array<const TypeParam*> ntype_params(num_type_params());
    for (size_t i = 0, e = num_type_params(); i != e; ++i) {
        assert(!map.contains(type_param(i)));
        auto ntype_param = typetable().type_param(type_param(i)->name());
        map[type_param(i)] = ntype_param;
        ntype_params[i] = ntype_param;
    }

    auto open = instantiate(map);
    return close(open, ntype_params);
}

Array<const Type*> Type::specialize_args(Type2Type& map) const {
    Array<const Type*> result(num_args());
    for (size_t i = 0, e = num_args(); i != e; ++i)
        result[i] = arg(i)->specialize(map);
    return result;
}

const Type* DefiniteArrayType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().definite_array_type(elem_type()->specialize(map), dim());
}

const Type* SimdType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().simd_type(elem_type()->specialize(map), dim());
}

const Type* IndefiniteArrayType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().indefinite_array_type(elem_type()->specialize(map));
}

const Type* BorrowedPtrType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().borrowed_ptr_type(referenced_type()->specialize(map), addr_space());
}

const Type* MutPtrType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().mut_ptr_type(referenced_type()->specialize(map), addr_space());
}

const Type* OwnedPtrType::vinstantiate(Type2Type& map) const {
    return map[this] = typetable().owned_ptr_type(referenced_type()->specialize(map), addr_space());
}

const Type* StructAbsType::instantiate(Types args) const { return typetable().struct_app_type(this, args); }

const Type* StructAppType::vinstantiate(Type2Type& map) const {
    Array<const Type*> nargs(num_type_args());
    for (size_t i = 0, e = num_type_args(); i != e; ++i)
        nargs[i] = type_arg(i)->specialize(map);
    return map[this] = typetable().struct_app_type(struct_abs_type(), nargs);
}

const Type* TupleType  ::vinstantiate(Type2Type& map) const { return map[this] = typetable().tuple_type(specialize_args(map)); }
const Type* FnType     ::vinstantiate(Type2Type& map) const { return map[this] = typetable().fn_type(specialize_args(map)); }
const Type* PrimType   ::vinstantiate(Type2Type& map) const { return map[this] = this; }
const Type* TypeParam  ::vinstantiate(Type2Type& map) const { return map[this] = this; }
const Type* NoRetType  ::vinstantiate(Type2Type& map) const { return map[this] = this; }
const Type* TypeError  ::vinstantiate(Type2Type& map) const { return map[this] = this; }
const Type* UnknownType::vinstantiate(Type2Type&    ) const { THORIN_UNREACHABLE; return nullptr; }

#endif

//------------------------------------------------------------------------------

}
