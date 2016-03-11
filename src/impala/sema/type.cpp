#include "impala/sema/type.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stack>

#include "impala/ast.h"
#include "impala/sema/typetable.h"

namespace impala {

using thorin::hash_begin;
using thorin::hash_combine;

//------------------------------------------------------------------------------

/*
 * constructors
 */

size_t Type::gid_counter_ = 1;

StructAbsType::StructAbsType(TypeTable& typetable, const StructDecl* struct_decl)
    : Type(typetable, Kind_struct_abs, Array<const Type*>(struct_decl->num_field_decls()))
    , struct_decl_(struct_decl)
{}

//StructAppType::StructAppType(TypeTable& typetable, StructAbsType struct_abs_type, ArrayRef<Type> args)
    //: KnownType(typetable, Kind_struct_app, args)
    //, struct_abs_type_(struct_abs_type.unify())
    //, elem_cache_(struct_abs_type->num_args())
//{}

//------------------------------------------------------------------------------

const Type* Type::close(ArrayRef<const TypeParam*> type_params) const {
    assert(num_type_params() == type_params.size());

    for (size_t i = 0, e = num_type_params(); i != e; ++i) {
        assert(!type_params[i]->is_closed());
        type_params_[i] = type_params[i];
        type_params_[i]->binder_ = this;
        type_params_[i]->closed_ = true;
        type_params_[i]->index_ = i;
    }

    std::stack<const Type*> stack;
    TypeSet done;

    auto push = [&](const Type* type) {
        if (!type->is_closed() && !done.contains(type) && !type->isa<TypeParam>()) {
            done.insert(type);
            stack.push(type);
            return true;
        }
        return false;
    };

    push(this);

    // TODO this is potentially quadratic when closing n types
    while (!stack.empty()) {
        auto type = stack.top();

        bool todo = false;
        for (auto arg : type->args())
            todo |= push(arg);

        if (!todo) {
            stack.pop();
            type->closed_ = true;
            for (size_t i = 0, e = type->num_args(); i != e && type->closed_; ++i)
                type->closed_ &= type->arg(i)->is_closed();
        }
    }

    return typetable().unify_base(this);
}

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
            if (fn->num_args() == 1)
                return fn->args().front();
            return typetable().tuple_type(fn->args());
        }
    }
    return typetable().type_noret();
}

static Type2Type type2type(const Type* type, Types args) {
    assert(type->num_type_params() == args.size());
    Type2Type map;
    for (size_t i = 0, e = args.size(); i != e; ++i)
        map[type->type_param(i)] = args[i];
    assert(map.size() == args.size());
    return map;
}

const Type* StructAppType::elem(size_t i) const {
    if (auto type = elem_cache_[i])
        return type;

    assert(i < struct_abs_type()->num_args());
    auto type = struct_abs_type()->arg(i);
    auto map = type2type(struct_abs_type(), type_args());
    return elem_cache_[i] = type->specialize(map);
}

Types StructAppType::elems() const {
    for (size_t i = 0; i < num_elems(); ++i)
        elem(i);
    return elem_cache_;
}

//------------------------------------------------------------------------------

/*
 * hash
 */

uint64_t Type::vhash() const {
    uint64_t seed = hash_combine(hash_combine(hash_begin((int) kind()), num_args()), num_type_params());
    for (auto arg : args_)
        seed = hash_combine(seed, arg->hash());
    return seed;
}

uint64_t PtrType::vhash() const {
    return hash_combine(Type::vhash(), (uint64_t)addr_space());
}

uint64_t TypeParam::vhash() const {
    auto seed = hash_combine(hash_combine(hash_begin(int(kind())), index()), int(binder()->kind()));
    return hash_combine(hash_combine(seed, binder()->num_type_params()), binder()->num_args());
}

//------------------------------------------------------------------------------

/*
 * equal
 */

bool Type::equal(const Type* other) const {
    bool result =  this->kind() == other->kind()     &&  this->is_monomorphic() == other->is_monomorphic()
            && this->num_args() == other->num_args() && this->num_type_params() == other->num_type_params();

    if (result) {
        if (is_monomorphic()) {
            for (size_t i = 0, e = num_args(); result && i != e; ++i)
                result &= this->args_[i] == other->args_[i];
        } else {
            for (size_t i = 0, e = num_type_params(); result && i != e; ++i) {
                assert(this->type_param(i)->equiv_ == nullptr);
                this->type_param(i)->equiv_ = other->type_param(i);
            }

            for (size_t i = 0, e = num_args(); result && i != e; ++i)
                result &= this->args_[i]->equal(other->args_[i]);

            for (auto type_param : type_params())
                type_param->equiv_ = nullptr;
        }
    }

    return result;
}

bool PtrType::equal(const Type* other) const {
    if (!Type::equal(other))
        return false;
    auto ptr = other->as<PtrType>();
    return ptr->addr_space() == addr_space();
}

bool TypeParam::equal(const Type* other) const {
    if (auto type_param = other->isa<TypeParam>())
        return this->equiv_ == type_param;
    return false;
}

bool UnknownType::equal(const Type* other) const { return this == other; }

//------------------------------------------------------------------------------

/*
 * stream
 */

std::ostream& Type::stream_type_params(std::ostream& os) const {
    if (num_type_params() == 0)
        return os;

    return streamf(os, "[%]", stream_list(type_params(), [&](const TypeParam* type_param) {
        if (type_param)
            os << type_param;
        else
            os << "<null>";
    }));
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
    stream_type_params(os << "fn");
    auto ret_type = return_type();
    if (ret_type->isa<NoRetType>())
        return stream_list(os, args(), [&](const Type* type) { os << type; }, "(", ")");

    return streamf(os, "(%) -> %", stream_list(args().skip_back(), [&](const Type* type) { os << type; }), ret_type);
}

std::ostream& TypeParam::stream(std::ostream& os) const { return os << symbol().str(); }

std::ostream& PtrType::stream(std::ostream& os) const {
    os << prefix();
    if (addr_space() != 0)
        os << '[' << addr_space() << ']';
    return os << referenced_type();
}

std::ostream& DefiniteArrayType::stream(std::ostream& os) const { return streamf(os, "[% * %]", elem_type(), dim()); }
std::ostream& IndefiniteArrayType::stream(std::ostream& os) const { return streamf(os, "[%]", elem_type()); }
std::ostream& SimdType::stream(std::ostream& os) const { return streamf(os, "simd[% * %]", elem_type(), dim()); }
std::ostream& StructAbsType::stream(std::ostream& os) const { return os << struct_decl_->symbol(); }

std::ostream& StructAppType::stream(std::ostream& os) const {
    os << struct_abs_type()->struct_decl()->symbol();
    if (num_args() != 0)
        return stream_list(os, args(), [&](const Type* type) { os << type; }, "[", "]");
    return os;
}

//std::ostream& TypedefAbsNode::stream(std::ostream& os) const {
    //assert(num_type_params() > 0); // otherwise no TypedefAbsNode should have been used in the first place
    //return stream_type_params(os << "type") << " = " << type();
//}

std::ostream& TupleType::stream(std::ostream& os) const {
    return stream_list(stream_type_params(os), args(), [&](const Type* type) { os << type; }, "(", ")");
}

//------------------------------------------------------------------------------

/*
 * rebuild
 */

const Type* Type::rebuild(Types args) const {
    assert(num_args() == args.size());
    if (args.empty())
        return this;
    return vrebuild(args);
}

const Type* PrimType           ::vrebuild(Types     ) const { return typetable().prim_type(primtype_kind()); }
const Type* FnType             ::vrebuild(Types args) const { return typetable().   fn_type(args); }
const Type* TupleType          ::vrebuild(Types args) const { return typetable().tuple_type(args); }
const Type* DefiniteArrayType  ::vrebuild(Types args) const { return typetable().  definite_array_type(args[0], dim()); }
const Type* SimdType           ::vrebuild(Types args) const { return typetable().            simd_type(args[0], dim()); }
const Type* IndefiniteArrayType::vrebuild(Types args) const { return typetable().indefinite_array_type(args[0]); }
const Type* BorrowedPtrType    ::vrebuild(Types args) const { return typetable().borrowed_ptr_type(args[0], addr_space()); }
const Type* MutPtrType         ::vrebuild(Types args) const { return typetable().     mut_ptr_type(args[0], addr_space()); }
const Type* OwnedPtrType       ::vrebuild(Types args) const { return typetable().   owned_ptr_type(args[0], addr_space()); }
const Type* TypeParam          ::vrebuild(Types     ) const { return this; }
const Type* NoRetType          ::vrebuild(Types     ) const { return this; }
const Type* TypeError          ::vrebuild(Types     ) const { return this; }
const Type* UnknownType        ::vrebuild(Types     ) const { return this; }

const Type* StructAbsType::vrebuild(Types /*args*/) const {
    //// TODO how do we handle recursive types?
    //auto ntype = typetable().struct_abs_type(args.size());
    //for (size_t i = 0, e = args.size(); i != e; ++i)
        //ntype->set(i, args[i]);
    //return ntype;
    return nullptr;
}

const Type* StructAppType::vrebuild(Types args) const {
    return typetable().struct_app_type(args[0]->as<StructAbsType>(), args.skip_front());
}


//------------------------------------------------------------------------------

/*
 * specialize and instantiate
 */

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
        auto ntype_param = typetable().type_param(type_param(i)->symbol());
        map[type_param(i)] = ntype_param;
        ntype_params[i] = ntype_param;
    }

    return instantiate(map)->close(ntype_params);;
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

//------------------------------------------------------------------------------

}
