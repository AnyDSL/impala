#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/def.h"
#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"

#include "impala/symbol.h"
#include "impala/sema/type_table.h"

namespace impala {

using thorin::Stream;

enum Tag {
#define IMPALA_TYPE(itype, atype) Tag_##itype,
#include "impala/tokenlist.h"
    Tag_app,
    Tag_borrowed_ptr,
    Tag_definite_array,
    Tag_error,
    Tag_fn,
    Tag_impl,
    Tag_infer_error,
    Tag_indefinite_array,
    Tag_lambda,
    Tag_noret,
    Tag_owned_ptr,
    Tag_pi,
    Tag_ref,
    Tag_simd,
    Tag_struct,
    Tag_enum,
    Tag_tuple,
    Tag_typedef_abs,
    Tag_unknown,
    Tag_var,
};

enum PrimTypeTag {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Tag_##itype,
#include "impala/tokenlist.h"
};

//------------------------------------------------------------------------------

class TypeTable;
using Type = thorin::TypeBase<TypeTable>;

class StructDecl;
class EnumDecl;
template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

template<class T>
using TypeMap   = thorin::GIDMap<const Type*, T>;
using TypeSet   = thorin::GIDSet<const Type*>;
using Type2Type = TypeMap<const Type*>;
using Types     = ArrayRef<const Type*>;

//------------------------------------------------------------------------------

/// Primitive type.
class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, PrimTypeTag tag)
        : Type(typetable, (Tag) tag, {})
    {}

public:
    PrimTypeTag primtype_tag() const { return (PrimTypeTag) tag(); }

    Stream& stream(Stream&) const override;

    virtual const Type* tangent_vector() const override;

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

bool is(const Type*, PrimTypeTag tag);
#define IMPALA_TYPE(itype, atype) inline bool is_##itype(const Type* t) { return is(t, PrimType_##itype); }
#include "impala/tokenlist.h"
inline bool is_float (const Type* t) { return             is_f16(t) || is_f32(t) || is_f64(t); }
inline bool is_int   (const Type* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t)
                                           || is_u8(t) || is_u16(t) || is_u32(t) || is_u64(t); }
inline bool is_signed(const Type* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t); }
bool is_void(const Type*);
bool is_subtype(const Type* dst, const Type* src);
bool is_strict_subtype(const Type* dst, const Type* src);

//------------------------------------------------------------------------------

/// Common base Type for PtrType%s and RefType.
class RefTypeBase : public Type {
protected:
    RefTypeBase(TypeTable& typetable, int tag, const Type* pointee, bool mut, uint64_t addr_space)
        : Type(typetable, tag, {pointee})
        , mut_(mut)
        , addr_space_(addr_space)
    {}

public:
    const Type* pointee() const { return op(0); }
    bool is_mut() const { return mut_; }
    uint64_t addr_space() const { return addr_space_; }

    Stream& stream(Stream&) const override;
    uint32_t vhash() const override;
    bool equal(const Type* other) const override;
    virtual std::string prefix() const = 0;

private:
    bool mut_;
    uint64_t addr_space_;

    friend class TypeTable;
};

/// Pointer @p Type.
class PtrType : public RefTypeBase {
protected:
    PtrType(TypeTable& typetable, int tag, const Type* pointee, bool mut, uint64_t addr_space)
        : RefTypeBase(typetable, tag, pointee, mut, addr_space)
    {}

    Stream& stream_ptr_type(Stream&, std::string prefix, uint64_t addr_space, const Type* ref_type) const;

private:
    uint64_t addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& typetable, const Type* pointee, bool mut, uint64_t addr_space)
        : PtrType(typetable, Tag_borrowed_ptr, pointee, mut, addr_space)
    {}

    std::string prefix() const override { return is_mut() ? "&mut " : "&"; }

private:
    const Type* vrebuild(TypeTable&, Types) const override;
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& typetable, const Type* pointee, uint64_t addr_space)
        : PtrType(typetable, Tag_owned_ptr, pointee, true, addr_space)
    {}

    std::string prefix() const override { return "~"; }

private:
    const Type* vrebuild(TypeTable&, Types) const override;
};

class RefType : public RefTypeBase {
protected:
    RefType(TypeTable& typetable, const Type* pointee, bool mut, uint64_t addr_space)
        : RefTypeBase(typetable, Tag_ref, pointee, mut, addr_space)
    {}

public:
    std::string prefix() const override { return is_mut() ? "lvalue of " : "reference of "; }

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

inline const RefType* is_lvalue(const Type* type) {
    if (auto ref = type->isa<RefType>()) {
        if (ref->is_mut())
            return ref;
    }
    return nullptr;
}

inline const Type* unpack_ref_type(const Type* type) {
    return type->isa<RefType>() ? type->as<RefType>()->pointee() : type;
}

inline bool is_ptr(const Type* t) {
    return t->isa<PtrType>() || (t->isa<RefType>() && t->as<RefType>()->pointee()->isa<PtrType>());
}

inline const RefType* split_ref_type(const Type*& type) {
    auto ref = type->isa<RefType>();
    type = ref ? ref->pointee() : type;
    return ref;
}

//------------------------------------------------------------------------------

class FnType : public Type {
private:
    FnType(TypeTable& typetable, const Type* op)
        : Type(typetable, Tag_fn, {op})
    {
        ++order_;
    }

public:
    const Type* domain() const { return op(0); }
    const Type* param(size_t) const;
    size_t num_params() const;
    const Type* last_param() const;
    const Type* return_type() const;
    bool is_returning() const;
    Stream& stream(Stream&) const override;

    const Type* rev_diffed_type() const;
private:
    const Type* vrebuild(TypeTable&, Types) const override;

    const Type* params_without_return_continuation() const;
    const Type* grad_return_type() const;
    const Type* grad_with_val_return_type() const;
    const Type* pullback_return_type() const;
    const Type* pullback_with_val_return_type() const;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class Lambda : public Type {
private:
    Lambda(TypeTable& table, const Type* body, const char* name)
        : Type(table, Tag_lambda, {body})
        , name_(name)
    {}

public:
    const char* name() const { return name_; }
    const Type* body() const { return op(0); }
    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable& to, Types ops) const override;
    const Type* vreduce(int, const Type*, Type2Type&) const override;

    const char* name_;

    friend class TypeTable;
};

class Var : public Type {
private:
    Var(TypeTable& table, int depth)
        : Type(table, Tag_var, {})
        , depth_(depth)
    {
        monomorphic_ = false;
    }

public:
    int depth() const { return depth_; }
    Stream& stream(Stream&) const override;

private:
    uint32_t vhash() const override;
    bool equal(const Type*) const override;
    const Type* vrebuild(TypeTable& to, Types ops) const override;
    const Type* vreduce(int, const Type*, Type2Type&) const override;

    int depth_;

    friend class TypeTable;
};

class App : public Type {
private:
    App(TypeTable& table, const Type* callee, const Type* arg)
        : Type(table, Tag_app, {callee, arg})
    {}

public:
    const Type* callee() const { return Type::op(0); }
    const Type* arg() const { return Type::op(1); }
    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable& to, Types ops) const override;

    mutable const Type* cache_ = nullptr;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class TupleType : public Type {
private:
    TupleType(TypeTable& table, Types ops)
        : Type(table, Tag_tuple, ops)
    {}

public:
    const Type* vrebuild(TypeTable& to, Types ops) const override;
    Stream& stream(Stream&) const override;

    virtual const Type* tangent_vector() const override;

    friend class TypeTable;
};

class StructType : public Type {
private:
    StructType(TypeTable& table, const StructDecl* decl, size_t size)
        : Type(table, Tag_struct, thorin::Array<const Type*>(size))
        , decl_(decl)
    {
        nominal_ = true;
    }

public:
    const StructDecl* struct_decl() const { return decl_; }
    void set(size_t i, const Type* type) const { return const_cast<StructType*>(this)->Type::set(i, type); }

    virtual const Type* tangent_vector() const override;

private:
    const Type* vrebuild(TypeTable& to, Types ops) const override;
    const Type* vreduce(int, const Type*, Type2Type&) const override;
    Stream& stream(Stream&) const override;

    const StructDecl* decl_;

    friend class TypeTable;
};

class EnumType : public Type {
private:
    EnumType(TypeTable& table, const EnumDecl* decl, size_t size)
        : Type(table, Tag_enum, thorin::Array<const Type*>(size))
        , decl_(decl)
    {
        nominal_ = true;
    }

public:
    const EnumDecl* enum_decl() const { return decl_; }
    void set(size_t i, const Type* type) const { return const_cast<EnumType*>(this)->Type::set(i, type); }

private:
    const Type* vrebuild(TypeTable& to, Types ops) const override;
    const Type* vreduce(int, const Type*, Type2Type&) const override;
    Stream& stream(Stream&) const override;

    const EnumDecl* decl_;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, int tag, const Type* elem_type)
        : Type(typetable, tag, {elem_type})
    {}

public:
    const Type* elem_type() const { return op(0); }
};

class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(TypeTable& typetable, const Type* elem_type)
        : ArrayType(typetable, Tag_indefinite_array, elem_type)
    {}

    Stream& stream(Stream&) const override;

    virtual const Type* tangent_vector() const override;
private:
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Tag_definite_array, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    uint32_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<DefiniteArrayType>()->dim();
    }

    Stream& stream(Stream&) const override;

    virtual const Type* tangent_vector() const override;

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class SimdType : public ArrayType {
public:
    SimdType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Tag_simd, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    uint32_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<SimdType>()->dim();
    }

    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& typetable)
        : Type(typetable, Tag_noret, {})
    {}

public:
    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

class UnknownType : public Type {
private:
    UnknownType(TypeTable& typetable)
        : Type(typetable, Tag_unknown, {})
    {
        known_ = false;
    }

public:
    Stream& stream(Stream&) const override;

private:
    bool equal(const Type*) const override;
    uint32_t vhash() const override { return this->gid(); }
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& table)
        : Type(table, Tag_error, {})
    {}

public:
    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable& to, Types ops) const override;

    friend class TypeTable;
};

class InferError : public Type {
    InferError(TypeTable& typetable, const Type* dst, const Type* src)
        : Type(typetable, Tag_infer_error, {dst, src})
    {}

public:
    const Type* dst() const { return op(0); }
    const Type* src() const { return op(1); }
    Stream& stream(Stream&) const override;

private:
    const Type* vrebuild(TypeTable&, Types) const override;

    friend class TypeTable;
};

inline bool is_no_ret_or_type_error(const Type* t) {
    return t->isa<NoRetType>() || t->isa<TypeError>();
}

inline bool is_unit(const Type* t) {
    return t->isa<TupleType>() && t->num_ops() == 0;
}

//------------------------------------------------------------------------------

class TypeTable : public thorin::TypeTableBase<Type> {
public:
    TypeTable();

    const Var* var(int depth) { return unify(new Var(*this, depth)); }
    const Type* app(const Type* callee, const Type* op);
    const Lambda* lambda(const Type* body, const char* name) { return unify(new Lambda(*this, body, name)); }

    const TupleType* tuple_type(Types ops) { assert(ops.size() != 1); return unify(new TupleType(*this, ops)); }
    const TupleType* unit() { return unit_; }

    const StructType* struct_type(const StructDecl* decl, size_t size);
    const EnumType* enum_type(const EnumDecl* decl, size_t size);

#define IMPALA_TYPE(itype, atype) const PrimType* type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    const DefiniteArrayType* definite_array_type(const Type* elem_type, uint64_t dim) {
        return unify(new DefiniteArrayType(*this, elem_type, dim));
    }
    const FnType* fn_type(const Type* op) { return unify(new FnType(*this, op)); }
    const FnType* fn_type(Types params) { return unify(new FnType(*this, params.size() == 1 ? params.front() : tuple_type(params))); }
    const IndefiniteArrayType* indefinite_array_type(const Type* elem_type) {
        return unify(new IndefiniteArrayType(*this, elem_type));
    }
    const SimdType* simd_type(const Type* elem_type, uint64_t size) { return unify(new SimdType(*this, elem_type, size)); }
    const BorrowedPtrType* borrowed_ptr_type(const Type* pointee, bool mut, uint64_t addr_space) {
        return unify(new BorrowedPtrType(*this, pointee, mut, addr_space));
    }
    const OwnedPtrType* owned_ptr_type(const Type* pointee, uint64_t addr_space) {
        return unify(new OwnedPtrType(*this, pointee, addr_space));
    }
    const RefType* ref_type(const Type* pointee, bool mut, uint64_t addr_space) {
        return unify(new RefType(*this, pointee, mut, addr_space));
    }
    const NoRetType* type_noret() { return type_noret_; }
    const PrimType* prim_type(PrimTypeTag tag);
    const UnknownType* unknown_type() { return unify(new UnknownType(*this)); }
    const TypeError* type_error() { return type_error_; }
    const InferError* infer_error(const Type* dst, const Type* src);

private:
    const TupleType* unit_;
    const NoRetType* type_noret_;
    const TypeError* type_error_;
#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
};

}

#endif
