#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"

#include "impala/symbol.h"

namespace impala {

enum Tag {
#define IMPALA_TYPE(itype, atype) Tag_##itype,
#include "impala/tokenlist.h"
    Tag_app,
    Tag_borrowed_ptr,
    Tag_definite_array,
    Tag_error,
    Tag_fn,
    Tag_impl,
    Tag_indefinite_array,
    Tag_lambda,
    Tag_lvalue,
    Tag_noret,
    Tag_owned_ptr,
    Tag_pi,
    Tag_simd,
    Tag_struct,
    Tag_tuple,
    Tag_typedef_abs,
    Tag_unknown,
    Tag_var,
};

enum PrimTypeTag {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Tag_##itype,
#include "impala/tokenlist.h"
};

class StructDecl;
template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

static const int Node_App        = impala::Tag_app;
static const int Node_Lambda     = impala::Tag_lambda;
static const int Node_Pi         = impala::Tag_pi;
static const int Node_StructType = impala::Tag_struct;
static const int Node_TupleType  = impala::Tag_tuple;
static const int Node_TypeError  = impala::Tag_error;
static const int Node_Var        = impala::Tag_var;

#define HENK_STRUCT_EXTRA_NAME  struct_decl
#define HENK_STRUCT_EXTRA_TYPE  const StructDecl*
#define HENK_TABLE_NAME  typetable
#define HENK_TABLE_TYPE  TypeTable
#include "thorin/henk.h"

//------------------------------------------------------------------------------

/// Primitive type.
class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, PrimTypeTag tag)
        : Type(typetable, (Tag) tag, {})
    {}

public:
    PrimTypeTag primtype_tag() const { return (PrimTypeTag) tag(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

bool is(const Type*, PrimTypeTag tag);
#define IMPALA_TYPE(itype, atype) inline bool is_##itype(const Type* t) { return is(t, PrimType_##itype); }
#include "impala/tokenlist.h"
inline bool is_float(const Type* t) { return             is_f16(t) || is_f32(t) || is_f64(t); }
inline bool is_int  (const Type* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t)
                                          || is_u8(t) || is_u16(t) || is_u32(t) || is_u64(t); }
bool is_void(const Type*);
bool is_subtype(const Type* dst, const Type* src);
bool is_strict_subtype(const Type* dst, const Type* src);

//------------------------------------------------------------------------------

/// Common base Type for PtrType%s and LValueType.
class RefType : public Type {
protected:
    RefType(TypeTable& typetable, int tag, const Type* pointee, bool mut, int addr_space)
        : Type(typetable, tag, {pointee})
        , mut_(mut)
        , addr_space_(addr_space)
    {}

public:
    const Type* pointee() const { return op(0); }
    bool is_mut() const { return mut_; }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual uint64_t vhash() const override;
    virtual bool equal(const Type* other) const override;
    virtual std::string prefix() const = 0;

private:
    bool mut_;
    int addr_space_;

    friend class TypeTable;
};

/// Pointer @p Type.
class PtrType : public RefType {
protected:
    PtrType(TypeTable& typetable, int tag, const Type* pointee, bool mut, int addr_space)
        : RefType(typetable, tag, {pointee}, mut, addr_space)
    {}

    std::ostream& stream_ptr_type(std::ostream&, std::string prefix, int addr_space, const Type* ref_type) const;

private:
    int addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& typetable, const Type* pointee, bool mut, int addr_space)
        : PtrType(typetable, Tag_borrowed_ptr, pointee, mut, addr_space)
    {}

    virtual std::string prefix() const override { return is_mut() ? "&mut " : "&"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& typetable, const Type* pointee, int addr_space)
        : PtrType(typetable, Tag_owned_ptr, pointee, true, addr_space)
    {}

    virtual std::string prefix() const override { return "~"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;
};

class LValueType : public RefType {
protected:
    LValueType(TypeTable& typetable, const Type* pointee, int addr_space)
        : RefType(typetable, Tag_lvalue, {pointee}, true, addr_space)
    {}

public:
    virtual std::string prefix() const override { return "lvalue of "; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class FnType : public Type {
private:
    FnType(TypeTable& typetable, Types ops)
        : Type(typetable, Tag_fn, ops)
    {
        ++order_;
    }

public:
    const Type* return_type() const;
    bool is_returning() const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

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

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Tag_definite_array, elem_type)
        , dim_(dim)
    {}

    uint64_t dim() const { return dim_; }
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<DefiniteArrayType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

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
    virtual uint64_t vhash() const override { return thorin::hash_combine(Type::vhash(), dim()); }
    virtual bool equal(const Type* other) const override {
        return Type::equal(other) && this->dim() == other->as<SimdType>()->dim();
    }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& typetable)
        : Type(typetable, Tag_noret, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

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
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual bool equal(const Type*) const override;
    virtual uint64_t vhash() const override { return this->gid(); }
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vreduce(int, const Type*, Type2Type&) const override;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

}

#endif
