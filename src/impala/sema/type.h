#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"

#include "impala/symbol.h"

namespace impala {

enum Kind {
#define IMPALA_TYPE(itype, atype) Kind_##itype,
#include "impala/tokenlist.h"
    Kind_borrowed_ptr,
    Kind_definite_array,
    Kind_error,
    Kind_fn,
    Kind_impl,
    Kind_indefinite_array,
    Kind_mut_ptr,
    Kind_noret,
    Kind_owned_ptr,
    Kind_simd,
    Kind_struct,
    Kind_tuple,
    Kind_lambda,
    Kind_type_param,
    Kind_typedef_abs,
    Kind_unknown,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Kind_##itype,
#include "impala/tokenlist.h"
};

class StructDecl;
template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

static const int Node_StructType = impala::Kind_struct;
static const int Node_Lambda     = impala::Kind_lambda;
static const int Node_TypeParam  = impala::Kind_type_param;
static const int Node_TupleType  = impala::Kind_tuple;

#define HENK_STRUCT_UNIFIER_NAME  struct_decl
#define HENK_STRUCT_UNIFIER_TYPE  const StructDecl*
#define HENK_TABLE_NAME  typetable
#define HENK_TABLE_TYPE  TypeTable
#include "thorin/henk.h"

//------------------------------------------------------------------------------

/// Primitive type.
class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, PrimTypeKind kind)
        : Type(typetable, (Kind) kind, {})
    {}

public:
    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;

    friend class TypeTable;
};

bool is(const Type*, PrimTypeKind kind);
#define IMPALA_TYPE(itype, atype) inline bool is_##itype(const Type* t) { return is(t, PrimType_##itype); }
#include "impala/tokenlist.h"
inline bool is_float(const Type* t) { return             is_f16(t) || is_f32(t) || is_f64(t); }
inline bool is_int  (const Type* t) { return is_i8(t) || is_i16(t) || is_i32(t) || is_i64(t)
                                        || is_u8(t) || is_u16(t) || is_u32(t) || is_u64(t); }

//------------------------------------------------------------------------------

class PtrType : public Type {
protected:
    PtrType(TypeTable& typetable, int kind, const Type* referenced_type, int addr_space)
        : Type(typetable, kind, {referenced_type})
        , addr_space_(addr_space)
    {}

    std::ostream& stream_ptr_type(std::ostream&, std::string prefix, int addr_space, const Type* ref_type) const;

public:
    const Type* referenced_type() const { return arg(0); }
    int addr_space() const { return addr_space_; }

    virtual std::ostream& stream(std::ostream&) const override;
    virtual uint64_t vhash() const override;
    virtual bool equal(const Type* other) const override;
    virtual std::string prefix() const = 0;

private:
    int addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_borrowed_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "&"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;
};

class MutPtrType : public PtrType {
public:
    MutPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_mut_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "&mut"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_owned_ptr, referenced_type, addr_space)
    {}

    virtual std::string prefix() const override { return "~"; }

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;
};

//------------------------------------------------------------------------------

class FnType : public Type {
private:
    FnType(TypeTable& typetable, Types args)
        : Type(typetable, Kind_fn, args)
    {
        ++order_;
    }

public:
    const Type* return_type() const;
    bool is_returning() const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, int kind, const Type* elem_type)
        : Type(typetable, kind, {elem_type})
    {}

public:
    const Type* elem_type() const { return arg(0); }
};

class IndefiniteArrayType : public ArrayType {
public:
    IndefiniteArrayType(TypeTable& typetable, const Type* elem_type)
        : ArrayType(typetable, Kind_indefinite_array, elem_type)
    {}

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const override;

    friend class TypeTable;
};

class DefiniteArrayType : public ArrayType {
public:
    DefiniteArrayType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Kind_definite_array, elem_type)
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
    virtual const Type* vspecialize(Type2Type&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class SimdType : public ArrayType {
public:
    SimdType(TypeTable& typetable, const Type* elem_type, uint64_t dim)
        : ArrayType(typetable, Kind_simd, elem_type)
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
    virtual const Type* vspecialize(Type2Type&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& typetable)
        : Type(typetable, Kind_noret, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const;

    friend class TypeTable;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable)
        : Type(typetable, Kind_error, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const;

    friend class TypeTable;
};

class UnknownType : public Type {
private:
    UnknownType(TypeTable& typetable)
        : Type(typetable, Kind_unknown, {})
    {
        known_ = false;
    }

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual bool equal(const Type*) const override;
    virtual uint64_t vhash() const override { return thorin::hash_value(this->gid()); }
    virtual const Type* vrebuild(TypeTable&, Types) const override;
    virtual const Type* vspecialize(Type2Type&) const;

    friend class TypeTable;
};

const Type* stream_type_params(std::ostream& os, const Type* type);

//------------------------------------------------------------------------------

}

#endif
