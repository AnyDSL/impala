#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include "thorin/util/array.h"
#include "thorin/util/cast.h"
#include "thorin/util/hash.h"
#include "thorin/util/stream.h"

#include "impala/symbol.h"

namespace thorin {
    class StructAbsType;
    class Type;
}

namespace impala {

//------------------------------------------------------------------------------

class CodeGen;
class StructDecl;
class Type;
class TypeParam;
class TypeTable;

template<class T> using ArrayRef = thorin::ArrayRef<T>;
template<class T> using Array    = thorin::Array<T>;

struct TypeHash {
    uint64_t operator()(const Type* type) const;
};

template<class Value>
using TypeMap    = thorin::HashMap<const Type*, Value, TypeHash>;
using TypeSet    = thorin::HashSet<const Type*, TypeHash>;
using Type2Type  = TypeMap<const Type*>;

typedef ArrayRef<const Type*> Types;

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
    Kind_struct_abs,
    Kind_struct_app,
    Kind_trait_abs,
    Kind_trait_app,
    Kind_tuple,
    Kind_type_param,
    Kind_typedef_abs,
    Kind_unknown,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Kind_##itype,
#include "impala/tokenlist.h"
};

//------------------------------------------------------------------------------

/// Base class for all \p Type%s.
class Type : public thorin::Streamable, public thorin::MagicCast<Type> {
protected:
    Type(const Type&) = delete;
    Type& operator=(const Type&) = delete;

    Type(TypeTable& typetable, Kind kind, Types args, size_t num_type_params = 0)
        : typetable_(typetable)
        , kind_(kind)
        , args_(args.size())
        , type_params_(num_type_params)
        , gid_(gid_counter_++)
    {
        for (size_t i = 0, e = num_args(); i != e; ++i) {
            if (auto arg = args[i])
                set(i, arg);
        }
    }

    void set(size_t i, const Type* type) {
        args_[i] = type;
        order_ = std::max(order_, type->order());
        closed_      &= type->is_closed();
        monomorphic_ &= type->is_monomorphic();
        known_       &= type->is_known();
    }

public:
    Kind kind() const { return kind_; }
    Types args() const { return args_; }
    ArrayRef<const TypeParam*> type_params() const { return type_params_; }
    size_t num_type_params() const { return type_params().size(); }
    const Type* arg(size_t i) const { assert(i < args().size()); return args()[i]; }
    const TypeParam* type_param(size_t i) const { assert(i < type_params().size()); return type_params()[i]; }
    const Type* close(ArrayRef<const TypeParam*>) const;
    size_t num_args() const { return args_.size(); }
    bool is_hashed() const { return hashed_; }          ///< This @p Type is already recorded inside of @p TypeTable.
    bool empty() const { return args_.empty(); }
    TypeTable& typetable() const { return typetable_; }
    size_t gid() const { return gid_; }
    int order() const { return order_; }
    bool is_closed() const { return closed_; }  ///< Are all @p TypeParam%s bound?
    bool is_monomorphic() const { return monomorphic_; }        ///< Does this @p Type not depend on any @p TypeParam%s?.
    bool is_polymorphic() const { return !is_monomorphic(); }   ///< Does this @p Type depend on any @p TypeParam%s?.
    bool is_known() const { return known_; }
    bool is_unknown() const { return !is_known(); }
    virtual const Type* instantiate(Types) const;
    const Type* instantiate(Type2Type&) const;
    const Type* specialize(Type2Type&) const;
    const thorin::Type* thorin_type() const { return thorin_type_; }
    const Type* rebuild(Types) const;

#define IMPALA_TYPE(itype, atype) bool is_##itype() const { return is(PrimType_##itype); }
#include "impala/tokenlist.h"
    bool is(PrimTypeKind kind) const;
    bool is_int() const { return is_i8() || is_i16() || is_i32() || is_i64() || is_u8() || is_u16() || is_u32() || is_u64(); }
    bool is_float() const { return is_f32() || is_f64(); }

    uint64_t hash() const { return is_hashed() ? hash_ : hash_ = vhash(); }
    virtual uint64_t vhash() const;
    virtual bool equal(const Type*) const;

    static size_t gid_counter() { return gid_counter_; }

protected:
    std::ostream& stream_type_params(std::ostream&) const;
    Array<const Type*> specialize_args(Type2Type&) const;
    void convert_args(CodeGen&, std::vector<const thorin::Type*>&) const;

    int order_ = 0;
    mutable uint64_t hash_ = 0;
    mutable bool hashed_ = false;
    mutable bool closed_ = true;
    mutable bool monomorphic_ = true;
    mutable bool known_ = true;

private:
    virtual const Type* vrebuild(Types) const = 0;
    virtual const Type* vinstantiate(Type2Type&) const = 0;
    virtual const thorin::Type* convert(CodeGen&) const = 0;

    TypeTable& typetable_;
    Kind kind_;
    Array<const Type*> args_;
    mutable Array<const TypeParam*> type_params_;
    mutable size_t gid_;
    static size_t gid_counter_;

protected:
    mutable const thorin::Type* thorin_type_ = nullptr;

    friend class CodeGen;
    friend class TypeTable;
};

inline uint64_t TypeHash::operator()(const Type* type) const { return type->gid(); }

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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

    friend class TypeTable;
};

//------------------------------------------------------------------------------

class PtrType : public Type {
protected:
    PtrType(TypeTable& typetable, Kind kind, const Type* referenced_type, int addr_space)
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
    virtual const thorin::Type* convert(CodeGen&) const override;

    int addr_space_;

    friend class TypeTable;
};

class BorrowedPtrType : public PtrType {
public:
    BorrowedPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_borrowed_ptr, referenced_type, addr_space)
    {}

    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual std::string prefix() const override { return "&"; }
};

class MutPtrType : public PtrType {
public:
    MutPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_mut_ptr, referenced_type, addr_space)
    {}

    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual std::string prefix() const override { return "&mut"; }
};

class OwnedPtrType : public PtrType {
public:
    OwnedPtrType(TypeTable& typetable, const Type* referenced_type, int addr_space)
        : PtrType(typetable, Kind_owned_ptr, referenced_type, addr_space)
    {}

    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual std::string prefix() const override { return "~"; }
};

//------------------------------------------------------------------------------

class StructAbsType : public Type {
private:
    StructAbsType(TypeTable& typetable, const StructDecl* struct_decl);

public:
    const StructDecl* struct_decl() const { return struct_decl_; }
    const thorin::StructAbsType* thorin_struct_abs_type() const { return thorin_struct_abs_type_; }
    void set(size_t i, const Type* type) const { const_cast<StructAbsType*>(this)->Type::set(i, type); }
    virtual uint64_t vhash() const override { return thorin::hash_value(this->gid()); }
    virtual bool equal(const Type* other) const override { return this == other; }
    virtual const Type* instantiate(Types args) const override;

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override { THORIN_UNREACHABLE; }
    virtual const thorin::Type* convert(CodeGen&) const override;

    const StructDecl* struct_decl_;
    mutable const thorin::StructAbsType* thorin_struct_abs_type_;

    friend class TypeTable;
};

/**
 * @brief A struct application.
 *
 * A concrete instantiation of a struct abstraction is a struct application.
 * @see StructAbsType.
 */
class StructAppType : public Type {
private:
    StructAppType(const StructAbsType* struct_abs_type, Types args)
        : Type(struct_abs_type->typetable(), Kind_struct_app, Array<const Type*>(args.size() + 1))
        , struct_abs_type_(struct_abs_type)
        , elem_cache_(struct_abs_type->num_args())
    {
        set(0, struct_abs_type);
        for (size_t i = 0, e = args.size(); i != e; ++i)
            set(i+1, args[i]);
    }

public:
    const StructAbsType* struct_abs_type() const { return arg(0)->as<StructAbsType>(); }
    Types type_args() const { return args().skip_front(); }
    const Type* type_arg(size_t i) const { return type_args()[i]; }
    size_t num_type_args() const { return type_args().size(); }
    const Type* elem(size_t i) const;
    Types elems() const;
    size_t num_elems() const { return struct_abs_type()->num_args(); }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

    const StructAbsType* struct_abs_type_;
    mutable Array<const Type*> elem_cache_;

    friend class TypeTable;
};

class TupleType : public Type {
private:
    TupleType(TypeTable& typetable, Types args)
        : Type(typetable, Kind_tuple, args)
    {}

    virtual std::ostream& stream(std::ostream&) const override;
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

    friend class TypeTable;
};

class FnType : public Type {
private:
    FnType(TypeTable& typetable, Types args, size_t num_type_params)
        : Type(typetable, Kind_fn, args, num_type_params)
    {
        ++order_;
    }

public:
    const Type* return_type() const;
    bool is_returning() const;
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, Kind kind, const Type* elem_type)
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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override;

    uint64_t dim_;

    friend class TypeTable;
};

class TypeParam : public Type {
private:
    TypeParam(TypeTable& typetable, Symbol symbol)
        : Type(typetable, Kind_type_param, {})
        , symbol_(symbol)
    {
        closed_ = false;
        monomorphic_ = false;
    }

public:
    Symbol symbol() const { return symbol_; }
    const Type* binder() const { return binder_; }
    size_t index() const { return index_; }

    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual uint64_t vhash() const override;
    virtual bool equal(const Type*) const override;
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const override;
    virtual const thorin::Type* convert(CodeGen&) const override { THORIN_UNREACHABLE; return nullptr; }

    Symbol symbol_;
    mutable const Type* binder_;
    mutable size_t index_;
    mutable const TypeParam* equiv_ = nullptr;

    friend bool Type::equal(const Type*) const;
    friend const Type* Type::close(ArrayRef<const TypeParam*>) const;
    friend class TypeTable;
    friend class InferSema;
};

class NoRetType : public Type {
private:
    NoRetType(TypeTable& typetable)
        : Type(typetable, Kind_noret, {})
    {}

public:
    virtual std::ostream& stream(std::ostream&) const override;

private:
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const;
    virtual const thorin::Type* convert(CodeGen&) const override { THORIN_UNREACHABLE; return nullptr; }

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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const;
    virtual const thorin::Type* convert(CodeGen&) const override { THORIN_UNREACHABLE; return nullptr; }

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
    virtual const Type* vrebuild(Types) const override;
    virtual const Type* vinstantiate(Type2Type&) const;
    virtual const thorin::Type* convert(CodeGen&) const override { THORIN_UNREACHABLE; return nullptr; }

    std::string name_;
    mutable const Type* binder_;
    mutable size_t index_;
    mutable const TypeParam* equiv_ = nullptr;

    friend bool Type::equal(const Type*) const;
    friend const Type* Type::close(ArrayRef<const TypeParam*>) const;
    friend class TypeTable;
};

std::ostream& stream_type_params(std::ostream& os, const Type* type);

//------------------------------------------------------------------------------

}

#endif
