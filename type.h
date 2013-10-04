#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>

#include "anydsl2/util/array.h"
#include "anydsl2/util/cast.h"
#include "anydsl2/util/hash.h"

class FnType;
class PrimType;
class TupleType;
class Type;
class TypeError;
class TypeTable;

//------------------------------------------------------------------------------

enum Kind {
#define PRIMTYPE(T) Type_##T,
#include "primtypes.h"
    Type_error,
    Type_fn,
    Type_tuple,
};

enum PrimTypeKind {
#define PRIMTYPE(T) PrimType_##T = Type_##T,
#include "primtypes.h"
};

class Type : public anydsl2::MagicCast {
private:
    Type& operator = (const Type&); ///< Do not copy-assign a \p Type instance.
    Type(const Type& node);         ///< Do not copy-construct a \p Type.

protected:
    Type(TypeTable& typetable, Kind kind, size_t size)
        : typetable_(typetable)
        , kind_(kind)
        , ops_(size)
    {}

    void set(size_t i, const Type* n) { ops_[i] = n; }

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    anydsl2::ArrayRef<const Type*> elems() const { return anydsl2::ArrayRef<const Type*>(ops_); }
    const Type* elem(size_t i) const { return elems()[i]; }
    size_t size() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void dump() const;
    virtual size_t hash() const;
    virtual bool equal(const Type*) const;
    virtual std::string to_string() const = 0;

private:
    TypeTable& typetable_;
    Kind kind_;
    std::vector<const Type*> ops_;

    friend class TypeTable;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable) 
        : Type(typetable, Type_error, 0)
    {}

public:
    virtual std::string to_string() const { return "<type error>"; }

    friend class TypeTable;
};

class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, PrimTypeKind kind)
        : Type(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

public:
    virtual std::string to_string() const;

    friend class TypeTable;
};

class CompoundType : public Type {
protected:
    CompoundType(TypeTable& typetable, Kind kind, anydsl2::ArrayRef<const Type*> elems)
        : Type(typetable, kind, elems.size())
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }

    std::string elems_to_string() const;
};

class FnType : public CompoundType {
private:
    FnType(TypeTable& typetable, anydsl2::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

public:
    virtual std::string to_string() const { return std::string("fn") + elems_to_string(); }

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, anydsl2::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

public:
    virtual std::string to_string() const { return std::string("tuple") + elems_to_string(); }

    friend class TypeTable;
};

//------------------------------------------------------------------------------

struct TypeHash : std::unary_function<const Type*, size_t> {
    size_t operator () (const Type* t) const { return t->hash(); }
};

struct TypeEqual : std::binary_function<const Type*, const Type*, bool> {
    bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); }
};

typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

    const TypeError* type_error() { return type_error_; }
    const PrimType* primtype(PrimTypeKind kind);
#define PRIMTYPE(T) const PrimType* type_##T() { return T##_; }
#include "primtypes.h"
    const FnType* fntype0() { return fntype(anydsl2::ArrayRef<const Type*>(nullptr, 0)); }
    const FnType* fntype1(const Type* elem1) { 
        const Type* elems[1] = { elem1 }; 
        return fntype(elems); 
    }
    const FnType* fntype2(const Type* elem1, const Type* elem2) { 
        const Type* elems[2] = { elem1, elem2 }; 
        return fntype(elems); 
    }
    const FnType* fntype3(const Type* elem1, const Type* elem2, const Type* elem3) { 
        const Type* elems[3] = { elem1, elem2, elem3 }; 
        return fntype(elems); 
    }
    const FnType* fntype(anydsl2::ArrayRef<const Type*> elems) { return unify(new FnType(*this, elems)); }

    const TupleType* tupletype0() { return tupletype(anydsl2::ArrayRef<const Type*>(nullptr, 0)); }
    const TupleType* tupletype1(const Type* elem1) { 
        const Type* elems[1] = { elem1 }; 
        return tupletype(elems); 
    }
    const TupleType* tupletype2(const Type* elem1, const Type* elem2) { 
        const Type* elems[2] = { elem1, elem2 }; 
        return tupletype(elems); 
    }
    const TupleType* tupletype3(const Type* elem1, const Type* elem2, const Type* elem3) { 
        const Type* elems[3] = { elem1, elem2, elem3 }; 
        return tupletype(elems); 
    }
    const TupleType* tupletype(anydsl2::ArrayRef<const Type*> elems) { return unify(new TupleType(*this, elems)); }

private:
    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }

    TypeSet types_;

#define PRIMTYPE(T) const PrimType* T##_;
#include "primtypes.h"
    const TypeError* type_error_;
};

//------------------------------------------------------------------------------

#endif
