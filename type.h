#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>

#include "anydsl2/util/hash.h"

class FnType;
class PrimType;
class TupleType;
class Type;
class TypeError;
class TypeTable;

//------------------------------------------------------------------------------

class Type : public anydsl2::Node {
private:
    Type& operator = (const Type&); ///< Do not copy-assign a \p Type instance.
    Type(const Type& node);         ///< Do not copy-construct a \p Type.

protected:
    Type(TypeTable& typetable, Kind kind, size_t size)
        : typetable_(typetable)
        , kind_(kind)
        , ops_(size)
    {}
    virtual ~Type() {}

public:
    enum Kind {
#define PRIMTYPE(T) Type_##T,
#include "primtypes.h"
        Type_error,
        Type_fn,
        Type_tuple,
    };

    TokenKind kind() const { return (TokenKind) anydsl2::Node::kind(); }
    anydsl2::ArrayRef<const Type*> elems() const { return ops_ref<const Type*>(); }
    const Type* elem(size_t i) const { return elems()[i]; }
    void dump() const;
    void set(size_t i, const Type* t) { Node::set(i, t); }

protected:
    TypeTable& typetable_;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable) 
        : Type(typetable, Token::TYPE_error, 0)
    {}

    friend class TypeTable;
};

class PrimType : public Type {
public:
    enum Kind {
#define PRIMTYPE(T) PrimType_##T = Type_##T,
#include "primtypes.h"
    };

private:
    PrimType(TypeTable& typetable, PrimTypeKind kind)
        : Type(typetable, kind, 0)
    {}

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
};

class FnType : public CompoundType {
private:
    FnType(TypeTable& typetable, anydsl2::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

public:
    const Type* return_type() const;

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, anydsl2::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

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
    const FnType* fntype1(const Type*);
    const FnType* fntype(anydsl2::ArrayRef<const Type*> elems);
    const TupleType* tupletype(anydsl2::ArrayRef<const Type*> elems);

private:
    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }

    TypeSet types_;

#define PRIMTYPE(T) const PrimType* T##_;
#include "primtypes.h"
    const TypeError* type_error_;
};

//------------------------------------------------------------------------------

} // namespace impala

#endif
