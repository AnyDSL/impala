#ifndef IMPALA_TYPE_H
#define IMPALA_TYPE_H

#include <iostream>
#include <unordered_set>

#include "thorin/node.h"
#include "thorin/util/hash.h"

#include "impala/token.h"

namespace thorin {
    class Type;
    class World;
}

namespace impala {

class FnType;
class Fun;
class IdType;
class NoRet;
class PrimType;
class Sema;
class TupleType;
class Type;
class TypeTable;
class TypeError;

//------------------------------------------------------------------------------

class Type : public thorin::Node<Type> {
protected:
    Type(TypeTable& typetable, TokenKind kind, size_t size, const std::string& name)
        : Node((int) kind, size, name)
        , typetable_(typetable)
    {}

public:
    TokenKind kind() const { return (TokenKind) thorin::Node<Type>::kind(); }
    thorin::ArrayRef<const Type*> elems() const { return ops_ref<const Type*>(); }
    const Type* elem(size_t i) const { return elems()[i]; }
    virtual const Type* refine(const Sema&) const = 0;
    virtual const thorin::Type* convert(thorin::World&) const = 0;
    bool is_bool() const;
    bool is_int() const;
    bool is_float() const;
    bool is_noret() const { return isa<NoRet>() != nullptr; }
    bool check_with(const Type*) const;
    void dump() const;
    void set(size_t i, const Type* t) { assert(!t->is_noret()); Node::set(i, t); }

protected:
    TypeTable& typetable_;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable) 
        : Type(typetable, Token::TYPE_error, 0, "<error>")
    {}

    virtual const Type* refine(const Sema&) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

class PrimType : public Type {
private:
    PrimType(TypeTable& typetable, TokenKind kind)
        : Type(typetable, kind, 0, "<primitive type>")
    {}

public:
    virtual const Type* refine(const Sema&) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class NoRet : public Type {
private:
    NoRet(TypeTable& typetable) 
        : Type(typetable, Token::L_N, 0, "!")
    {}

    virtual const Type* refine(const Sema&) const { return this; }
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

class ArrayType : public Type {
protected:
    ArrayType(TypeTable& typetable, TokenKind kind, const Type* elem_type)
        : Type(typetable, kind, 1, "<array type>")
    {
        set(0, elem_type);
    }

public:
    const Type* elem_type() const { return elem(0); }
};

class IndefiniteArray : public ArrayType {
public:
    IndefiniteArray(TypeTable& typetable, const Type* elem_type)
        : ArrayType(typetable, Token::TYPE_definite_array, elem_type)
    {}

    virtual const Type* refine(const Sema&) const;
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class DefiniteArray : public ArrayType {
public:
    DefiniteArray(TypeTable& typetable, const Type* elem_type, thorin::u64 dim)
        : ArrayType(typetable, Token::TYPE_definite_array, elem_type)
        , dim_(dim)
    {}

    thorin::u64 dim() const { return dim_; }
    virtual const Type* refine(const Sema&) const;
    virtual const thorin::Type* convert(thorin::World&) const;
    virtual size_t hash() const { return thorin::hash_combine(ArrayType::hash(), dim()); }
    virtual bool equals(const Node* other) const {
        return Type::equal(other) && this->dim() == other->as<DefiniteArray>()->dim();
    }

private:
    thorin::u64 dim_;

    friend class TypeTable;
};

class CompoundType : public Type {
protected:
    CompoundType(TypeTable& typetable, TokenKind kind, thorin::ArrayRef<const Type*> elems, const std::string& name)
        : Type(typetable, kind, elems.size(), name)
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }
};

class FnType : public CompoundType {
private:
    FnType(TypeTable& typetable, thorin::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Token::FN, elems, "<function type>")
    {}

public:
    virtual const Type* refine(const Sema&) const;
    virtual const thorin::Type* convert(thorin::World&) const;
    const Type* unpack_return_type() const;
    const Type* return_type() const;

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, thorin::ArrayRef<const Type*> elems)
        : CompoundType(typetable, Token::TYPE_tuple, elems, "<tuple type>")
    {}

    virtual const Type* refine(const Sema& sema) const;
    virtual const thorin::Type* convert(thorin::World&) const;

    friend class TypeTable;
};

class IdType : public Type {
private:
    IdType(TypeTable& typetable, thorin::Symbol symbol)
        : Type(typetable, Token::TYPE_id, 0, symbol.str())
    {}
    virtual size_t hash() const { return thorin::hash_value(this); }
    virtual bool equal(const Node* other) const { return this == other; }
    virtual const Type* refine(const Sema&) const;
    virtual const thorin::Type* convert(thorin::World&) const { assert(false); return nullptr; }

    friend class TypeTable;
};

//------------------------------------------------------------------------------

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable();

    const TypeError* type_error() { return type_error_; }
    const NoRet* noret() { return noret_; }
    const PrimType* primtype(TokenKind kind);
    const DefiniteArray* definite_array(const Type* elem_type, thorin::u64 dim);
    const IndefiniteArray* indefinite_array(const Type* elem_type);
#define IMPALA_TYPE(itype, atype) const PrimType* type_##itype() { return itype##_; }
#include "impala/tokenlist.h"
    const FnType* fntype(thorin::ArrayRef<const Type*> elems);
    const FnType* pack_return_type(const Type* type);
    const TupleType* tupletype(thorin::ArrayRef<const Type*> elems);
    const IdType* idtype(thorin::Symbol);

private:
    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    TypeTable& operator = (const TypeTable&);

    TypeSet types_;

#define IMPALA_TYPE(itype, atype) const PrimType* itype##_;
#include "impala/tokenlist.h"
    const TypeError* type_error_;
    const NoRet* noret_;
};

//------------------------------------------------------------------------------

} // namespace impala

#endif
