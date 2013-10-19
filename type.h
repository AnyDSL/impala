#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>

#include "anydsl2/util/array.h"
#include "anydsl2/util/cast.h"
#include "anydsl2/util/hash.h"

// TODO there should be a way to get the const's back!
// e.g. one could use a set to store all not types that are not yet unified as
// non-const value and give out the values only as const ones... something like
// that, but my first try did not work!

class FnType;
class PrimType;
class TupleType;
class TypeVar;
class Type;
class TypeError;
class TypeTable;

typedef anydsl2::ArrayRef<Type*> TypeArray;
typedef anydsl2::ArrayRef<TypeVar*> TypeVarArray;


//------------------------------------------------------------------------------

enum Kind {
#define PRIMTYPE(T) Type_##T,
#include "primtypes.h"
    Type_error,
    Type_fn,
    Type_tuple,
    Type_var,
    Type_generic,
};

enum PrimTypeKind {
#define PRIMTYPE(T) PrimType_##T = Type_##T,
#include "primtypes.h"
};

class TypeVisitor {
public:
    virtual ~TypeVisitor() {}
    virtual void visit(TypeError&) {}
    virtual void visit(PrimType&) {}
    virtual void visit(FnType&) {}
    virtual void visit(TupleType&) {}
    virtual void visit(TypeVar&) {}
};

class Type : public anydsl2::MagicCast<Type> {
private:
    Type& operator = (const Type&); ///< Do not copy-assign a \p Type.
    Type(const Type& node);         ///< Do not copy-construct a \p Type.

protected:
    Type(TypeTable& typetable, Kind kind, size_t size)
        : typetable_(typetable)
        , kind_(kind)
        , elems_(size)
        , representative_(nullptr)
    {}

    void set(size_t i, Type* n) { elems_[i] = n; }

    std::string bound_vars_to_string() const;

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    TypeArray elems() const { return TypeArray(elems_); }
    Type* elem(size_t i) const { return elems()[i]; }

    anydsl2::ArrayRef<const TypeVar*> bound_vars() const { return anydsl2::ArrayRef<const TypeVar*>(bound_vars_); }
    const TypeVar* bound_var(size_t i) const { return bound_vars()[i]; }
    void add_bound_var(const TypeVar* v) { bound_vars_.push_back(v); }

    /// Returns number of \p Type operands (\p elems_).
    size_t size() const { return elems_.size(); }
    size_t num_bound_vars() const { return bound_vars_.size(); }

    /// Returns true if this \p Type does not have any \p Type operands (\p elems_).
    bool is_empty() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return elems_.empty();
    }

    /// Returns true if this \p Type does have any bound type variabes (\p bound_vars_).
    bool is_generic() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return !bound_vars_.empty();
    }

    virtual bool equal(const Type*) const;
    size_t hash() const;

    void dump() const;
    virtual std::string to_string() const = 0;
    virtual void accept(TypeVisitor&) = 0;

    /**
     * A type is closed if it contains no unbound type variables.
     */
    virtual bool is_closed() const;

    /**
     * Get the unambiguous representative of this type.
     * All operations should only be done with this representative.
     *
     * (representative == nullptr) means that this type has not yet been unified.
     * Otherwise it either points to itself of to another type.
     */
    const Type* get_representative() const {
        assert((representative_ == nullptr) || representative_->is_final_representative());
        return representative_;
    }

    /// @see get_representative()
    void set_representative(const Type* repr) {
        // TODO does this really hold? (is it set only once?)
        assert(representative_ == nullptr);
        representative_ = repr;
        assert((representative_)->is_final_representative());
    }

    bool is_final_representative() const {
        return representative_ == this;
    }

    /// @see get_representative()
    bool is_unified() const { return get_representative() != nullptr; }

    // TODO bool is_sane() const;

private:
    TypeTable& typetable_;
    const Kind kind_;
    const Type* representative_;
    std::vector<Type*> elems_; ///< The operands of this type constructor.
    std::vector<const TypeVar*> bound_vars_;

    friend class TypeTable;
};

class TypeError : public Type {
private:
    TypeError(TypeTable& typetable) 
        : Type(typetable, Type_error, 0)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }
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

    virtual void accept(TypeVisitor& v) { v.visit(*this); }

    friend class TypeTable;
};

class CompoundType : public Type {
protected:
    CompoundType(TypeTable& typetable, Kind kind, TypeArray elems)
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
    FnType(TypeTable& typetable, TypeArray elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }

    virtual std::string to_string() const { return std::string("fn") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, TypeArray elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    virtual std::string to_string() const { return std::string("tuple") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TypeVar : public Type {
private:
    TypeVar(TypeTable& tt)
        : Type(tt, Type_var, 0)
        , bound_at_(nullptr)
        , equiv_var_(new const TypeVar*())
    {
        id_ = counter++;
    }

    static int counter;

    /// used for unambiguous dumping
    int id_;

    const Type* bound_at_;

    /// Used to define equivalence constraints when checking equality of types
    const TypeVar** const equiv_var_;

    void set_equiv_variable(const TypeVar* v) const {
        assert(*equiv_var_ == nullptr);
        assert(v != nullptr);
        *equiv_var_ = v;
    }

    void unset_equiv_variable() const {
        assert(*equiv_var_ != nullptr);
        *equiv_var_ = nullptr;
    }

public:
    virtual bool equal(const Type* other) const;

    void bind(const Type* const t) {
        // TODO mayby do a real pre-condition instead of assert
        assert(bound_at_ == nullptr && "type variables can only be bound once!");
        bound_at_ = t;
    }

    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    std::string to_string() const;

    virtual bool is_closed() const { return bound_at_ != nullptr; }

    friend class TypeTable;
    friend class Type;
};


//------------------------------------------------------------------------------

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<Type*, TypeHash, TypeEqual> TypeSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable() { for (auto type : types_) delete type; }

    TypeError* type_error() { return type_error_; }
    PrimType* primtype(PrimTypeKind kind);

#define PRIMTYPE(T) PrimType* type_##T() { return T##_; }
#include "primtypes.h"

    TypeVar* typevar() { return new TypeVar(*this); }

    FnType* fntype(TypeArray params) { return unify(new FnType(*this, params)); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    FnType* fntype_simple(TypeArray params, Type* return_type);

    /**
     * Create a generic type given the quantified type variables and the type
     * using them.
     *
     * Example: create 'fn<A>(A)'
     * @code{.cpp}
     * TypeVarRef* A = typevar();
     * gentype({A}, fntype({A}));
     * @endcode
     */
    template<class T> T* gentype(TypeVarArray tvars, T* type) {
        for (auto v : tvars) {
            v->bind(type);
            type->add_bound_var(v);
        }
        return unify(type);
    }

    TupleType* tupletype(TypeArray elems) { return unify(new TupleType(*this, elems)); }

    /**
     * Checks if all types in the type tables are sane and correctly unified.
     */
    void check_sanity() const;

private:
    /// insert all not-unified types contained in type
    void insert_new(Type* type);

    Type* unify_base(Type* type);
    template<class T> T* unify(T* type) { return unify_base(type)->template as<T>(); }

    TypeSet types_;

#define PRIMTYPE(T) PrimType* T##_;
#include "primtypes.h"
    TypeError* type_error_;
};

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void check_sanity(TypeArray types);

#endif
