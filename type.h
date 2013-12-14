#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <exception>

#include "anydsl2/util/array.h"
#include "anydsl2/util/cast.h"
#include "anydsl2/util/hash.h"

class FnType;
class PrimType;
class TupleType;
class TypeVar;
class Type;
class TypeError;

class TypeTraitInstance;
class TypeTable;

typedef anydsl2::ArrayRef<const Type*> TypeArray;
typedef anydsl2::ArrayRef<const TypeVar*> TypeVarArray;
//typedef anydsl2::ArrayRef<const TypeTraitInstance*> TypeTraitInstArray;
typedef std::unordered_set<const TypeTraitInstance*> TypeTraitInstSet;

//------------------------------------------------------------------------------

class IllegalTypeException : public std::exception {
public:
    IllegalTypeException(const char* what)
        : std::exception()
        , what_(what)
    {}

    virtual const char* what() const throw () { return what_; }

private:
    const char* const what_;
};

//------------------------------------------------------------------------------

enum Kind {
#define PRIMTYPE(T) Type_##T,
#include "primtypes.h"
    Type_error,
    Type_fn,
    Type_tuple,
    Type_var,
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

    void set(size_t i, const Type* n) { elems_[i] = n; }

    std::string bound_vars_to_string() const;

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    TypeArray elems() const { return TypeArray(elems_); }
    const Type* elem(size_t i) const { return elems()[i]; }

    TypeVarArray bound_vars() const { return TypeVarArray(bound_vars_); }
    const TypeVar* bound_var(size_t i) const { return bound_vars()[i]; }

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
    virtual size_t hash() const;

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

    bool is_final_representative() const {
        return representative_ == this;
    }

    /// @see get_representative()
    bool is_unified() const { return get_representative() != nullptr; }

    /// @return true if this is a subtype of super_type.
    bool is_subtype(const Type* super_type) const;

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at().
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const;

private:
    TypeTable& typetable_;
    const Kind kind_;
    std::vector<const Type*> elems_; ///< The operands of this type constructor.
    mutable std::vector<const TypeVar*> bound_vars_;

    mutable const Type* representative_;

    void add_bound_var(const TypeVar* v) const { bound_vars_.push_back(v); }

    /// @see get_representative()
    void set_representative(const Type* repr) const {
        // TODO does this really hold? (is it set only once?)
        assert(representative_ == nullptr);
        representative_ = repr;
        assert((representative_)->is_final_representative());
    }

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
    TypeVar(TypeTable& tt, const TypeTraitInstSet restriction)
        : Type(tt, Type_var, 0)
        , id_(counter++)
        , restricted_by_(restriction)
        , bound_at_(nullptr)
        , equiv_var_(nullptr)
    {}

    static int counter;

    /// used for unambiguous dumping
    const int id_;

    const TypeTraitInstSet restricted_by_;
    mutable const Type* bound_at_;

    /// Used to define equivalence constraints when checking equality of types
    mutable const TypeVar* equiv_var_;

    void set_equiv_variable(const TypeVar* v) const {
        assert(equiv_var_ == nullptr);
        assert(v != nullptr);
        equiv_var_ = v;
    }

    void unset_equiv_variable() const {
        assert(equiv_var_ != nullptr);
        equiv_var_ = nullptr;
    }

    void bind(const Type* const t) const {
        if (bound_at_ != nullptr) {
            throw IllegalTypeException("type variables can only be bound once!");
        }
        bound_at_ = t;
    }

public:
    const TypeTraitInstSet* restricted_by() const { return &restricted_by_; }
    const Type* bound_at() const { return bound_at_; }

    virtual bool equal(const Type* other) const;

    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    std::string to_string() const;

    virtual bool is_closed() const { return bound_at_ != nullptr; }
    virtual bool is_sane() const { return is_closed() && this->is_subtype(bound_at()); }

    friend class TypeTable;
    friend class Type;
};

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void check_sanity(TypeArray types);

#endif
