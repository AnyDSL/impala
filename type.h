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
class TypeTrait;
class TypeVar;
class Type;
class TypeError;
class TypeTable;

typedef anydsl2::ArrayRef<const Type*> TypeArray;
typedef anydsl2::ArrayRef<const TypeVar*> TypeVarArray;
typedef anydsl2::ArrayRef<const TypeTrait*> TypeTraitArray;
typedef std::unordered_set<const TypeTrait*> TypeTraitSet;

//-----------------------------------------------------------------------------

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

class TypeTrait {
private:
    /// create the global top type trait (like Object in java)
    TypeTrait(TypeTable& tt)
        : typetable_(tt)
        , name_(top_trait_name)
        , super_traits_() // TODO is this correct?
    {}

    TypeTrait(TypeTable& tt, std::string name, const TypeTraitSet super_traits)
        : typetable_(tt)
        , name_(name)
        , super_traits_(super_traits)
    {}

    TypeTrait& operator = (const Type&); ///< Do not copy-assign a \p TypeTrait.
    TypeTrait(const Type& node);         ///< Do not copy-construct a \p TypeTrait.

    TypeTable& typetable_;
    std::string name_;
    const TypeTraitSet super_traits_;

    // TODO make this const
    static std::string top_trait_name;

public:
    bool equal(const TypeTrait* t) const;
    size_t hash() const;

    std::string to_string() const { return name_; }

    /// true if this is the global super type trait (like Object in java)
    bool is_top_trait() const {
        assert(super_traits_.size() != 0 || name_.compare(top_trait_name) == 0);
        return super_traits_.size() == 0;
    }

    friend class TypeTable;
};

class TypeVar : public Type {
private:
    TypeVar(TypeTable& tt, const TypeTraitSet restriction)
        : Type(tt, Type_var, 0)
        , id_(counter++)
        , restricted_by_(restriction)
        , bound_at_(nullptr)
        , equiv_var_(nullptr)
    {}

    static int counter;

    /// used for unambiguous dumping
    const int id_;

    const TypeTraitSet restricted_by_;
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
    const TypeTraitSet* restricted_by() const { return &restricted_by_; }
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

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

struct TypeTraitHash { size_t operator () (const TypeTrait* t) const { return t->hash(); } };
struct TypeTraitEqual { bool operator () (const TypeTrait* t1, const TypeTrait* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const TypeTrait*, TypeTraitHash, TypeTraitEqual> TraitTableSet;

class TypeTable {
public:
    TypeTable();
    ~TypeTable() { for (auto type : types_) delete type; }

    const TypeError* type_error() { return type_error_; }
    const PrimType* primtype(PrimTypeKind kind);

#define PRIMTYPE(T) const PrimType* type_##T() { return T##_; }
#include "primtypes.h"

    const TypeTrait* typetrait(std::string name, TypeTraitSet super_traits) {
        return unify_trait(new TypeTrait(*this, name, super_traits));
    }
    const TypeTrait* typetrait(std::string name) { return typetrait(name, {top_trait_}); }

    const TypeVar* typevar(TypeTraitSet restriction) { return new TypeVar(*this, restriction); }
    const TypeVar* typevar() { return typevar({top_trait_}); }

    const FnType* fntype(TypeArray params) { return unify_new(new FnType(*this, params)); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    const FnType* fntype_simple(TypeArray params, const Type* return_type);

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
    template<class T> const T* gentype(TypeVarArray tvars, const T* type) { return gentype_base(tvars, type)->template as<const T>(); }

    const TupleType* tupletype(TypeArray elems) { return unify_new(new TupleType(*this, elems)); }

    /**
     * Checks if all types in the type tables are sane and correctly unified.
     */
    void check_sanity() const;

private:
    const Type* gentype_base(TypeVarArray tvars, const Type* type);

    /// insert all not-unified types contained in type
    void insert_new(const Type* type);

    /**
     * Recursivly change the representatives of the not-unified types in t to the
     * corresponding types in repr.
     *
     * This assumes that t is equal to repr.
     */
    void change_repr(const Type* t, const Type* repr) const;

    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<const T>(); }

    /// like unify but deletes the given type if unification returned a different one
    template<class T> const T* unify_new(const T* type) {
        const T* unified_type = unify(type);
        if (unified_type != type)
            delete type;
        return unified_type;
    }

    const TypeTrait* unify_trait(const TypeTrait* type);

    TypeSet types_;
    TraitTableSet traits_;

#define PRIMTYPE(T) const PrimType* T##_;
#include "primtypes.h"
    const TypeError* type_error_;
    const TypeTrait* top_trait_; // TODO possibly find a better name
};

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void check_sanity(TypeArray types);

#endif
