#ifndef IMPALA_SEMA_TYPE_H
#define IMPALA_SEMA_TYPE_H

#include <unordered_map>

#include "thorin/util/autoptr.h"
#include "thorin/util/array.h"
#include "thorin/util/hash.h"

#include "impala/sema/typeproperties.h"

namespace impala {

//------------------------------------------------------------------------------

struct TraitInstanceHash { 
    size_t operator () (const TraitInstance t) const { return thorin::hash_value(t.representative()); } 
};

struct TraitInstanceEqual { 
    bool operator () (const TraitInstance t1, const TraitInstance t2) const { 
        return t1.representative() == t2.representative(); } 
};

typedef std::unordered_set<TraitInstance, TraitInstanceHash, TraitInstanceEqual> TraitInstSet;

//------------------------------------------------------------------------------

typedef std::unordered_map<const TypeNode*, Type> SpecializeMapping;

enum Kind {
#define IMPALA_TYPE(itype, atype) Type_##itype,
#include "impala/tokenlist.h"
    Type_error,
    Type_fn,
    Type_tuple,
    Type_var,
};

enum PrimTypeKind {
#define IMPALA_TYPE(itype, atype) PrimType_##itype = Type_##itype,
#include "impala/tokenlist.h"
};

class TypeNode : public GenericElement {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p TypeNode.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p TypeNode.

protected:
    TypeNode(TypeTable& typetable, Kind kind, size_t size)
        : GenericElement(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    void set(size_t i, Type n) { elems_[i] = n; }
    Type elem_(size_t i) const { return elems_[i]; }

public:
    Kind kind() const { return kind_; }
    //TypeNodeArray elems() const { return TypeNodeArray(elems_); }
    const Type elem(size_t i) const { return elems_[i]; }

    /// Returns number of \p TypeNode operands (\p elems_).
    size_t size() const { return elems_.size(); }

    /// Returns true if this \p TypeNode does not have any \p TypeNode operands (\p elems_).
    bool is_empty() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return elems_.empty();
    }

    virtual bool equal(const GenericElement*) const;
    virtual bool equal(Type t) const { return equal(t.representative()); }
    virtual bool equal(const TypeNode*) const;
    virtual size_t hash() const;

    void dump() const;
    virtual std::string to_string() const = 0;

    Type instantiate(thorin::ArrayRef<Type> var_instances) const;

    bool is_generic() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return GenericElement::is_generic();
    }

    /**
     * A type is closed if it contains no unbound type variables.
     */
    virtual bool is_closed() const;

    /// @return true if this is a subtype of super_type.
    bool is_subtype(const Type super_type) const { return is_subtype(super_type.representative()); }

    /**
     * A type is sane if all type variables are bound correctly,
     * i.e. forall type variables v, v is a subtype of v.bound_at().
     *
     * This also means that a sane type is always closed!
     */
    virtual bool is_sane() const;

private:
    bool is_subtype(const TypeNode* super_type) const;

    /// copy this type but replace the subtypes given in the mapping
    Type specialize(SpecializeMapping&) const;

    /// like specialize but does not care about generics (this method is called by specialize)
    virtual Type vspecialize(SpecializeMapping&) const = 0;

    const Kind kind_;

protected:
    std::vector<Type> elems_; ///< The operands of this type constructor.

    friend class CompoundType;
    friend class TypeTable;
};

struct TypeNodeHash { 
    size_t operator () (const TypeNode* t) const { return t->hash(); } 
};

struct TypeNodeEqual { 
    bool operator () (const TypeNode* t1, const TypeNode* t2) const { return t1->equal(t2); } 
};

typedef std::unordered_set<TypeNode*, TypeNodeHash, TypeNodeEqual> TypeNodeSet;

class TypeErrorNode : public TypeNode {
private:
    TypeErrorNode(TypeTable& typetable)
        : TypeNode(typetable, Type_error, 0)
    {}

    virtual Type vspecialize(SpecializeMapping&) const;

public:
    virtual std::string to_string() const { return "<type error>"; }

    friend class TypeTable;
};

class PrimTypeNode : public TypeNode {
private:
    PrimTypeNode(TypeTable& typetable, PrimTypeKind kind)
        : TypeNode(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }
    virtual Type vspecialize(SpecializeMapping&) const;

public:
    virtual std::string to_string() const;

    friend class TypeTable;
};

class CompoundType : public TypeNode {
protected:
    CompoundType(TypeTable& typetable, Kind kind, thorin::ArrayRef<Type> elems)
        : TypeNode(typetable, kind, elems.size())
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }

    std::string elems_to_string() const;

    thorin::Array<Type> specialize_elems(SpecializeMapping& mapping) const;
};

class FnTypeNode : public CompoundType {
private:
    FnTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

    virtual Type vspecialize(SpecializeMapping&) const;

public:
    virtual std::string to_string() const { return std::string("fn") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TupleTypeNode : public CompoundType {
private:
    TupleTypeNode(TypeTable& typetable, thorin::ArrayRef<Type> elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

    virtual Type vspecialize(SpecializeMapping&) const;

public:
    virtual std::string to_string() const { return std::string("tuple") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TypeVarNode : public TypeNode {
private:
    TypeVarNode(TypeTable& tt)
        : TypeNode(tt, Type_var, 0)
        , id_(counter++)
        , bound_at_(nullptr)
        , equiv_var_(nullptr)
    {}

    void set_equiv_variable(const TypeVarNode* v) const { assert(equiv_var_ == nullptr); assert(v != nullptr); equiv_var_ = v; }
    void unset_equiv_variable() const { assert(equiv_var_ != nullptr); equiv_var_ = nullptr; }
    void bind(const GenericElement* const e);
    bool restrictions_equal(const TypeVar other) const;

public:
    const TraitInstSet* bounds() const { return &bounds_; }
    const GenericElement* bound_at() const { return bound_at_; }
    void add_bound(TraitInstance restriction);
    virtual bool equal(const TypeNode* other) const;
    std::string to_string() const;

    /**
     * A type variable is closed if it is bound and all restrictions are closed.
     * If a type variable is closed it must not be changed anymore!
     */
    virtual bool is_closed() const;

    // CHECK this->is_subtype(bound_at()); if bound_at is a Type, else it should occur in the method signatures
    virtual bool is_sane() const { return is_closed(); }

private:
    const int id_;       ///< Used for unambiguous dumping.
    TraitInstSet bounds_;///< All traits that restrict the instantiation of this variable.
    /**
     * The type where this variable is bound.
     * If such a type is set, then the variable must not be changed anymore!
     */
    const GenericElement* bound_at_;
    mutable const TypeVarNode* equiv_var_;///< Used to define equivalence constraints when checking equality of types.
    static int counter;

    virtual Type vspecialize(SpecializeMapping&) const;
    /// Create a fully fledged clone of this TypeVar (except of the binding)
    TypeVar clone() const;

    friend class TypeTable;
    friend class TypeNode;
    friend class GenericElement;
};

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void verify(thorin::ArrayRef<const Type> types);

}

#endif
