#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>
#include <exception>

#include "thorin/util/array.h"
#include "thorin/util/hash.h"

#include "type_properties.h"

class FnTypeNode;
class PrimType;
class TupleType;
class TypeVarNode;
class TypeNode;
class TypeError;

class TypeTraitInstance;
class TypeTable;

typedef thorin::ArrayRef<TypeNode*> TypeNodeArray;
//typedef thorin::ArrayRef<const TypeTraitInstance*> TypeTraitInstArray;
typedef std::unordered_set<TypeTraitInstance*> TypeTraitInstSet;

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
    virtual void visit(FnTypeNode&) {}
    virtual void visit(TupleType&) {}
    virtual void visit(TypeVarNode&) {}
};

class TypeNode : public GenericElement, public Unifiable<TypeNode> {
private:
    TypeNode& operator = (const TypeNode&); ///< Do not copy-assign a \p Type.
    TypeNode(const TypeNode& node);         ///< Do not copy-construct a \p Type.

protected:
    TypeNode(TypeTable& typetable, Kind kind, size_t size)
        : typetable_(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    std::vector<TypeNode*> elems_; ///< The operands of this type constructor.

    void set(size_t i, TypeNode* n) { elems_[i] = n; }
    TypeNode* elem_(size_t i) const { return elems_[i]; }

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    //TypeNodeArray elems() const { return TypeNodeArray(elems_); }
    const TypeNode* elem(size_t i) const { return elems_[i]; }

    /// Returns number of \p TypeNode operands (\p elems_).
    size_t size() const { return elems_.size(); }

    /// Returns true if this \p TypeNode does not have any \p TypeNode operands (\p elems_).
    bool is_empty() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return elems_.empty();
    }

    virtual bool equal(const GenericElement*) const;
    virtual bool equal(const TypeNode*) const;
    virtual size_t hash() const;

    void dump() const;
    virtual std::string to_string() const = 0;
    virtual void accept(TypeVisitor&) = 0;

    bool is_generic() const {
        assert (!elems_.empty() || bound_vars_.empty());
        return GenericElement::is_generic();
    }

    /**
     * A type is closed if it contains no unbound type variables.
     */
    virtual bool is_closed() const;

    /// @return true if this is a subtype of super_type.
    bool is_subtype(const TypeNode* super_type) const;

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

    friend class TypeTable;
};

class TypeError : public TypeNode {
private:
    TypeError(TypeTable& typetable) 
        : TypeNode(typetable, Type_error, 0)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    virtual std::string to_string() const { return "<type error>"; }

    friend class TypeTable;
};

class PrimType : public TypeNode {
private:
    PrimType(TypeTable& typetable, PrimTypeKind kind)
        : TypeNode(typetable, (Kind) kind, 0)
    {}

    PrimTypeKind primtype_kind() const { return (PrimTypeKind) kind(); }

public:
    virtual std::string to_string() const;

    virtual void accept(TypeVisitor& v) { v.visit(*this); }

    friend class TypeTable;
};

class CompoundType : public TypeNode {
protected:
    CompoundType(TypeTable& typetable, Kind kind, TypeNodeArray elems)
        : TypeNode(typetable, kind, elems.size())
    {
        size_t i = 0;
        for (auto elem : elems)
            set(i++, elem);
    }

    std::string elems_to_string() const;
};

class FnTypeNode : public CompoundType {
private:
    FnTypeNode(TypeTable& typetable, TypeNodeArray elems)
        : CompoundType(typetable, Type_fn, elems)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }

    virtual std::string to_string() const { return std::string("fn") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};

class TupleType : public CompoundType {
private:
    TupleType(TypeTable& typetable, TypeNodeArray elems)
        : CompoundType(typetable, Type_tuple, elems)
    {}

public:
    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    virtual std::string to_string() const { return std::string("tuple") + bound_vars_to_string() + elems_to_string(); }

    friend class TypeTable;
};



class TypeVarNode : public TypeNode {
private:
    TypeVarNode(TypeTable& tt)
        : TypeNode(tt, Type_var, 0)
        , id_(counter++)
        , restricted_by_()
        , bound_at_(nullptr)
        , equiv_var_(nullptr)
    {}

    static int counter;

    /// used for unambiguous dumping
    const int id_;

    /// All traits that restrict the instantiation of this variable
    TypeTraitInstSet restricted_by_;

    /**
     * The type where this variable is bound.
     * If such a type is set, then the variable must not be changed anymore!
     */
    const GenericElement* bound_at_;

    /// Used to define equivalence constraints when checking equality of types
    mutable const TypeVarNode* equiv_var_;

    void set_equiv_variable(const TypeVarNode* v) const {
        assert(equiv_var_ == nullptr);
        assert(v != nullptr);
        equiv_var_ = v;
    }

    void unset_equiv_variable() const {
        assert(equiv_var_ != nullptr);
        equiv_var_ = nullptr;
    }

    void bind(const GenericElement* const e);

    bool restrictions_equal(const TypeVarNode* other) const;

public:
    const TypeTraitInstSet* restricted_by() const { return &restricted_by_; }
    const GenericElement* bound_at() const { return bound_at_; }

    void add_restriction(TypeTraitInstance* restriction);

    virtual bool equal(const TypeNode* other) const;

    virtual void accept(TypeVisitor& v) { v.visit(*this); }
    std::string to_string() const;

    /**
     * A type variable is closed if it is bound and all restrictions are closed.
     * If a type variable is closed it must not be changed anymore!
     */
    virtual bool is_closed() const;

    // TODO this->is_subtype(bound_at()); if bound_at is a Type, else it should occur in the method signatures
    virtual bool is_sane() const { return is_closed(); }

    friend class TypeTable;
    friend class TypeNode;
    friend class GenericElement;
};

//------------------------------------------------------------------------------

/**
 * Checks if for all combination of types t1, t2 in 'types' it holds that if
 * both are unified and t1 equals t2 then they have the same representative.
 */
void check_sanity(thorin::ArrayRef<const TypeNode*> types);

#endif
