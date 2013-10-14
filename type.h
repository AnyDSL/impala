#ifndef TYPE_H
#define TYPE_H

#include <unordered_set>

#include "anydsl2/util/array.h"
#include "anydsl2/util/cast.h"
#include "anydsl2/util/hash.h"

class FnType;
class PrimType;
class TupleType;
class TypeVar;
class Type;
class TypeError;
class TypeTable;

typedef anydsl2::ArrayRef<const Type*> TypeArray;
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

class Type : public anydsl2::MagicCast<Type> {
private:
    Type& operator = (const Type&); ///< Do not copy-assign a \p Type.
    Type(const Type& node);         ///< Do not copy-construct a \p Type.

protected:
    Type(TypeTable& typetable, Kind kind, size_t size)
        : typetable_(typetable)
        , kind_(kind)
        , elems_(size)
    {}

    void set(size_t i, const Type* n) { elems_[i] = n; }

public:
    TypeTable& typetable() const { return typetable_; }
    Kind kind() const { return kind_; }
    anydsl2::ArrayRef<const Type*> elems() const { return anydsl2::ArrayRef<const Type*>(elems_); }
    const Type* elem(size_t i) const { return elems()[i]; }
    /// Returns number of \p Type operands (\p elems_).
    size_t size() const { return elems_.size(); }
    /// Returns true if this \p Type does not have any \p Type operands (\p elems_).
    bool empty() const { return elems_.empty(); }
    void dump() const;
    virtual size_t hash() const;
    virtual bool equal(const Type*) const;
    virtual std::string to_string() const = 0;

private:
    TypeTable& typetable_;
    Kind kind_;
    std::vector<const Type*> elems_; ///< The operands of this type constructor.

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

class TypeVar : public Type {
private:
	TypeVar(TypeTable& typetable) : Type(typetable, Type_var, 0) {
		id = counter++;
	}

	static int counter;

	int id;
	const Type* boundAt;

public:
	void bind(const Type* t) { boundAt = t; }

	virtual std::string to_string() const {
		return std::string("a") + std::to_string(id);
	}



	friend class TypeTable;
};

//------------------------------------------------------------------------------

struct TypeHash { size_t operator () (const Type* t) const { return t->hash(); } };
struct TypeEqual { bool operator () (const Type* t1, const Type* t2) const { return t1->equal(t2); } };
typedef std::unordered_set<const Type*, TypeHash, TypeEqual> TypeSet;

struct GenTypeReturn {
	anydsl2::ArrayRef<TypeVar*> type_vars;
	const Type* generic_type;
};

class TypeTable {
public:
    TypeTable();
    ~TypeTable() { for (auto type : types_) delete type; }

    const TypeError* type_error() { return type_error_; }
    const PrimType* primtype(PrimTypeKind kind);

#define PRIMTYPE(T) const PrimType* type_##T() { return T##_; }
#include "primtypes.h"

    //TypeVar* typevar() { return new TypeVar(*this); }

    const FnType* fntype(TypeArray params) { return unify(new FnType(*this, params)); }

    /**
     * A shortcut to create function types with a return type.
     *
     * Actually for a Type fn(int)->int a type fn(int, fn(int)) will be created
     * (continuation passing style).
     */
    const FnType* fntype_simple(TypeArray params, const Type* return_type) {
    	const FnType* retfun = fntype({return_type});

    	size_t psize = params.size();

    	const Type** p = new const Type*[psize + 1];

    	for (int i = 0; i < psize; ++i) {
    		p[i] = params[i];
    	}
    	p[psize] = retfun;

    	return fntype(TypeArray(p, psize + 1));
    }

    /**
     * Create a generic type.
     *
     * This method ensures that the same type is never created twice using hash
     * consing.
     *
     * Example Usage: Creation of generic type 'forall a b, fn(a, b)'
     * @code{.cpp}
     * gentype(2, [](TypeVarArray tvars, TypeTable& tt) -> const Type* {
     *   return tt.fntype({tvars[0], tvars[1]});
     * });
     * @endcode
     *
     * @param tvar_num Number of quantified type variables
     * @param create_type The callback function that actually creates the
     * 				desired type with given generic type variables.
     * @return The created generic type and the quantified type variables used
     * 		in this type.
     */
    const GenTypeReturn gentype(int tvar_num, const Type* (*create_type)(TypeVarArray, TypeTable&)) {
    	TypeVar** tvars_ptr = new TypeVar*[tvar_num];

    	for (int i = 0; i < tvar_num; ++i) {
    		tvars_ptr[i] = new TypeVar(*this);
    	}

    	TypeVarArray tvars = TypeVarArray(tvars_ptr, tvar_num);
    	const Type* the_type = create_type(tvars, *this);

    	for (auto v : tvars) {
    		v->bind(the_type);
    	}

    	GenTypeReturn ret;
    	ret.generic_type = unify(the_type);

    	if (ret.generic_type != the_type) {
    		// TODO get the new type variables
    	} else {
    		ret.type_vars = tvars;
    	}

    	return ret;
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
