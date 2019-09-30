// TODO convert this to impala namespace or maybe just directly use Artic's infrastructure here

#ifndef THORIN_UTIL_TYPE_TABLE_H
#define THORIN_UTIL_TYPE_TABLE_H

#include <type_traits>

#include "thorin/def.h"
#include "thorin/util/hash.h"
#include "thorin/util/cast.h"
#include "thorin/util/array.h"
#include "thorin/util/streamf.h"

namespace thorin {

#define THORIN_S_TYPES(m) m(s8)  m(s16) m(s32) m(s64)
#define THORIN_U_TYPES(m) m(u8)  m(u16) m(u32) m(u64)
#define THORIN_F_TYPES(m)        m(f16) m(f32) m(f64)

#define THORIN_TYPES(m) THORIN_S_TYPES(m) THORIN_U_TYPES(m) THORIN_F_TYPES(m)

/// @c static_cast checked in debug version
template<class L, class R>
inline L* scast(R* r) {
    static_assert(std::is_base_of<R, L>::value, "R is not a base type of L");
    assert((!r || dynamic_cast<L*>(r)) && "cast not possible" );
    return static_cast<L*>(r);
}

/// @c static_cast checked in debug version -- @c const version
template<class L, class R>
inline const L* scast(const R* r) { return const_cast<const L*>(scast<L, R>(const_cast<R*>(r))); }

/// shorthand for @c dynamic_cast
template<class L, class R>
inline L* dcast(R* u) {
    static_assert(std::is_base_of<R, L>::value, "R is not a base type of L");
    return dynamic_cast<L*>(u);
}

/// shorthand for @c dynamic_cast -- @c const version
template<class L, class R>
inline const L* dcast(const R* r) { return const_cast<const L*>(dcast<L, R>(const_cast<R*>(r))); }

/**
 * Provides handy @p as and @p isa methods.
 * Inherit from this class in order to use
 * @code
Bar* bar = foo->as<Bar>();
if (Bar* bar = foo->isa<Bar>()) { ... }
 * @endcode
 * instead of more combersume
 * @code
Bar* bar = thorin::scast<Bar>(foo);
if (Bar* bar = thorin::dcast<Bar>(foo)) { ... }
 * @endcode
 */
template<class Base>
class RTTICast {
public:
    /**
     * Acts as static cast -- checked for correctness in the debug version.
     * Use if you @em know that @p this is of type @p To.
     * It is a program error (an assertion is raised) if this does not hold.
     */
    template<class To> To* as() { return thorin::scast<To>(static_cast<Base*>(this)); }

    /**
     * Acts as dynamic cast.
     * @return @p this cast to @p To if @p this is a @p To, 0 otherwise.
     */
    template<class To> To* isa() { return thorin::dcast<To>(static_cast<Base*>(this)); }

    ///< @c const version of @see RTTICast#as.
    template<class To>
    const To* as()  const { return thorin::scast<To>(static_cast<const Base*>(this)); }

    ///< @c const version of @see RTTICast#isa.
    template<class To>
    const To* isa() const { return thorin::dcast<To>(static_cast<const Base*>(this)); }
};

class Type;

template<class To>
using TypeMap   = GIDMap<const Type*, To>;
using TypeSet   = GIDSet<const Type*>;
using Type2Type = TypeMap<const Type*>;

//------------------------------------------------------------------------------

/// Base class for all \p Type%s.
template <class TypeTable>
class TypeBase : public RTTICast<TypeBase<TypeTable>>, public Streamable {
protected:
    using Type2Type = GIDMap<const TypeBase*, const TypeBase*>;
    using Types     = ArrayRef<const TypeBase*>;

    TypeBase(const TypeBase&) = delete;
    TypeBase& operator=(const TypeBase&) = delete;

    TypeBase(TypeTable& table, int tag, Types ops);

    void set(size_t i, const TypeBase* type) {
        ops_[i] = type;
        order_       = std::max(order_, type->order());
        monomorphic_ &= type->is_monomorphic();
        if (!is_nominal())
            known_ &= type->is_known();
    }

public:
    int tag() const { return tag_; }
    TypeTable& table() const { return *table_; }

    Types ops() const { return ops_; }
    const TypeBase* op(size_t i) const { return ops()[i]; }
    size_t num_ops() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }

    bool is_nominal() const { return nominal_; }              ///< A nominal @p Type is always different from each other @p Type.
    bool is_known()   const { return known_; }                ///< Does this @p Type depend on any @p UnknownType%s?
    bool is_monomorphic() const { return monomorphic_; }      ///< Does this @p Type not depend on any @p Var%s?.
    bool is_polymorphic() const { return !is_monomorphic(); } ///< Does this @p Type depend on any @p Var%s?.
    int order() const { return order_; }
    size_t gid() const { return gid_; }
    uint32_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
    virtual bool equal(const TypeBase*) const;

    const TypeBase* reduce(int, const TypeBase*, Type2Type&) const;
    const TypeBase* rebuild(TypeTable& to, Types ops) const;
    const TypeBase* rebuild(Types ops) const { return rebuild(table(), ops); }

    static size_t gid_counter() { return gid_counter_; }

protected:
    virtual uint32_t vhash() const;
    virtual const TypeBase* vreduce(int, const TypeBase*, Type2Type&) const;

    mutable uint32_t hash_ = 0;
    int order_ = 0;
    mutable bool known_       = true;
    mutable bool monomorphic_ = true;
    mutable bool nominal_     = false;

private:
    virtual const TypeBase* vrebuild(TypeTable& to, Types ops) const = 0;

    mutable TypeTable* table_;
    int tag_;
    thorin::Array<const TypeBase*> ops_;
    mutable size_t gid_;
    static size_t gid_counter_;

    friend TypeTable;
};

//------------------------------------------------------------------------------

/// Base class for all \p TypeTable%s.
template <class Type>
class TypeTableBase {
public:
    struct TypeHash {
        static uint32_t hash(const Type* t) { return t->hash(); }
        static bool eq(const Type* t1, const Type* t2) { return t2->equal(t1); }
        static const Type* sentinel() { return (const Type*)(1); }
    };

    typedef thorin::HashSet<const Type*, TypeHash> TypeSet;

    TypeTableBase& operator=(const TypeTableBase&) = delete;
    TypeTableBase(const TypeTableBase&) = delete;

    TypeTableBase() {}
    virtual ~TypeTableBase() { for (auto type : types_) delete type; }

    const TypeSet& types() const { return types_; }

protected:
    const Type* unify_base(const Type* type);
    template<class T> const T* unify(const T* type) { return unify_base(type)->template as<T>(); }
    const Type* insert(const Type*);

    TypeSet types_;
};

//------------------------------------------------------------------------------

template <class TypeTable>
size_t TypeBase<TypeTable>::gid_counter_ = 1;

template <class TypeTable>
TypeBase<TypeTable>::TypeBase(TypeTable& table, int tag, Types ops)
    : table_(&table)
    , tag_(tag)
    , ops_(ops.size())
    , gid_(gid_counter_++)
{
    for (size_t i = 0, e = num_ops(); i != e; ++i) {
        if (auto op = ops[i])
            set(i, op);
    }
}

template <class TypeTable>
const TypeBase<TypeTable>* TypeBase<TypeTable>::reduce(int depth, const TypeBase* type, Type2Type& map) const {
    if (auto result = map.lookup(this))
        return *result;
    if (is_monomorphic())
        return this;
    auto new_type = vreduce(depth, type, map);
    return map[this] = new_type;
}

template <class TypeTable>
const TypeBase<TypeTable>* TypeBase<TypeTable>::vreduce(int depth, const TypeBase* type, Type2Type& map) const {
    if (num_ops() > 0) {
        Array<const TypeBase*> result(num_ops());
        for (size_t i = 0, e = num_ops(); i != e; ++i)
            result[i] = op(i)->reduce(depth, type, map);
        return vrebuild(table(), result);
    }
    return this;
}

template <class TypeTable>
const TypeBase<TypeTable>* TypeBase<TypeTable>::rebuild(TypeTable& to, Types ops) const {
    assert(num_ops() == ops.size());
    if (ops.empty() && &table() == &to)
        return this;
    return vrebuild(to, ops);
}

template <class TypeTable>
uint32_t TypeBase<TypeTable>::vhash() const {
    if (is_nominal())
        return thorin::murmur3(uint32_t(gid()));

    uint32_t seed = thorin::hash_begin(uint8_t(tag()));
    for (auto op : ops_)
        seed = thorin::hash_combine(seed, uint32_t(op->gid()));
    return seed;
}

template <class TypeTable>
bool TypeBase<TypeTable>::equal(const TypeBase* other) const {
    if (is_nominal())
        return this == other;

    bool result = this->tag() == other->tag() && this->num_ops() == other->num_ops()
        && this->is_monomorphic() == other->is_monomorphic();

    if (result) {
        for (size_t i = 0, e = num_ops(); result && i != e; ++i)
            result &= this->op(i) == other->op(i);
    }

    return result;
}

//------------------------------------------------------------------------------

template <class Type>
const Type* TypeTableBase<Type>::unify_base(const Type* type) {
    auto i = types_.find(type);
    if (i != types_.end()) {
        delete type;
        type = *i;
        return type;
    }

    return insert(type);
}

template <class Type>
const Type* TypeTableBase<Type>::insert(const Type* type) {
    const auto& p = types_.insert(type);
    assert_unused(p.second && "hash/equal broken");
    return type;
}

//------------------------------------------------------------------------------

}

#endif // THORIN_UTIL_TYPE_TABLE_H
