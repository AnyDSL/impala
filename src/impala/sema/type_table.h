// TODO convert this to impala namespace or maybe just directly use Artic's
// infrastructure here

#ifndef MIM_UTIL_TYPE_TABLE_H
#define MIM_UTIL_TYPE_TABLE_H

#include <type_traits>

#include <fe/cast.h>

#include "impala/stream.h"

#include "mim/def.h"
#include "mim/util/hash.h"

namespace mim {

#define MIM_S_TYPES(m) m(s8) m(s16) m(s32) m(s64)
#define MIM_U_TYPES(m) m(u8) m(u16) m(u32) m(u64)
#define MIM_F_TYPES(m) m(f16) m(f32) m(f64)

#define MIM_TYPES(m) MIM_S_TYPES(m) MIM_U_TYPES(m) MIM_F_TYPES(m)

/// @c static_cast checked in debug version
template <class L, class R> inline L *scast(R *r) {
  static_assert(std::is_base_of<R, L>::value, "R is not a base type of L");
  assert((!r || dynamic_cast<L *>(r)) && "cast not possible");
  return static_cast<L *>(r);
}

/// @c static_cast checked in debug version -- @c const version
template <class L, class R> inline const L *scast(const R *r) {
  return const_cast<const L *>(scast<L, R>(const_cast<R *>(r)));
}

/// shorthand for @c dynamic_cast
template <class L, class R> inline L *dcast(R *u) {
  static_assert(std::is_base_of<R, L>::value, "R is not a base type of L");
  return dynamic_cast<L *>(u);
}

/// shorthand for @c dynamic_cast -- @c const version
template <class L, class R> inline const L *dcast(const R *r) {
  return const_cast<const L *>(dcast<L, R>(const_cast<R *>(r)));
}

class Type;

template <class To> using TypeMap = GIDMap<const Type *, To>;
using TypeSet = GIDSet<const Type *>;
using Type2Type = TypeMap<const Type *>;

//------------------------------------------------------------------------------

/// Base class for all \p Type%s.
template <class TypeTable>
class TypeBase : public fe::RuntimeCast<TypeBase<TypeTable>>,
                 public impala::Streamable<TypeBase<TypeTable>> {
protected:
  using Type2Type = GIDMap<const TypeBase *, const TypeBase *>;
  using Types = Span<const TypeBase *>;

  TypeBase(const TypeBase &) = delete;
  TypeBase &operator=(const TypeBase &) = delete;

  TypeBase(TypeTable &table, int tag, Types ops);

  void set(size_t i, const TypeBase *type) {
    ops_[i] = type;
    order_ = std::max(order_, type->order());
    monomorphic_ &= type->is_monomorphic();
    if (!is_nominal())
      known_ &= type->is_known();
  }

public:
  virtual ~TypeBase() {}

  int tag() const { return tag_; }
  TypeTable &table() const { return *table_; }

  Types ops() const { return ops_; }
  const TypeBase *op(size_t i) const { return ops()[i]; }
  size_t num_ops() const { return ops_.size(); }
  bool empty() const { return ops_.empty(); }

  bool is_nominal() const {
    return nominal_;
  } ///< A nominal @p Type is always different from each other @p Type.
  bool is_known() const {
    return known_;
  } ///< Does this @p Type depend on any @p UnknownType%s?
  bool is_monomorphic() const {
    return monomorphic_;
  } ///< Does this @p Type not depend on any @p Var%s?.
  bool is_polymorphic() const {
    return !is_monomorphic();
  } ///< Does this @p Type depend on any @p Var%s?.
  int order() const { return order_; }
  size_t gid() const { return gid_; }
  uint32_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
  virtual bool equal(const TypeBase *) const;

  const TypeBase *reduce(int, const TypeBase *, Type2Type &) const;
  const TypeBase *rebuild(TypeTable &to, Types ops) const;
  const TypeBase *rebuild(Types ops) const { return rebuild(table(), ops); }

  /// The type of a partial derivative when applying the grad-operator.
  /// nullptr if the type cannot be derived.
  virtual const TypeBase *tangent_vector() const { return nullptr; }

  static size_t gid_counter() { return gid_counter_; }
  virtual impala::Stream &stream(impala::Stream &) const = 0;

protected:
  virtual uint32_t vhash() const;
  virtual const TypeBase *vreduce(int, const TypeBase *, Type2Type &) const;

  mutable uint32_t hash_ = 0;
  int order_ = 0;
  mutable bool known_ = true;
  mutable bool monomorphic_ = true;
  mutable bool nominal_ = false;

private:
  virtual const TypeBase *vrebuild(TypeTable &to, Types ops) const = 0;

  mutable TypeTable *table_;
  int tag_;
  mim::Vector<const TypeBase *> ops_;
  mutable size_t gid_;
  static size_t gid_counter_;

  friend TypeTable;
};

//------------------------------------------------------------------------------

/// Base class for all \p TypeTable%s.
template <class Type> class TypeTableBase {
public:
  struct TypeHash {
    size_t operator()(const Type *t) const { return t->hash(); }
  };

  struct TypeEq {
    bool operator()(const Type *t1, const Type *t2) const {
      return t2->equal(t1);
    }
  };

  typedef absl::flat_hash_set<const Type *, TypeHash, TypeEq> TypeSet;

  TypeTableBase &operator=(const TypeTableBase &) = delete;
  TypeTableBase(const TypeTableBase &) = delete;

  TypeTableBase() {}
  virtual ~TypeTableBase() {
    for (auto type : types_)
      delete type;
  }

  const TypeSet &types() const { return types_; }

protected:
  const Type *unify_base(const Type *type);
  template <class T> const T *unify(const T *type) {
    return unify_base(type)->template as<T>();
  }
  const Type *insert(const Type *);

  TypeSet types_;
};

//------------------------------------------------------------------------------

template <class TypeTable> size_t TypeBase<TypeTable>::gid_counter_ = 1;

template <class TypeTable>
TypeBase<TypeTable>::TypeBase(TypeTable &table, int tag, Types ops)
    : table_(&table), tag_(tag), ops_(ops.size()), gid_(gid_counter_++) {
  for (size_t i = 0, e = num_ops(); i != e; ++i) {
    if (auto op = ops[i])
      set(i, op);
  }
}

template <class TypeTable>
const TypeBase<TypeTable> *TypeBase<TypeTable>::reduce(int depth,
                                                       const TypeBase *type,
                                                       Type2Type &map) const {
  if (auto i = map.find(this); i != map.end())
    return i->second;
  if (is_monomorphic())
    return this;
  auto new_type = vreduce(depth, type, map);
  return map[this] = new_type;
}

template <class TypeTable>
const TypeBase<TypeTable> *TypeBase<TypeTable>::vreduce(int depth,
                                                        const TypeBase *type,
                                                        Type2Type &map) const {
  if (num_ops() > 0) {
    Vector<const TypeBase *> result(num_ops());
    for (size_t i = 0, e = num_ops(); i != e; ++i)
      result[i] = op(i)->reduce(depth, type, map);
    return vrebuild(table(), result);
  }
  return this;
}

template <class TypeTable>
const TypeBase<TypeTable> *TypeBase<TypeTable>::rebuild(TypeTable &to,
                                                        Types ops) const {
  assert(num_ops() == ops.size());
  if (ops.empty() && &table() == &to)
    return this;
  return vrebuild(to, ops);
}

template <class TypeTable> uint32_t TypeBase<TypeTable>::vhash() const {
  if (is_nominal())
    return mim::murmur3(uint32_t(gid()));

  uint32_t seed = mim::hash_begin(uint8_t(tag()));
  for (auto op : ops_)
    seed = mim::hash_combine(seed, uint32_t(op->gid()));
  return seed;
}

template <class TypeTable>
bool TypeBase<TypeTable>::equal(const TypeBase *other) const {
  if (is_nominal())
    return this == other;

  bool result = this->tag() == other->tag() &&
                this->num_ops() == other->num_ops() &&
                this->is_monomorphic() == other->is_monomorphic();

  if (result) {
    for (size_t i = 0, e = num_ops(); result && i != e; ++i)
      result &= this->op(i) == other->op(i);
  }

  return result;
}

//------------------------------------------------------------------------------

template <class Type>
const Type *TypeTableBase<Type>::unify_base(const Type *type) {
  auto i = types_.find(type);
  if (i != types_.end()) {
    delete type;
    type = *i;
    return type;
  }

  return insert(type);
}

template <class Type>
const Type *TypeTableBase<Type>::insert(const Type *type) {
  const auto &p = types_.insert(type);
  assert_unused(p.second && "hash/equal broken");
  return type;
}

//------------------------------------------------------------------------------

} // namespace mim

#endif // MIM_UTIL_TYPE_TABLE_H
