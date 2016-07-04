#ifndef IMPALA_SEMA_LVMAP_H
#define IMPALA_SEMA_LVMAP_H

#include <memory>
#include <stack>

#include "thorin/util/hash.h"
#include "thorin/util/location.h"
#include "thorin/util/stream.h"

namespace impala {

class ValueDecl;
class Expr;
class LvTree;

typedef int payload_t;

const payload_t INHERITED_PAYLOAD = 0;

const thorin::Location UNSET_LOCATION = thorin::Location();

class Payload: public thorin::HasLocation {
    public:
        // TODO: the loc_ init is not good, maybe use a pointer instead
        Payload(): HasLocation(UNSET_LOCATION), value_(INHERITED_PAYLOAD) {}
        payload_t get_value() const { assert(!is_inherited()); return value_; }
        bool is_inherited() const { return value_ == INHERITED_PAYLOAD; }

    protected:
        void set_payload(payload_t, const thorin::Location&);
        void set_inherited(const thorin::Location&);

        friend class LvTree;

    private:
        payload_t value_;
};

//-----------------------------------------------------------------------

struct LvTreeLookupRes {
    LvTreeLookupRes(std::shared_ptr<LvTree>, bool multi_ref, const Payload&);
    LvTreeLookupRes(std::shared_ptr<LvTree>, bool multi_ref);
    // TODO: maybe introduce a move constructor for the shared pointer
    LvTreeLookupRes(const Payload& pl);

    const std::shared_ptr<LvTree> tree_;
    const bool is_multi_ref_;
    const Payload& payload_;
};

//---------------------------------------------------------------------------------

enum class Relation { LESS, GREATER, EQUAL, INCOMPARABLE };
class LvMapComparator {
public:
    // TODO: extend this
    //public void add_relation(payload_t, payload_t, Relation);

    /// returns LESS if p1 < p2, GREATER if p1 > p2 and EQUAL if p1 == p2
    Relation compare(payload_t, payload_t) const;
    payload_t infimum(payload_t, payload_t) const;
};

//---------------------------------------------------------------------------------

/*
 * ScopedMap
 */

template <class Value>
class ScopedMap : public thorin::HashMap<const ValueDecl*, Value> {
public:
    // TODO: prohibit other ways to add mappings
    // TODO: pass Value by value?
    void add_mapping (const ValueDecl* k, Value v) {
        assert(!(thorin::HashMap<const ValueDecl*, Value>::contains)(k));
        scope_stack_.push(k);
        thorin::HashMap<const ValueDecl*, Value>::operator[](k) = v;
    }

    void enter_scope() {
        scope_stack_.push(nullptr);
    }

    void leave_scope() {
        assert(!scope_stack_.empty());
        const ValueDecl* k;
        do {
            k = scope_stack_.top();
            scope_stack_.pop();
            if (k != nullptr) {
                assert((thorin::HashMap<const ValueDecl*, Value>::contains)(k));
                thorin::HashMap<const ValueDecl*, Value>::erase(k);
            }
        } while (k != nullptr);
    }

private:
    std::stack<const ValueDecl*> scope_stack_;
};

//---------------------------------------------------------------------------------

class LvMap : public thorin::Streamable {
private:
    LvMap& operator= (const LvMap &);

public:
    LvMap(const LvMapComparator&);
    LvMap(const LvMap&);
    ~LvMap();

    std::shared_ptr<LvTree> lookup(const ValueDecl*) const;
    bool contains(const ValueDecl* decl) const { return varmap_.contains(decl); }
    void insert(const ValueDecl*, payload_t, const thorin::Location&);
    void update(const ValueDecl*, LvTree*);

    const LvMapComparator& get_comparator(void) const { return comparator_; }

    virtual void enter_scope();
    virtual void leave_scope();

    void merge(LvMap& other);

    std::ostream& stream(std::ostream&) const;

private:
    ScopedMap<std::shared_ptr<LvTree>> varmap_;
    const LvMapComparator& comparator_;
};

//--------------------------------------------------------------------------------

inline bool payload2bool(payload_t pl) { assert(pl == 0 || pl == 1); return (bool) pl; }

const Payload& lookup(const Expr*, LvMap&);

void insert(const Expr*, LvMap&, payload_t, const thorin::Location& loc);

}

#endif
