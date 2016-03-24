#ifndef IMPALA_SEMA_LVMAP_H
#define IMPALA_SEMA_LVMAP_H

#include <memory>
#include <stack>

#include "thorin/util/hash.h"
#include "thorin/util/location.h"

namespace impala {

class ValueDecl;
class Expr;
class LvTree;

typedef int payload_t;

const payload_t ERROR_PAYLOAD = -1;

const thorin::Location UNSET_LOCATION = thorin::Location();

class Payload: public thorin::HasLocation {
    public:
        // TODO: the loc_ init is not good, maybe use a pointer instead
        Payload(): HasLocation(UNSET_LOCATION), payload_val_(0) {}
        payload_t get_value() const { return payload_val_; }

    protected:
        void set_payload(payload_t, const thorin::Location&);

        friend class LvTree;

    private:
        payload_t payload_val_;
};

//-----------------------------------------------------------------------

struct LvTreeLookupTree {
    LvTreeLookupTree(LvTree* tree, bool multi_ref): tree_(tree), multi_ref_(multi_ref) {}

    LvTree* tree_;
    bool multi_ref_;
};

struct LvTreeLookupRes {
    LvTreeLookupRes(LvTree* tree, bool multi_ref): is_tree_(true), value_(tree, multi_ref) {};
    LvTreeLookupRes(LvTreeLookupTree tree): is_tree_(true), value_(tree) {};
    LvTreeLookupRes(const Payload& pl): is_tree_(false), value_(pl) {};

    bool is_tree_;

    union LvTreeLookupResValue {
        LvTreeLookupResValue(LvTree* tree, bool multi_ref): tree_res_(tree, multi_ref) {};
        LvTreeLookupResValue(LvTreeLookupTree tree): tree_res_(tree) {};
        LvTreeLookupResValue(const Payload& pl): implicit_payload_(pl) {};

        LvTreeLookupTree tree_res_;
        const Payload& implicit_payload_;
    } value_;
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

class LvMap {
private:
    LvMap& operator= (const LvMap &);

public:
    LvMap(const LvMapComparator&);
    //LvMap(const LvMap&);
    ~LvMap();

    LvTreeLookupTree lookup(const ValueDecl*) const;
    void insert(const ValueDecl*, payload_t, const thorin::Location&);
    void update(const ValueDecl*, LvTree*);

    const LvMapComparator& get_comparator(void) const { return comparator_; };

    void enter_scope();
    void leave_scope();

    void merge(LvMap& other);

private:
    thorin::HashMap<const ValueDecl*, std::shared_ptr<LvTree>> varmap_;
    const LvMapComparator& comparator_;
    std::stack<const ValueDecl*> scope_stack_;
};

inline bool payload2bool(payload_t pl) { assert(pl == 0 || pl == 1); return (bool) pl; }

const Payload& lookup_payload(const Expr&, LvMap&);

void insert(const Expr&, LvMap&, payload_t, const thorin::Location& loc);

}

#endif
