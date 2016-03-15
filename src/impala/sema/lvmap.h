#ifndef IMPALA_SEMA_LVMAP_H
#define IMPALA_SEMA_LVMAP_H

#include <memory>

#include "thorin/util/hash.h"

namespace impala {

class ValueDecl;
class Expr;
class LvTree;

typedef int payload_t;

const payload_t ERROR_PAYLOAD = -1;

//enum class LvTreeLookupResType { TREE, NOT_EXPLICIT, ERROR };

struct LvTreeLookupRes {
    LvTreeLookupRes(LvTree& tree): is_tree_(true), value_(tree) {};
    LvTreeLookupRes(payload_t pl): is_tree_(false), value_(pl) {};

    bool is_tree_;
    union LvTreeLookupResValue {
        LvTreeLookupResValue(LvTree& tree): tree_(tree) {};
        LvTreeLookupResValue(payload_t pl): implicit_payload_(pl) {}

        LvTree& tree_;
        payload_t implicit_payload_;
    } value_;
};

enum class Relation { LESS, GREATER, EQUAL, INCOMPARABLE };
class LvMapComparator {
public:
    // TODO: extend this
    //public void add_relation(payload_t, payload_t, Relation);

    /// returns LESS if p1 < p2, GREATER if p1 > p2 and EQUAL if p1 == p2
    Relation compare(payload_t p1, payload_t p2) const;
};

class LvMap {
public:
    LvMap(LvMapComparator);
    ~LvMap();

    LvTree& lookup(const ValueDecl*) const;
    void insert(const ValueDecl*, payload_t);

    const LvMapComparator& get_comparator(void) const { return comparator_; };

    //void enter_scope();
    //void leave_scope();

    // TODO: rather use copy constructor?
    //LvMap duplicate() const;
    //void merge(const LvMap& other);

private:
    thorin::HashMap<const ValueDecl*, std::shared_ptr<LvTree>> varmap_;
    LvMapComparator comparator_;
};

inline bool payload2bool(payload_t pl) { assert(pl == 0 || pl == 1); return (bool) pl; }

payload_t lookup_payload(const Expr&, LvMap&);

void insert(const Expr&, LvMap&, payload_t);

}

#endif
