#ifndef IMPALA_SEMA_LVMAP_H
#define IMPALA_SEMA_LVMAP_H

#include <memory>

#include "thorin/util/hash.h"

namespace impala {

class ValueDecl;
class LvTree;

typedef int payload_t;

const payload_t ERROR_PAYLOAD = -1;

enum class LvTreeLookupResType { TREE, NOT_EXPLICIT, ERROR };

struct LvTreeLookupRes {
    LvTreeLookupRes(LvTree& tree): type_(LvTreeLookupResType::TREE), value_(tree) {};
    LvTreeLookupRes(payload_t pl): type_(LvTreeLookupResType::NOT_EXPLICIT), value_(pl) {};
    LvTreeLookupRes(void): type_(LvTreeLookupResType::ERROR), value_(ERROR_PAYLOAD) {};

    LvTreeLookupResType type_;
    union LvTreeLookupResValue {
        LvTreeLookupResValue(LvTree& tree): tree_(tree) {};
        LvTreeLookupResValue(payload_t pl): implicit_payload_(pl) {}

        LvTree& tree_;
        payload_t implicit_payload_;
    } value_;
};

class LvMap {
public:
    LvMap();
    ~LvMap();

    LvTree& lookup(const ValueDecl*) const;
    void insert(const ValueDecl*, payload_t);

    //void enter_scope();
    //void leave_scope();

    // TODO: rather use copy constructor?
    //LvMap duplicate() const;
    //void merge(const LvMap& other);

private:
    thorin::HashMap<const ValueDecl*, std::shared_ptr<LvTree>> varmap_;
};

inline bool payload2bool(payload_t pl) { assert(pl == 0 || pl == 1); return (bool) pl; }

}

#endif
