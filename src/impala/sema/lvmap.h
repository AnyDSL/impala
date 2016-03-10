#ifndef IMPALA_SEMA_LVMAP_H
#define IMPALA_SEMA_LVMAP_H

#include <memory>

#include "thorin/util/hash.h"

namespace impala {

class ValueDecl;

typedef uint32_t varid;
typedef int payload_t;

enum class LvComponentType: char { VAR, STRUCTFIELD, DEREF, ERROR, NOT_PRESENT};

class LvTree {
public:
    LvTree(LvComponentType type)
        : type_(type)
        {}
    ~LvTree() {}
    LvComponentType get_type() const { return type_; }
    const char* get_name() const;
    payload_t get_payload() const { return payload_; }
    void set_payload(payload_t pl) { payload_ = pl; }
    // TODO: use ArrayRef ?
    std::vector<LvTree> get_children();

private:
    std::vector<LvTree> children_;
    payload_t payload_ = 0;
    LvComponentType type_;
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
