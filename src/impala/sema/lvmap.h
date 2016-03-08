#ifndef IMPALA_SEMA_LVALUEMAP_H
#define IMPALA_SEMA_LVALUEMAP_H

//#include "thorin/util/hash.h"

#include "impala/ast.h"

namespace impala {

class ValueDecl;

typedef uint32_t varid;
typedef int payload_t;

enum class LvComponentType { VAR, STRUCTFIELD, DEREF };

class LvTree {
public:
    LvComponentType get_type();
    const char* get_name();
    payload_t get_payload();
    // TODO: use ArrayRef ?
    std::vector<LvTree> get_children();

private:
    std::vector<LvTree> children_;
    payload_t payload_;
};

class LvMap {
public:
    LvMap();
    ~LvMap();

    LvTree& lookup(const ValueDecl*) const;

    //void enter_scope();
    //void leave_scope();

    // TODO: rather use copy constructor?
    //LvMap duplicate() const;
    //void merge(const LvMap& other);

private:
    thorin::HashMap<const ValueDecl*, LvTree*> varmap_;
};

}

#endif
