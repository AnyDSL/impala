#ifndef IMPALA_SEMA_LVALUEMAP_H
#define IMPALA_SEMA_LVALUEMAP_H

#include <iostream>

#include "thorin/util/hash.h"

#include "impala/ast.h"

namespace impala {

typedef uint32_t varid;

//class LvaluePath {
//public:
//    LvaluePath();
//    ~LvaluePath();
//
//    void add_struct_filed(const char* name);
//    void add_deref();
//    void add_array_access();
//    void add_tuple_access();
//};

enum class LvComponentType { VAR, STRUCTFIELD, DEREF };

template <class T>
class LvTree {
public:
    LvComponentType get_type();
    const char* get_name();
    T get_payload();
    // TODO: use ArrayRef ?
    std::vector<LvTree> get_children();

private:
    std::vector<LvTree> children_;
    T payload_;
};

template <class T>
class LvMap {
public:
public:
    LvMap();
    ~LvMap();

    LvTree<T> lookup(varid id) const {
        auto tree = thorin::find(varmap_, id);
        if (tree == nullptr)
            // TODO: throw exception, assertion failure, etc.
        return tree;
    }

    //void enter_scope();
    //void leave_scope();

    // TODO: rather use copy constructor?
    LvMap duplicate() const;
    void merge(const LvMap& other);

private:
    thorin::HashMap<varid, LvTree<T>> varmap_;
};

template <class T>
const LvTree<T> PathExpr::getLvTree(const LvMap<T>& map) const {
    varid id = 0; // TODO: set this to the real id
    return map.lookup(id);
}
    
template <class T>
const LvTree<T> PrefixExpr::getLvTree(const LvMap<T>& map) const {
    // TODO: this is problematic because we need to use a mixture
    // of templatex and virtual functions here which is not allowed.
    //LvTree<T> rhs_tree = rhs()->getLvTree(map);
}

template <class T>
const LvTree<T> PostfixExpr::getLvTree(const LvMap<T>& map) const {
}

}

#endif
