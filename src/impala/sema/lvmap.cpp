#include "impala/sema/lvmap.h"

namespace impala {

//---------------------------------------------------------------

LvComponentType LvTree::get_type() {}

const char* LvTree::get_name() {}

payload_t get_payload() {}

std::vector<LvTree> get_children() {}

//-------------------------------------------------------------

LvMap::LvMap() {}

LvMap::~LvMap() {}

LvTree& LvMap::lookup(const ValueDecl* decl) const {
    LvTree* tree = thorin::find(varmap_, decl);
    if (tree == nullptr)
        // TODO: throw exception, assertion failure, etc.
    return *tree;
}

//void LvMap::enter_scope() {}
//
//void LvMap::leave_scope() {}

//LvMap LvMap::duplicate() const { return LvMap() }
//
//void LvMap::merge(const LvMap& other) {}

//-------------------------------------------------------

const LvTree& PathExpr::getLvTree(const LvMap& map) const {
    auto decl = value_decl().get();
    return map.lookup(decl);
}
    
const LvTree& PrefixExpr::getLvTree(const LvMap& map) const {
    LvTree rhs_tree = rhs()->getLvTree(map);
}

const LvTree& CastExpr::getLvTree(const LvMap& map) const {
}

const LvTree& MapExpr::getLvTree(const LvMap& map) const {
}

}

