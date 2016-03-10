#include "impala/sema/lvmap.h"

#include "impala/ast.h"

namespace impala {

//---------------------------------------------------------------

LvComponentType LvTree::get_type() const { return LvComponentType::VAR; }

const char* LvTree::get_name() const { return ""; }

payload_t LvTree::get_payload() const { return 0; }

std::vector<LvTree> LvTree::get_children() { return std::vector<LvTree>(); }

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

payload_t Expr::lookup_payload(const LvMap& map) const {
    assert(is_lvalue());
    const LvTree& tree = lookup_lv_tree(map);
    // TODO: do some kind of error checking here
    return tree.get_payload();
}

const LvTree& PathExpr::lookup_lv_tree(const LvMap& map) const {
    auto decl = value_decl().get();
    return map.lookup(decl);
}
    
const LvTree& PrefixExpr::lookup_lv_tree(const LvMap& map) const {
    LvTree rhs_tree = rhs()->lookup_lv_tree(map);
}

const LvTree& CastExpr::lookup_lv_tree(const LvMap& map) const {
}

const LvTree& MapExpr::lookup_lv_tree(const LvMap& map) const {
}

}

