#include "impala/sema/lvmap.h"

#include "impala/ast.h"

#include <iostream>

namespace impala {

//---------------------------------------------------------------

const char* LvTree::get_name() const { return ""; }

std::vector<LvTree> LvTree::get_children() { return std::vector<LvTree>(); }

//-------------------------------------------------------------

LvMap::LvMap()
    : varmap_(thorin::HashMap<const ValueDecl*, std::shared_ptr<LvTree>>())
    {}

LvMap::~LvMap() {
    // TODO: delete the trees
}

LvTree& LvMap::lookup(const ValueDecl* decl) const {
    // this code is copied from the find() function in thorin/util/hash.h because this function
    // is not compatible with std::shared_ptr
    auto i = varmap_.find(decl);
    assert (i != varmap_.end()); // there has to be a declaration
    std::shared_ptr<LvTree> tree = i->second;
    return *tree;
}

void LvMap::insert(const ValueDecl* decl, payload_t pl) {
    assert (varmap_.find(decl) == varmap_.end());
    LvTree* tree = new LvTree(LvComponentType::VAR);
    tree->set_payload(pl);
    varmap_[decl] = std::shared_ptr<LvTree>(tree);
}

//void LvMap::enter_scope() {}
//
//void LvMap::leave_scope() {}

//LvMap LvMap::duplicate() const { return LvMap() }
//
//void LvMap::merge(const LvMap& other) {}

//-------------------------------------------------------

payload_t Expr::lookup_payload(const LvMap& map) const {
    //assert(is_lvalue());
    const LvTree& tree = lookup_lv_tree(map);
    // TODO: do some kind of error checking here
    return tree.get_payload();
}

const LvTree& PathExpr::lookup_lv_tree(const LvMap& map) const {
    return map.lookup(value_decl().get());
}
    
const LvTree& PrefixExpr::lookup_lv_tree(const LvMap& map) const {
    const LvTree& rhs_tree = rhs()->lookup_lv_tree(map);
}

const LvTree& CastExpr::lookup_lv_tree(const LvMap& map) const {
}

const LvTree& MapExpr::lookup_lv_tree(const LvMap& map) const {
}

void Expr::insert_payload(LvMap& map, payload_t pl) const {
    insert_lv_payload(map, pl);
}

LvTree& PathExpr::insert_lv_payload(LvMap& map, payload_t pl) const {
    const ValueDecl* decl = value_decl().get();
    LvTree& tree = map.lookup(decl);
    // TODO: maybe assert validity of tree here
    tree.set_payload(pl);
    return tree;
}

}

