#include "impala/sema/lvmap.h"

#include "impala/ast.h"

#include <iostream>

namespace impala {

//---------------------------------------------------------------

/*
 * LvTree
 */

enum class LvComponentType: char { VAR, FIELD, DEREF, ERROR, NOT_PRESENT};

class LvTree {
public:
    LvTree(LvComponentType);
    ~LvTree() {}

    LvComponentType get_type() const { return type_; }
    payload_t get_payload() const { return payload_; }
    void set_payload(payload_t pl) { payload_ = pl; }
    LvTree& get_pointer_child(bool create);
    LvTree& get_field_child(Symbol field, bool create);

private:
    const LvComponentType type_;
    payload_t payload_ = 0; // TODO: check for collisions
    thorin::HashMap<Symbol, std::shared_ptr<LvTree>> children_;
};

// effectively const, since the field we care about cannot be modified
LvTree ERROR_TREE = LvTree(LvComponentType::ERROR);
LvTree NOT_PRESENT_TREE = LvTree(LvComponentType::NOT_PRESENT);

std::unique_ptr<Symbol> DEREF_SYMBOL = nullptr;

// necessary because static initialization of DEREF_SYMBOL causes a segfault in Symbol because the
// hashmap it uses is not initialized yet
inline const Symbol& get_deref_symbol(void) {
    if (DEREF_SYMBOL == nullptr)
        DEREF_SYMBOL = std::unique_ptr<Symbol>(new Symbol("*"));
    return *DEREF_SYMBOL;
}

template <class Key>
LvTree& find_tree(const thorin::HashMap<Key, std::shared_ptr<LvTree>> map, Key key,
    LvComponentType expected_type) {
    // this code is copied from the find() function in thorin/util/hash.h because this function
    // is not compatible with std::shared_ptr
    auto i = map.find(key);
    if (i == map.end())
        return NOT_PRESENT_TREE;
    std::shared_ptr<LvTree> tree = i->second;
    assert(tree->get_type() == expected_type);
    return *tree;
}

template <class Key>
LvTree& create_tree(thorin::HashMap<Key, std::shared_ptr<LvTree>> map, Key key,
    LvComponentType type) {
    assert(!map.contains(key));
    auto new_tree = std::shared_ptr<LvTree>(new LvTree(type));
    map[key] = new_tree;
    return *new_tree;
}

LvTree::LvTree(LvComponentType type)
    : type_(type)
    {}

LvTree& LvTree::get_pointer_child(bool create) {
    if (children_.size() == 0) {
        if (create) {
            return create_tree<Symbol>(children_, get_deref_symbol(), LvComponentType::DEREF);
        } else
            return NOT_PRESENT_TREE;
    }
    assert(children_.size() == 1);
    LvTree& tree = find_tree<Symbol>(children_, get_deref_symbol(), LvComponentType::DEREF);
    assert(tree.get_type() == LvComponentType::DEREF);
    return tree;
}

LvTree& LvTree::get_field_child(Symbol field, bool create) {
    if (create && !children_.contains(field))
        return create_tree<Symbol>(children_, field, LvComponentType::FIELD);
    if (children_.size() == 0)
        // NOTE: create == false here
        return NOT_PRESENT_TREE;
    return find_tree<Symbol>(children_, field, LvComponentType::FIELD);
}

//-------------------------------------------------------------

/*
 * LvMapComparator
 */

Relation LvMapComparator::compare(payload_t p1, payload_t p2) const {
    assert(p1 >= 0 && p2 >= 0); // TODO: remove this restriction
    return p1 > p2 ? Relation::LESS : p1 < p2 ? Relation::GREATER : Relation::EQUAL;
}

//-------------------------------------------------------------

/*
 * LvMap
 */

LvMap::LvMap(LvMapComparator comparator)
    : varmap_(thorin::HashMap<const ValueDecl*, std::shared_ptr<LvTree>>())
    , comparator_(comparator) // TODO: eliminate copy
    {}

LvMap::~LvMap() {
    // TODO: delete the trees
}

LvTree& LvMap::lookup(const ValueDecl* decl) const {
    LvTree& tree = find_tree<const ValueDecl*>(varmap_, decl, LvComponentType::VAR);
    assert(tree.get_type() != LvComponentType::NOT_PRESENT);
    return tree;
}

void LvMap::insert(const ValueDecl* decl, payload_t pl) {
    assert (!varmap_.contains(decl));
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
    assert(is_lvalue());
    auto res = lookup_lv_tree(map);
    switch (res.type_) {
        case LvTreeLookupResType::TREE:          return res.value_.tree_.get_payload();
        case LvTreeLookupResType::NOT_EXPLICIT:  return res.value_.implicit_payload_;
        case LvTreeLookupResType::ERROR:         return 0; // TODO: make sure this does not collide with anything
    }
    return 0;
}

//---------------------------------------------------------

/*
 * lookup_lv_tree
 */

const LvTreeLookupRes PathExpr::lookup_lv_tree(const LvMap& map) const {
    // TODO: are there valid cases where there is no mapping for the corresponding declaration?
    // TODO: check for invalid returned trees
    return LvTreeLookupRes(map.lookup(value_decl().get()));
}
    
const LvTreeLookupRes PrefixExpr::lookup_lv_tree(const LvMap& map) const {
    assert(kind() == PrefixExpr::MUL);
    LvTreeLookupRes parent_res = rhs()->lookup_lv_tree(map);
    switch (parent_res.type_) {
        case LvTreeLookupResType::TREE: {
            LvTree& this_tree = parent_res.value_.tree_.get_pointer_child(false);
            switch (this_tree.get_type()) {
                case LvComponentType::DEREF:
                    // TODO: assert > parent payload
                    return LvTreeLookupRes(this_tree);
                case LvComponentType::NOT_PRESENT:
                    return LvTreeLookupRes(parent_res.value_.tree_.get_payload());
                default:
                    assert(false); // should not happen
            }
        }
        default:
            return parent_res;
    }
}

// TODO: code of this function is similar to the one above, can this be cleaned up?
const LvTreeLookupRes FieldExpr::lookup_lv_tree(const LvMap& map) const {
    LvTreeLookupRes parent_res = lhs()->lookup_lv_tree(map);
    switch (parent_res.type_) {
        case LvTreeLookupResType::TREE: {
            LvTree& this_tree = parent_res.value_.tree_.get_field_child(symbol(), false);
            switch (this_tree.get_type()) {
                case LvComponentType::FIELD:
                    // TODO: assert > parent payload
                    return LvTreeLookupRes(this_tree);
                case LvComponentType::NOT_PRESENT: {
                    return LvTreeLookupRes(parent_res.value_.tree_.get_payload());
                }
                default:
                    assert(false);
            }
        }
        default:
            return parent_res;
    }
}

const LvTreeLookupRes CastExpr::lookup_lv_tree(const LvMap& map) const {
    assert(false);
    return LvTreeLookupRes();
}

const LvTreeLookupRes MapExpr::lookup_lv_tree(const LvMap& map) const {
    assert(false);
    return LvTreeLookupRes();
}

//-------------------------------------------------------------------

void Expr::insert_payload(LvMap& map, payload_t pl) const {
    insert_lv_payload(map, pl);
}

//-------------------------------------------------------------------

/*
 * insert_lv_payload
 */

LvTree& PathExpr::insert_lv_payload(LvMap& map, payload_t pl) const {
    const ValueDecl* decl = value_decl().get();
    LvTree& tree = map.lookup(decl);
    // TODO: maybe assert validity of tree here
    tree.set_payload(pl);
    return tree;
}

LvTree& PrefixExpr::insert_lv_payload(LvMap& map, payload_t) const {
}

LvTree& FieldExpr::insert_lv_payload(LvMap& map, payload_t) const {

}

LvTree& CastExpr::insert_lv_payload(LvMap& map, payload_t) const {
}

LvTree& MapExpr::insert_lv_payload(LvMap& map, payload_t) const {
}

//---------------------------------------------------------------------

}

