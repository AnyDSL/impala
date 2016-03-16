#include "impala/sema/lvmap.h"

#include "impala/ast.h"

#include <iostream>

namespace impala {

//---------------------------------------------------------------

/*
 * LvTree
 */

enum class LvComponentType: char { VAR, FIELD, DEREF };

class LvTree {
public:
    LvTree(LvComponentType, LvTree*);
    ~LvTree() {}

    LvComponentType get_type() const { return type_; }
    payload_t get_payload() const { return payload_; }
    void set_payload(payload_t pl) { payload_ = pl; }
    LvTree* get_pointer_child(bool create);
    LvTree* get_field_child(Symbol field, bool create);
    LvTree* get_parent(void) const { return parent_; }

private:
    const LvComponentType type_;
    LvTree* parent_;
    payload_t payload_ = 0; // TODO: check for collisions
    thorin::HashMap<Symbol, std::shared_ptr<LvTree>> children_;
};

// effectively const, since the field we care about cannot be modified
//LvTree ERROR_TREE = LvTree(LvComponentType::ERROR, nullptr);
//LvTree NOT_PRESENT_TREE = LvTree(LvComponentType::NOT_PRESENT, nullptr);

std::unique_ptr<Symbol> DEREF_SYMBOL = nullptr;

// necessary because static initialization of DEREF_SYMBOL causes a segfault in Symbol because the
// hashmap it uses is not initialized yet
inline const Symbol& get_deref_symbol(void) {
    if (DEREF_SYMBOL == nullptr)
        DEREF_SYMBOL = std::unique_ptr<Symbol>(new Symbol("*"));
    return *DEREF_SYMBOL;
}

template <class Key>
LvTree* find_tree(const thorin::HashMap<Key, std::shared_ptr<LvTree>>& map, Key key,
    LvComponentType expected_type) {
    // this code is copied from the find() function in thorin/util/hash.h because this function
    // is not compatible with std::shared_ptr
    auto i = map.find(key);
    if (i == map.end())
        return nullptr;
    std::shared_ptr<LvTree> tree = i->second;
    assert(tree->get_type() == expected_type);
    return tree.get();
}

template <class Key>
LvTree* create_tree(thorin::HashMap<Key, std::shared_ptr<LvTree>>& map, Key key,
    LvComponentType type, LvTree* parent) {
    assert(!map.contains(key));
    auto new_tree = std::shared_ptr<LvTree>(new LvTree(type, parent));
    map[key] = new_tree;
    return new_tree.get();
}

LvTree::LvTree(LvComponentType type, LvTree* parent)
    : type_(type)
    , parent_(parent)
    {}

LvTree* LvTree::get_pointer_child(bool create) {
    if (children_.size() == 0) {
        if (create)
            return create_tree<Symbol>(children_, get_deref_symbol(), LvComponentType::DEREF, this);
        else
            return nullptr;
    }
    assert(children_.size() == 1);
    LvTree* tree = find_tree<Symbol>(children_, get_deref_symbol(), LvComponentType::DEREF);
    assert(tree != nullptr);
    return tree;
}

LvTree* LvTree::get_field_child(Symbol field, bool create) {
    if (create && !children_.contains(field)) {
        LvTree* t = create_tree<Symbol>(children_, field, LvComponentType::FIELD, this);
        return t;
    }
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
    LvTree* tree = find_tree<const ValueDecl*>(varmap_, decl, LvComponentType::VAR);
    assert(tree != nullptr);
    return *tree;
}

void LvMap::insert(const ValueDecl* decl, payload_t pl) {
    assert (!varmap_.contains(decl));
    LvTree* tree = new LvTree(LvComponentType::VAR, nullptr);
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

payload_t lookup_payload(const Expr& expr, LvMap& map) {
    assert(expr.is_lvalue());
    auto res = expr.lookup_lv_tree(map, false);
    return res.is_tree_ ? res.value_.tree_.get_payload() : res.value_.implicit_payload_;
}

//---------------------------------------------------------

/*
 * lookup_lv_tree
 */

const LvTreeLookupRes PathExpr::lookup_lv_tree(LvMap& map, bool create) const {
    // TODO: are there valid cases where there is no mapping for the corresponding declaration?
    // TODO: check for invalid returned trees
    return LvTreeLookupRes(map.lookup(value_decl().get()));
}

LvTreeLookupRes lookup_shared(LvMap& map, bool create, const Expr& parent, bool lookup_pointer,
    const Symbol& symbol) {

    LvTreeLookupRes parent_res = parent.lookup_lv_tree(map, create);
    assert(!create || parent_res.is_tree_);
    if (!parent_res.is_tree_)
        return parent_res;

    LvTree* this_tree = lookup_pointer ? parent_res.value_.tree_.get_pointer_child(create)
        : parent_res.value_.tree_.get_field_child(symbol, create);
    if (this_tree == nullptr)
        return LvTreeLookupRes(parent_res.value_.tree_.get_payload());

    assert(this_tree->get_type() != LvComponentType::VAR);
    // TODO: assert type of this_tree
    assert(map.get_comparator().compare(parent_res.value_.tree_.get_payload(),
        this_tree->get_payload()) == Relation::LESS);
    return LvTreeLookupRes(*this_tree);
}
    
const LvTreeLookupRes PrefixExpr::lookup_lv_tree(LvMap& map, bool create) const {
    assert(kind() == PrefixExpr::MUL);
    return lookup_shared(map, create, *rhs(), true, get_deref_symbol());
}

const LvTreeLookupRes FieldExpr::lookup_lv_tree(LvMap& map, bool create) const {
    return lookup_shared(map, create, *lhs(), false, symbol());
}

const LvTreeLookupRes CastExpr::lookup_lv_tree(LvMap& map, bool) const {
    assert(false);
}

const LvTreeLookupRes MapExpr::lookup_lv_tree(LvMap& map, bool) const {
    assert(false);
}

//-------------------------------------------------------------------

/*
 * insert
 */

void insert(const Expr& expr, LvMap& map, payload_t pl) {
    LvTreeLookupRes res = expr.lookup_lv_tree(map, true);
    assert(res.is_tree_);
    if (res.value_.tree_.get_payload() != pl)
        expr.insert_payload(res.value_.tree_, pl);
}

//-------------------------------------------------------------------

/*
 * insert_payload
 */

void PathExpr::insert_payload(LvTree& tree, payload_t pl) const {
    // TODO: maybe assert validity of tree here
    assert(tree.get_type() == LvComponentType::VAR);
    assert(tree.get_parent() == nullptr);
    tree.set_payload(pl);
}

void PrefixExpr::insert_payload(LvTree& tree, payload_t) const {
}

void FieldExpr::insert_payload(LvTree& tree, payload_t) const {
    assert(tree.get_type() == LvComponentType::FIELD);
    assert(tree.get_parent() != nullptr);
    LvTree& parent = *tree.get_parent();
    // TODO: what is the difference between struct_abs and struct_app?
    assert(lhs()->type()->kind() == Kind_struct_abs); //|| lhs()->type()->kind() == Kind_struct_app);

}

void CastExpr::insert_payload(LvTree& tree, payload_t) const {
}

void MapExpr::insert_payload(LvTree& tree, payload_t) const {
}

//--------------------------------------------------------------------

}

