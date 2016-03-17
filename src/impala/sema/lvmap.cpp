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
    void remove_subtree(Symbol field);
    void clear_subtrees(void);

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

void LvTree::remove_subtree(Symbol field) {
    children_.erase(field);
}

void LvTree::clear_subtrees(void) {
    children_.clear();
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
    , scope_stack_(std::stack<const ValueDecl*>())
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
    scope_stack_.push(decl);
}

void LvMap::enter_scope() {
    scope_stack_.push(nullptr);
}

void LvMap::leave_scope() {
    assert(!scope_stack_.empty());
    const ValueDecl* decl;
    do {
        decl = scope_stack_.top();
        scope_stack_.pop();
        varmap_.erase(decl);
    } while (decl != nullptr);
}

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
    assert(!create || this_tree != nullptr);
    if (this_tree == nullptr)
        return LvTreeLookupRes(parent_res.value_.tree_.get_payload());

    assert(this_tree->get_type() != LvComponentType::VAR);
    Relation comp_res = map.get_comparator().compare(parent_res.value_.tree_.get_payload(),
        this_tree->get_payload());
    assert(comp_res == Relation::LESS || (comp_res == Relation::EQUAL && create));
    return LvTreeLookupRes(*this_tree);
}
    
const LvTreeLookupRes PrefixExpr::lookup_lv_tree(LvMap& map, bool create) const {
    assert(kind() == PrefixExpr::MUL);
    return lookup_shared(map, create, *rhs(), true, get_deref_symbol());
}

const LvTreeLookupRes FieldExpr::lookup_lv_tree(LvMap& map, bool create) const {
    return lookup_shared(map, create, *lhs(), false, symbol());
}

const LvTreeLookupRes CastExpr::lookup_lv_tree(LvMap& map, bool create) const {
    return lhs()->lookup_lv_tree(map, create);
}

const LvTreeLookupRes MapExpr::lookup_lv_tree(LvMap& map, bool create) const {
    assert(is_lvalue());
    // handled like a deref expr
    return lookup_shared(map, create, *lhs(), true, get_deref_symbol());
}

//-------------------------------------------------------------------

/*
 * insert
 */

void insert(const Expr& expr, LvMap& map, payload_t pl) {
    // TODO: this is not super optimal because we might build up the tree only to tear it down
    // afterwards in the insertion
    LvTreeLookupRes res = expr.lookup_lv_tree(map, true);
    assert(res.is_tree_);
    expr.insert_payload(res.value_.tree_, map.get_comparator(), pl);
}

//-------------------------------------------------------------------

/*
 * insert_payload
 */

void PathExpr::insert_payload(LvTree& tree, const LvMapComparator& comp, payload_t pl) const {
    // TODO: maybe assert validity of tree here
    assert(tree.get_type() == LvComponentType::VAR);
    assert(tree.get_parent() == nullptr);
    tree.set_payload(pl);
}

void handle_ptr_insert(const Expr* parent_expr, LvTree& tree, const LvMapComparator& comp, payload_t pl) {
    assert(tree.get_type() == LvComponentType::DEREF);
    LvTree* parent = tree.get_parent();
    assert(parent != nullptr);

    tree.clear_subtrees();
    
    switch (comp.compare(pl, parent->get_payload())) {
        case Relation::LESS:
            parent->remove_subtree(get_deref_symbol());
            parent_expr->insert_payload(*parent, comp, pl);
            break;
        case Relation::GREATER:
            tree.set_payload(pl);
            break;
        case Relation::EQUAL:
            parent->remove_subtree(get_deref_symbol());
            break;
        case Relation::INCOMPARABLE:
            assert(false);
    }
}

void PrefixExpr::insert_payload(LvTree& tree, const LvMapComparator& comp, payload_t pl) const {
    assert(kind() == PrefixExpr::MUL);
    handle_ptr_insert(rhs(), tree, comp, pl);
}

const StructDecl* get_decl(const Type type) {
    // TODO: what is the difference between struct_abs and struct_app?
    //assert(lhs()->type()->kind() == Kind_struct_abs || lhs()->type()->kind() == Kind_struct_app);
    assert(type->kind() == Kind_struct_app);
    const TypeNode* node = *type;
    const StructAppTypeNode* app_node = dynamic_cast<const StructAppTypeNode*>(node);
    assert(app_node != nullptr);
    //const StructAbsTypeNode* struct_node = app_node->struct_abs_type().node();
    //const StructDecl* decl = struct_node->struct_decl();
    const StructDecl* decl = app_node->struct_abs_type().node()->struct_decl();
    return decl;
}

void FieldExpr::insert_payload(LvTree& tree, const LvMapComparator& comp, payload_t pl) const {
    assert(tree.get_type() == LvComponentType::FIELD);
    LvTree* parent = tree.get_parent();
    assert(parent != nullptr);

    // All children inherit the payload set for a subtree, so they can be deleted
    tree.clear_subtrees();

    switch (comp.compare(pl, parent->get_payload())) {
        case Relation::LESS: {
            const StructDecl* decl = get_decl(lhs()->type());
            payload_t parent_pl = parent->get_payload();
            for (auto i : decl->field_decls()) {
                Symbol s = i->symbol();
                LvTree* sibling = parent->get_field_child(s, false);
                if (sibling == nullptr) {
                    sibling = parent->get_field_child(s, true);
                    sibling->set_payload(parent_pl);
                }
                parent->remove_subtree(symbol());
                lhs()->insert_payload(*parent, comp, pl);
            }
            break;
        }
        case Relation::GREATER: {
            const StructDecl* decl = get_decl(lhs()->type());
            bool all_siblings_same_payload = true;
            for (auto i : decl->field_decls()) {
                Symbol s = i->symbol();
                if (s == symbol())
                    continue;
                LvTree* sibling = parent->get_field_child(s, false);

                // TODO: shouldn't this also check that the siblings have no children?
                if (sibling == nullptr || sibling->get_payload() != pl) {
                    all_siblings_same_payload = false;
                    break;
                }
            }
            if (all_siblings_same_payload) {
                parent->clear_subtrees();
                lhs()->insert_payload(*parent, comp, pl);
            } else
                tree.set_payload(pl);
            break;
        }
        case Relation::EQUAL:
            parent->remove_subtree(symbol());
            break;
        case Relation::INCOMPARABLE:
            assert(false); // must not happen
    }
}

void CastExpr::insert_payload(LvTree& tree, const LvMapComparator& comp, payload_t pl) const {
    lhs()->insert_payload(tree, comp, pl);
}

void MapExpr::insert_payload(LvTree& tree, const LvMapComparator& comp, payload_t pl) const {
    assert(is_lvalue());
    handle_ptr_insert(lhs(), tree, comp, pl);
}

//--------------------------------------------------------------------

}

