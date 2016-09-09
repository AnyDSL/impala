#include "impala/sema/lvmap.h"

#include "impala/ast.h"
//TODO: get rid of this import, only needed for error()
#include "impala/impala.h"
#include "impala/sema/lvtree.h"

#include <iostream>

namespace impala {

const unsigned MULTI_REF_THRESHOLD = 2;

//---------------------------------------------------------------

/*
 * Payload
 */

void Payload::set_payload(payload_t pl, const thorin::Location& loc) {
    assert(pl != INHERITED_PAYLOAD);
    value_ = pl;
    set_loc(loc);
}

void Payload::set_inherited(const thorin::Location& loc) {
    value_ = INHERITED_PAYLOAD;
    set_loc(loc);
}

//---------------------------------------------------------------

template <class Key>
std::shared_ptr<LvTree> find_tree(const thorin::HashMap<Key, std::shared_ptr<LvTree>>& map, Key key) {
    // this code is copied from the find() function in thorin/util/hash.h because this function
    // is not compatible with std::shared_ptr
    auto i = map.find(key);
    if (i == map.end())
        return nullptr;
    return i->second;
}

template <class Key>
std::shared_ptr<LvTree> create_tree(thorin::HashMap<Key, std::shared_ptr<LvTree>>& map, Key key, LvTree* parent) {
    assert(!map.contains(key));
    auto new_tree = std::shared_ptr<LvTree>(new LvTree(parent));
    map[key] = new_tree;
    return new_tree;
}

std::shared_ptr<LvTree> LvTree::get_child(Symbol sym, bool create) {
    if (sym == get_deref_symbol())
        assert(children_.size() == 0 || (children_.size() == 1 && children_.contains(sym)));
    else
        assert(!children_.contains(get_deref_symbol()));

    if (create && !children_.contains(sym)) {
        auto t = create_tree<Symbol>(children_, sym, this);
        return t;
    }
    return find_tree<Symbol>(children_, sym);
}

void LvTree::update_child(Symbol s, LvTree* tree) {
    assert(children_.contains(s));
    children_[s] = std::shared_ptr<LvTree>(tree);
}

void LvTree::add_child(Symbol s, std::shared_ptr<LvTree> child) {
    assert(!children_.contains(s));
    // TODO assert children sanity
    children_[s] = child;
}

void LvTree::remove_subtree(Symbol field) {
    children_.erase(field);
}

void LvTree::clear_subtrees(void) {
    children_.clear();
}

void LvTree::copy_subtrees(const LvTree* other) {
    children_ = other->children_;
}

LvTree* LvTree::merge(LvTree* other, bool multi_ref, const LvMapComparator& comp) {
    if (this == other)
        // we are merging together the same trees, no need to do anything
        return nullptr;
    // TODO: assert something about the children like both pointer or struct field
  
    bool has_changed = false;
    // TODO: this is not so great because there might be different trees with equal structure and
    // payloads, in this case we create a new one but we actually wouldn't need to do that
   
    // TODO: is reinserting ok? i.e. not a multimap? 
    LvTree* res_tree = multi_ref ? new LvTree(nullptr) : this;

    // iterate over other to avoid changing this children_ map while we iterate over it
    for (auto i : other->children_) {
        if (children_.contains(i.first)) {
            auto child = children_[i.first];
            bool child_multi_ref = multi_ref || !child.unique();
            LvTree* child_res = child->merge(i.second.get(), child_multi_ref, comp);
            if (child_res == nullptr && multi_ref)
                res_tree->children_[i.first] = child;
            else {
                child_res->parent_ = res_tree;
                res_tree->children_[i.first] = std::shared_ptr<LvTree>(child_res);
                has_changed = true;
            }
        } else {
            res_tree->children_[i.first] = i.second;
            has_changed = true;
        }
    }
    for (auto i : children_) {
        if (!other->children_.contains(i.first) && multi_ref)
            res_tree->children_[i.first] = i.second;
    }

    payload_t infimum = comp.infimum(payload_.get_value(), other->payload_.get_value());
    has_changed |= infimum != payload_.get_value();

    // TODO: run simplification steps on the new children
    
    if (multi_ref && !has_changed) {
        // we inserted everything into a new tree but there were no changes, so we do not
        // need a new tree
        //children_ = res_tree->children_;
        delete res_tree;
        res_tree = this;
    }

    const thorin::Location& loc = infimum == payload_.get_value() ?
        payload_.loc() : other->payload_.loc();
    // TODO: change location computation once the infimum is not the minimum anymore
    res_tree->set_payload(infimum, loc);

    if (multi_ref && has_changed)
        return res_tree;
    return nullptr;
}

inline void print_tree(std::ostream& os, Symbol sym, std::shared_ptr<LvTree> tree) {
    os << "(" << sym << ", " << tree->get_payload().get_value() << ", <";
    os << tree.use_count() - 2 << ">, " << tree.get() << "), ";
    // -2 for the ref count because there are two copies of the shared pointer for printing
}

std::ostream& LvTree::stream(std::ostream& os) const {
    os << "[";
    for (auto i : children_) {
        print_tree(os, i.first, i.second);
    }
    os << "]";
    return os;
}

//---------------------------------------------------------------

/*
 * LvTreeLookupRes
 */

LvTreeLookupRes::LvTreeLookupRes(std::shared_ptr<LvTree> tree, bool multi_ref, const Payload& pl)
    : tree_(tree)
    , is_multi_ref_(multi_ref)
    , payload_(pl)
    {
        assert(tree_.use_count() > 1);
        assert(tree->get_payload().is_inherited());
    }

LvTreeLookupRes::LvTreeLookupRes(std::shared_ptr<LvTree> tree, bool multi_ref)
    : tree_(tree)
    , is_multi_ref_(multi_ref)
    , payload_(tree->get_payload())
    {
        assert(tree_.use_count() > 1);
        assert(!payload_.is_inherited());
    }

LvTreeLookupRes::LvTreeLookupRes(const Payload& pl)
    : tree_(nullptr)
    , is_multi_ref_(false)
    , payload_(pl)
    {
        assert(!payload_.is_inherited());
    }
 
//-------------------------------------------------------------

/*
 * LvMapComparator
 */

Relation LvMapComparator::compare(payload_t p1, payload_t p2) const {
    assert(p1 >= 0 && p2 >= 0); // TODO: remove this restriction
    return p1 > p2 ? Relation::LESS : p1 < p2 ? Relation::GREATER : Relation::EQUAL;
}

payload_t LvMapComparator::infimum(payload_t p1, payload_t p2) const {
    switch (compare(p1, p2)) {
        case Relation::LESS:
            return p1;
        case Relation::GREATER:
        case Relation::EQUAL:
            return p2;
        default:
            // TODO: support this
            assert(false);
    }
}

//-------------------------------------------------------------

/*
 * LvMap
 */

LvMap::LvMap(const LvMapComparator& comparator)
    : varmap_(ScopedMap<std::shared_ptr<LvTree>>())
    , comparator_(comparator) // TODO: maybe change this to be a pointer
    {}

LvMap::LvMap(const LvMap& map)
    // TODO: check that shared pointers get copied correctly
    : varmap_(map.varmap_)
    , comparator_(map.comparator_)
    {}

LvMap::~LvMap() {
    // TODO: delete the trees
}

std::shared_ptr<LvTree> LvMap::lookup(const ValueDecl* decl) const {
    auto tree = find_tree<const ValueDecl*>(varmap_, decl);
    assert(tree != nullptr);
    return tree;
}

void LvMap::insert(const ValueDecl* decl, payload_t pl, const thorin::Location& loc) {
    assert (!varmap_.contains(decl));
    LvTree* tree = new LvTree(nullptr);
    tree->set_payload(pl, loc);
    varmap_.add_mapping(decl, std::shared_ptr<LvTree>(tree));
}

void LvMap::update(const ValueDecl* decl, LvTree* tree) {
    assert(varmap_.contains(decl));
    varmap_[decl] = std::shared_ptr<LvTree>(tree);
}

void LvMap::enter_scope() {
    varmap_.enter_scope();
}

void LvMap::leave_scope() {
    varmap_.leave_scope();
}

void LvMap::merge(LvMap& other) {
    assert(varmap_.size() == other.varmap_.size());
    // TODO: assert same comparator

    for (auto i : other.varmap_) {
        assert(varmap_.contains(i.first));
        auto tree = varmap_[i.first];
        LvTree* new_tree = tree->merge(i.second.get(), !tree.unique(),
            get_comparator());
        if (new_tree != nullptr) {
            varmap_[i.first] = std::shared_ptr<LvTree>(new_tree);
        }
    }

    // make other unusable
    other.varmap_.clear();
}

std::ostream& LvMap::stream(std::ostream& os) const {
    os << "[";
    for (auto i : varmap_) {
        os << "\n\t";
        print_tree(os, i.first->symbol(), i.second);
    }
    os << "\n]";
    return os;
}


//-------------------------------------------------------

const Payload& lookup(const Expr* expr, LvMap& map) {
    auto res = expr->lookup_lv_tree(map, false);
    return res.payload_;
}

//---------------------------------------------------------

/*
 * lookup_lv_tree
 */

const LvTreeLookupRes PathExpr::lookup_lv_tree(LvMap& map, bool create) const {
    // TODO: are there valid cases where there is no mapping for the corresponding declaration?
    // TODO: check for invalid returned trees
    std::shared_ptr<LvTree> res = map.lookup(value_decl().get());
    assert(res != nullptr);
    //assert(res.tree_->get_parent() == nullptr);
    assert(!res->get_payload().is_inherited());
    bool multi_ref = res.use_count() > MULTI_REF_THRESHOLD;
    return LvTreeLookupRes(res, multi_ref);
    // we need to use > 2 here because of the copy of the shared pointer in this function
}

LvTreeLookupRes lookup_shared(LvMap& map, bool create, const Expr& parent,
    const Symbol& symbol) {

    LvTreeLookupRes parent_res = parent.lookup_lv_tree(map, create);
    assert(!create || parent_res.tree_ != nullptr);
    if (parent_res.tree_ == nullptr)
        return parent_res;

    std::shared_ptr<LvTree> this_tree = parent_res.tree_->get_child(symbol, create);
    assert(!create || this_tree != nullptr);
    if (this_tree == nullptr) {
        assert(!parent_res.payload_.is_inherited());
        return LvTreeLookupRes(parent_res.payload_);
    }
    
    bool multi_ref = parent_res.is_multi_ref_ || this_tree.use_count() > MULTI_REF_THRESHOLD;
    // set the parent for this lookup, necessary because of COW pattern that results in a DAG
    this_tree->set_parent(parent_res.tree_.get());

    if (this_tree->get_payload().is_inherited()) {
        assert(create || this_tree->has_children());
        return LvTreeLookupRes(this_tree, multi_ref, parent_res.payload_);
    } else {
        assert(map.get_comparator().compare(parent_res.payload_.get_value(),
            this_tree->get_payload().get_value()) == Relation::LESS);
        return LvTreeLookupRes(this_tree, multi_ref);
    }
}
    
const LvTreeLookupRes PrefixExpr::lookup_lv_tree(LvMap& map, bool create) const {
    switch (kind()) {
        case PrefixExpr::MUL:
            return lookup_shared(map, create, *rhs(), get_deref_symbol());
        case PrefixExpr::AND: {
            assert(!create);
            // TODO: support create and build a separate tree with a new root whose 
            // only deref child is the lookup res of the parent expr (rhs)
            LvTreeLookupRes parent_res = rhs()->lookup_lv_tree(map, create);
            if (parent_res.tree_ == nullptr)
                return parent_res;
            else {
                assert(false);
                //LvTree* new_root = new LvTree(nullptr);
                // TODO: set child
                return LvTreeLookupRes(parent_res.payload_);
            }
        }
        default:
            assert(false);
    }
}

const LvTreeLookupRes FieldExpr::lookup_lv_tree(LvMap& map, bool create) const {
    return lookup_shared(map, create, *lhs(), symbol());
}

const LvTreeLookupRes CastExpr::lookup_lv_tree(LvMap& map, bool create) const {
    return lhs()->lookup_lv_tree(map, create);
}

const LvTreeLookupRes MapExpr::lookup_lv_tree(LvMap& map, bool create) const {
    assert(is_lvalue());
    // handled like a deref expr
    return lookup_shared(map, create, *lhs(), get_deref_symbol());
}

//-------------------------------------------------------------------

/*
 * insert
 */

void insert(const Expr* expr, LvMap& map, payload_t pl, const thorin::Location& loc) {
    // TODO: this is not super optimal because we might build up the tree only to tear it down
    // afterwards in the insertion
    //std::cout << "inserting payload for: " << expr << " in map:\n";
    //std::cout << &map << "\n";
    LvTreeLookupRes res = expr->lookup_lv_tree(map, true);
    // TODO: try to insert only if the payload is changed
    assert(res.tree_ != nullptr);
    
    LvTree* tree = res.tree_.get();
    bool multi_ref = res.is_multi_ref_;
    if (multi_ref) {
        // TODO: and change
        // TODO: check that no memory leaks
        tree = new LvTree(*tree);
    }
    tree->clear_subtrees();
    expr->insert_payload(tree, multi_ref, map, pl, loc);
}

//-------------------------------------------------------------------

/*
 * insert_payload
 */

void PathExpr::insert_payload(LvTree* tree, bool multi_ref, LvMap& map, payload_t pl,
    const thorin::Location& loc) const {
    // TODO: maybe assert validity of tree here
    assert(tree->get_parent() == nullptr);

    tree->set_payload(pl, loc);
    if (multi_ref)
        map.update(value_decl().get(), tree);
    // TODO: do not always update, just when something changed
}

inline void propagate_change(const Expr* parent_expr, LvTree* parent_tree, bool multi_ref, LvMap& map) {
    if (multi_ref)
        parent_expr->insert_payload(parent_tree, true, map, parent_tree->get_payload().get_value(),
            parent_tree->get_payload().loc());
}

LvTree* init_parent(LvTree* tree, bool multi_ref, Symbol parent_symbol) {
    assert(tree->get_parent() != nullptr);
    LvTree* parent = multi_ref ? new LvTree(*tree->get_parent()) : tree->get_parent();
    if (multi_ref) {
        tree->set_parent(parent); // TODO: probably not necessary
        parent->update_child(parent_symbol, tree);
    }
    return parent;
}

payload_t get_parent_pl(LvTree* parent) {
    while (parent->get_payload().is_inherited()) {
        parent = parent->get_parent();
        assert(parent != nullptr);
    }
    return parent->get_payload().get_value();
}

void handle_ptr_insert(const Expr* parent_expr, LvTree* tree, bool multi_ref, LvMap& map,
    payload_t pl, const thorin::Location& loc) {

    LvTree* parent = init_parent(tree, multi_ref, get_deref_symbol());
    payload_t parent_pl = get_parent_pl(parent);

    switch (map.get_comparator().compare(pl, parent_pl)) {
        case Relation::LESS:
            if (!tree->has_children())
                parent->remove_subtree(get_deref_symbol());
            parent_expr->insert_payload(parent, multi_ref, map, pl, loc);
            break;
        case Relation::GREATER:
            tree->set_payload(pl, loc);
            propagate_change(parent_expr, parent, multi_ref, map);
            break;
        case Relation::EQUAL:
            if (!tree->has_children())
                parent->remove_subtree(get_deref_symbol());
            else
                tree->set_payload_inherited(loc);
            propagate_change(parent_expr, parent, multi_ref, map);
            break;
        case Relation::INCOMPARABLE:
            assert(false);
    }
}

void PrefixExpr::insert_payload(LvTree* tree, bool multi_ref, LvMap& map, payload_t pl,
    const thorin::Location& loc) const {
    assert(kind() == PrefixExpr::MUL);
    handle_ptr_insert(rhs(), tree, multi_ref, map, pl, loc);
}

const StructDecl* get_struct_decl(const Type type) {
    assert(type->kind() == Kind_struct_app);
    const TypeNode* node = *type;
    const StructAppTypeNode* app_node = dynamic_cast<const StructAppTypeNode*>(node);
    assert(app_node != nullptr);
    //const StructAbsTypeNode* struct_node = app_node->struct_abs_type().node();
    //const StructDecl* decl = struct_node->struct_decl();
    const StructDecl* decl = app_node->struct_abs_type().node()->struct_decl();
    return decl;
}

void FieldExpr::insert_payload(LvTree* tree, bool multi_ref, LvMap& map, payload_t pl,
    const thorin::Location& loc) const {

    LvTree* parent = init_parent(tree, multi_ref, symbol());
    payload_t parent_pl = get_parent_pl(parent);

    switch (map.get_comparator().compare(pl, parent_pl)) {
        case Relation::LESS: {
            const StructDecl* decl = get_struct_decl(lhs()->type());
            for (auto i : decl->field_decls()) {
                Symbol s = i->symbol();
                std::shared_ptr<LvTree> sibling = parent->get_child(s, false);
                if (sibling == nullptr) {
                    sibling = parent->get_child(s, true);
                    sibling->set_payload(parent_pl, loc);
                }
            }

            if (!tree->has_children())
                parent->remove_subtree(symbol());
            else
                tree->set_payload_inherited(loc);
            lhs()->insert_payload(parent, multi_ref, map, pl, loc);
            break;
        }
        case Relation::GREATER: {
            const StructDecl* decl = get_struct_decl(lhs()->type());
            bool all_siblings_same_payload = true;
            for (auto i : decl->field_decls()) {
                Symbol s = i->symbol();
                if (s == symbol())
                    continue;
                std::shared_ptr<LvTree> sibling = parent->get_child(s, false);

                if (sibling == nullptr || sibling->get_payload().get_value() != pl
                        || sibling->has_children()) {
                    // TODO: do not set to false if siblings have children
                    all_siblings_same_payload = false;
                    break;
                }
            }
            if (all_siblings_same_payload) {
                // TODO: remove only those children that don't have children
                parent->clear_subtrees();
                lhs()->insert_payload(parent, multi_ref, map, pl, loc);
            } else {
                tree->set_payload(pl, loc);
                propagate_change(lhs(), parent, multi_ref, map);
            }
            break;
        }
        case Relation::EQUAL:
            if (!tree->has_children())
                parent->remove_subtree(symbol());
            else
                tree->set_payload_inherited(loc);
            propagate_change(lhs(), parent, multi_ref, map);
            break;
        case Relation::INCOMPARABLE:
            assert(false); // must not happen
    }
}

void CastExpr::insert_payload(LvTree* tree, bool multi_ref, LvMap& map, payload_t pl,
    const thorin::Location& loc) const {
    lhs()->insert_payload(tree, multi_ref, map, pl, loc);
}

void MapExpr::insert_payload(LvTree* tree, bool multi_ref, LvMap& map, payload_t pl,
    const thorin::Location& loc) const {
    assert(is_lvalue());
    handle_ptr_insert(lhs(), tree, multi_ref, map, pl, loc);
}

//--------------------------------------------------------------------

}

