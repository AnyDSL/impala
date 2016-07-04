#ifndef IMPALA_SEMA_LVTREE_H
#define IMPALA_SEMA_LVTREE_H

#include "thorin/util/hash.h"
#include "thorin/util/location.h"
#include "thorin/util/stream.h"

namespace impala {

/*
 * LvTree
 */

class LvTree : public thorin::Streamable {
public:
    LvTree(LvTree* parent): parent_(parent) {}
    ~LvTree() = default;

    const Payload& get_payload() const { return payload_; }
    void set_payload(payload_t pl, const thorin::Location& loc) { payload_.set_payload(pl, loc); }
    void set_payload_inherited(const thorin::Location& loc) { payload_.set_inherited(loc); }
    std::shared_ptr<LvTree> get_child(Symbol, bool);
    LvTree* get_parent(void) const { return parent_; }
    void set_parent(LvTree* parent) { parent_ = parent; };
    void update_child(Symbol, LvTree*);
    bool has_children() { return children_.size() > 0; }
    void remove_subtree(Symbol field);
    void clear_subtrees(void);
    void copy_subtrees(const LvTree*);
    LvTree* merge(LvTree*, bool, const LvMapComparator&);
    std::ostream& stream(std::ostream&) const;

private:
    LvTree* parent_;
    //payload_t payload_ = 0; // TODO: check for collisions
    Payload payload_;
    thorin::HashMap<Symbol, std::shared_ptr<LvTree>> children_;
};

// necessary because static initialization of DEREF_SYMBOL causes a segfault in Symbol because the
// hashmap it uses is not initialized yet
inline const Symbol& get_deref_symbol(void) {
    // TODO: make this a global variable
    std::unique_ptr<Symbol> DEREF_SYMBOL = nullptr;
    if (DEREF_SYMBOL == nullptr)
        DEREF_SYMBOL = std::unique_ptr<Symbol>(new Symbol("*"));
    return *DEREF_SYMBOL;
}

}

#endif
