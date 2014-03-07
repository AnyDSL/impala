#include "impala/ast.h"
#include "impala/sema/errorhandler.h"

namespace impala {

//------------------------------------------------------------------------------

class NameSema : public ErrorHandler {
public:
    /** 
     * @brief Looks up the current definition of \p symbol.
     * @return Returns nullptr on failure.
     */
    const Decl* lookup(Symbol symbol) const;

    /// Lookups \p symbol and reports an error at location of \p n if was not found
    const Decl* lookup(const ASTNode* n, Symbol);

    /** 
     * @brief Maps \p decl's symbol to \p decl.
     * 
     * If \p decl's symbol already has a definition in the current scope, an error will be emitted.
     */
    void insert(const Decl* decl);

    /** 
     * @brief Checks whether there already exists a \p Symbol \p symbol in the \em current scope.
     * @param symbol The \p Symbol to check.
     * @return The current mapping if the lookup succeeds, nullptr otherwise.
     */
    const Decl* clash(Symbol symbol) const;
    void push_scope() { levels_.push_back(decl_stack_.size()); } ///< Opens a new scope.
    void pop_scope();                                            ///< Discards current scope.

private:
    size_t depth() const { return levels_.size(); }

    thorin::HashMap<Symbol, const Decl*> symbol2decl_;
    std::vector<const Decl*> decl_stack_;
    std::vector<size_t> levels_;
};

//------------------------------------------------------------------------------

const Decl* NameSema::lookup(Symbol symbol) const {
    auto i = symbol2decl_.find(symbol);
    return i != symbol2decl_.end() ? i->second : nullptr;
}

const Decl* NameSema::lookup(const ASTNode* n, Symbol symbol) {
    auto decl = lookup(symbol);
    if (decl == nullptr)
        error(n) << '\'' << symbol << "' not found in current scope\n";
    return decl;
}

void NameSema::insert(const Decl* decl) {
    if (const Decl* other = clash(decl->symbol())) {
        error(decl) << "symbol '" << decl->symbol() << "' already defined\n";
        error(other) << "previous location here\n";
        return;
    }

    Symbol symbol = decl->symbol();
    assert(clash(symbol) == nullptr && "must not be found");

    auto i = symbol2decl_.find(symbol);
    decl->shadows_ = i != symbol2decl_.end() ? i->second : nullptr;
    decl->depth_ = depth();
    decl_stack_.push_back(decl);
    symbol2decl_[symbol] = decl;
}

const Decl* NameSema::clash(Symbol symbol) const {
    if (auto decl = thorin::find(symbol2decl_, symbol))
        return (decl && decl->depth() == depth()) ? decl : nullptr;
    return nullptr;
}

void NameSema::pop_scope() {
    size_t level = levels_.back();
    for (size_t i = level, e = decl_stack_.size(); i != e; ++i) {
        const Decl* decl = decl_stack_[i];
        symbol2decl_[decl->symbol()] = decl->shadows();
    }

    decl_stack_.resize(level);
    levels_.pop_back();
}

//------------------------------------------------------------------------------

}
