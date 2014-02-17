#ifndef IMPALA_SEMA_SCOPETABLE_H
#define IMPALA_SEMA_SCOPETABLE_H

#include <unordered_map>
#include <vector>

#include "impala/symbol.h"

namespace impala {

class ASTNode;
class Decl;
class Location;

class ScopeTable {
public:
    ScopeTable()
        : result_(true)
    {}

    /** 
     * @brief Looks up the current definition of \p symbol.
     * @return Returns nullptr on failure.
     */
    const Decl* lookup(Symbol symbol) const;

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

    /// Opens a new scope.
    void push_scope() { levels_.push_back(decl_stack_.size()); }
    /// Discards current scope.
    void pop_scope();
    /// Has an error occurred?
    bool result() const { return result_; }
    /// Emit an error while using \p n as \p Location.
    std::ostream& error(const ASTNode* n);
    /// Emit an error at \p Location \p loc.
    std::ostream& error(const Location& loc);

private:
    size_t depth() const { return levels_.size(); }

    std::unordered_map<Symbol, const Decl*> sym2decl_;
    std::vector<const Decl*> decl_stack_;
    std::vector<size_t> levels_;
    bool result_;
};

}

#endif
