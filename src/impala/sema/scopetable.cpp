#include "impala/sema/scopetable.h"

#include "impala/ast.h"

namespace impala {

const Decl* ScopeTable::lookup(Symbol sym) const {
    auto i = sym2decl_.find(sym);
    return i != sym2decl_.end() ? i->second : nullptr;
}

void ScopeTable::insert(const Decl* decl) {
    if (const Decl* other = clash(decl->symbol())) {
        error(decl) << "symbol '" << decl->symbol() << "' already defined\n";
        error(other) << "previous location here\n";
        return;
    }

    Symbol symbol = decl->symbol();
    assert(clash(symbol) == nullptr && "must not be found");

    auto i = sym2decl_.find(symbol);
    decl->shadows_ = i != sym2decl_.end() ? i->second : nullptr;
    decl->depth_ = depth();
    decl_stack_.push_back(decl);
    sym2decl_[symbol] = decl;
}

const Decl* ScopeTable::clash(Symbol symbol) const {
    auto i = sym2decl_.find(symbol);
    if (i == sym2decl_.end())
        return nullptr;
    const Decl* decl = i->second;
    return (decl && decl->depth() == depth()) ? decl : nullptr;
}

void ScopeTable::pop_scope() {
    size_t level = levels_.back();
    for (size_t i = level, e = decl_stack_.size(); i != e; ++i) {
        const Decl* decl = decl_stack_[i];
        sym2decl_[decl->symbol()] = decl->shadows();
    }

    decl_stack_.resize(level);
    levels_.pop_back();
}

std::ostream& ScopeTable::error(const ASTNode* n) { result_ = false; return n->error(); }
std::ostream& ScopeTable::error(const Location& loc) { result_ = false; return loc.error(); }

}
