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

/*
 * ASTType::to_type
 */

void ParametricASTType::check_type_params(NameSema& sema) const {
    for (const TypeParam* tp : type_params())
        sema.insert(tp);

    for (const TypeParam* tp : type_params()) {
        for (const ASTType* b : tp->bounds()) {
            if (auto trait_inst = b->isa<ASTTypeApp>())
                trait_inst->to_trait(sema);
            else
                sema.error(tp) << "bounds must be trait instances, not types\n";
        }
    }
}

void ErrorASTType::to_type(NameSema& sema) const {}
void PrimASTType::to_type(NameSema& sema) const {}
void PtrASTType::to_type(NameSema& sema) const { referenced_type()->to_type(sema); }
void IndefiniteArrayASTType::to_type(NameSema& sema) const { elem_type()->to_type(sema); }
void DefiniteArrayASTType::to_type(NameSema& sema) const { elem_type()->to_type(sema); }

void TupleASTType::to_type(NameSema& sema) const {
    for (auto elem : this->elems())
        elem->to_type(sema);
}

void ASTTypeApp::to_type(NameSema& sema) const {
    if (auto decl = sema.lookup(this, symbol())) {
        type_param_ = decl->isa<TypeParam>();
        if (!type_param_)
            sema.error(this) << "cannot convert a trait instance into a type\n";
    }
}

void FnASTType::to_type(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    sema.pop_scope();
}

void ASTTypeApp::to_trait(NameSema& sema) const {
    if (auto decl = sema.lookup(this, symbol())) {
        if (auto trait_decl = decl->isa<TraitDecl>()) {
            // sth TODO here?
        } else
            sema.error(this) << "cannot convert a type variable into a trait instance\n";
    }
}

//------------------------------------------------------------------------------

void ModContents::check(NameSema& sema) const {
    for (auto item : items()) item->check_head(sema);
    for (auto item : items()) item->check(sema);
}

/*
 * Item::check_head
 */

void ModDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void ForeignMod::check_head(NameSema& sema) const {
    sema.insert(this);
}

void EnumDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void FnDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void StaticItem::check_head(NameSema& sema) const {
    sema.insert(this);
}

void StructDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void TraitDecl::check_head(NameSema& sema) const {
    sema.insert(this);
}

void Typedef::check_head(NameSema& sema) const {
    sema.insert(this);
}

void Impl::check_head(NameSema& sema) const {
}

//------------------------------------------------------------------------------

/*
 * Item::check
 */

void ModDecl::check(NameSema& sema) const {
    sema.push_scope();
    if (mod_contents())
        mod_contents()->check(sema);
    sema.pop_scope();
}

void ForeignMod::check(NameSema& sema) const {
}

void Typedef::check(NameSema& sema) const {
}

void EnumDecl::check(NameSema& sema) const {
}

void StaticItem::check(NameSema& sema) const {
}

void FnDecl::check(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    for (const Param* param : fn().params()) {
        sema.insert(param);
        param->ast_type()->to_type(sema);
    }
    fn().body()->check(sema);
    sema.pop_scope();
}

void StructDecl::check(NameSema& sema) const {
}

void TraitDecl::check(NameSema& sema) const {
    // FEATURE consider super traits and check methods
    check_type_params(sema);
    for (auto tp : type_params()) {
        // TODO
    }
    for (auto method : methods())
        method->check(sema);
}

void Impl::check(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    if (trait() != nullptr) {
        if (auto t = trait()->isa<ASTTypeApp>()) {
            t->to_trait(sema);
            for (auto tp : type_params()) {
                // TODO
            }
        } else
            sema.error(trait()) << "expected trait instance.\n";
    }
    for_type()->to_type(sema);

    for (auto fn : methods())
        fn->check(sema);
    sema.pop_scope();
}

//------------------------------------------------------------------------------

/*
 * Expr::check
 */

void EmptyExpr::check(NameSema& sema) const {}

void BlockExpr::check(NameSema& sema) const {
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            item_stmt->item()->check_head(sema);
    }

    for (auto stmt : stmts())
        stmt->check(sema);

    expr()->check(sema);
    set_type(expr()->type());
}

void LiteralExpr::check(NameSema& sema) const {} 

void FnExpr::check(NameSema& sema) const {
}

void PathExpr::check(NameSema& sema) const {
#if 0
    // FEATURE consider longer paths
    auto last_item = path()->path_items().back();

    if ((decl_ = sema.lookup(this, last_item->symbol()))) {
        if (auto vdec = decl_->isa<ValueDecl>()) {
            // consider type expressions
            if (!last_item->types().empty()) {
                std::vector<Type> type_args;
                for (const ASTType* t : last_item->types())
                    type_args.push_back(t->to_type(sema));

                set_type(vdec->calc_type(sema)->instantiate(type_args));
            } else
                set_type(vdec->calc_type(sema));
        }
    } else
        set_type(sema.type_error());
#endif
    // TODO
}

void PrefixExpr::check(NameSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(NameSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(NameSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(NameSema& sema) const {
}

void CastExpr::check(NameSema& sema) const {
}

void DefiniteArrayExpr::check(NameSema& sema) const {
}

void RepeatedDefiniteArrayExpr::check(NameSema& sema) const {
}

void IndefiniteArrayExpr::check(NameSema& sema) const {
}

void TupleExpr::check(NameSema& sema) const {
    for (auto elem : elems())
        elem->check(sema);
}

void StructExpr::check(NameSema& sema) const {
}

void MapExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    for (auto arg : args())
        arg->check(sema);
}

void IfExpr::check(NameSema& sema) const {
    cond()->check(sema);
    then_expr()->check(sema);
    else_expr()->check(sema);
}

void ForExpr::check(NameSema& sema) const {
}

//------------------------------------------------------------------------------

/*
 * Stmt::check
 */

void ExprStmt::check(NameSema& sema) const { expr()->check(sema); }
void ItemStmt::check(NameSema& sema) const { item()->check(sema); }
void LetStmt::check(NameSema& sema) const {
    if (init())
        init()->check(sema);
    // TODO handle local
}

//------------------------------------------------------------------------------

bool name_analysis(const ModContents* mod) {
    NameSema sema;
    mod->check(sema);
    return sema.result();
}

//------------------------------------------------------------------------------

}
