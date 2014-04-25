#include "impala/ast.h"
#include "impala/dump.h"
#include "impala/sema/errorhandler.h"

#include <sstream>

namespace impala {

//------------------------------------------------------------------------------

class NameSema : public ErrorHandler {
public:
    /** 
     * @brief Looks up the current definition of \p symbol.
     * Reports an error at location of \p n if was \p symbol was not found.
     * @return Returns nullptr on failure.
     */
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

    void check(const ASTType* ast_type) { ast_type->check(*this); }
    void check(const TypeDecl* type_decl) { type_decl->check(*this); }
    void check(const Item* item) { 
        if (auto type_decl_item = item->isa<TypeDeclItem>())
            check(type_decl_item);
        else if (auto value_item = item->isa<ValueItem>())
            check(value_item);
        else
            check(item->isa<MiscItem>());
    }
    void check_head(const Item* item) {
        if (auto decl = item->isa<Decl>())
            insert(decl);
    }
    void check(const TypeDeclItem* type_decl_item) { type_decl_item->check(*this); }
    void check(const ValueItem* value_item) { value_item->check(*this); }
    void check(const MiscItem* misc_item) { misc_item->check(*this); }

private:
    size_t depth() const { return levels_.size(); }

    thorin::HashMap<Symbol, const Decl*> symbol2decl_;
    std::vector<const Decl*> decl_stack_;
    std::vector<size_t> levels_;
};

//------------------------------------------------------------------------------

const Decl* NameSema::lookup(const ASTNode* n, Symbol symbol) {
    assert(symbol && "symbol is empty");
    auto decl = thorin::find(symbol2decl_, symbol);
    if (decl == nullptr)
        error(n) << '\'' << symbol << "' not found in current scope\n";
    return decl;
}

void NameSema::insert(const Decl* decl) {
    assert(decl->symbol() && "symbol is empty");
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
    assert(symbol && "symbol is empty");
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

void TypeParam::check(NameSema& sema) const {
    for (const ASTType* bound : bounds())
        sema.check(bound);
}

/*
 * ASTType::check
 */

void TypeParamList::check_type_params(NameSema& sema) const {
    // we need two runs for types like fn[A:T[B], B:T[A]](A, B)
    // first, insert names
    for (const TypeParam* tp : type_params())
        sema.insert(tp);

    // then, check bounds
    for (const TypeParam* tp : type_params())
        sema.check(tp);
}

void ErrorASTType::check(NameSema& sema) const {}
void PrimASTType::check(NameSema& sema) const {}
void PtrASTType::check(NameSema& sema) const { sema.check(referenced_type()); }
void IndefiniteArrayASTType::check(NameSema& sema) const { sema.check(elem_type()); }
void DefiniteArrayASTType::check(NameSema& sema) const { sema.check(elem_type()); }

void TupleASTType::check(NameSema& sema) const {
    for (auto elem : this->elems())
        sema.check(elem);
}

void ASTTypeApp::check(NameSema& sema) const {
    decl_ = sema.lookup(this, symbol());
    for (auto elem : elems())
        sema.check(elem);
}

void FnASTType::check(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    for (auto elem : elems())
        sema.check(elem);
    sema.pop_scope();
}

//------------------------------------------------------------------------------

void ModContents::check(NameSema& sema) const {
    for (auto item : items()) sema.check_head(item);
    for (auto item : items()) sema.check(item);
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

void Fn::fn_check(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    int i = 0;
    for (const Param* param : params()) {
        if (!param->symbol())  {
            std::ostringstream oss;
            oss << '<' << i << ">";
            const_cast<Param*>(param)->symbol_ = oss.str().c_str();
        }

        sema.insert(param);
        if (param->ast_type())
            sema.check(param->ast_type());
        ++i;
    }
    if (body() != nullptr)
        body()->check(sema);
    sema.pop_scope();
}

void FnDecl::check(NameSema& sema) const { 
    fn_check(sema); 
#ifndef NDEBUG
    for (auto param : params())
        assert(param->ast_type());
#endif
}

void StructDecl::check(NameSema& sema) const {
    sema.push_scope();
    for (auto field : fields()) {
        field->check(sema);
        field_table_[field->symbol()] = field;
    }
    sema.pop_scope();
}

void FieldDecl::check(NameSema& sema) const {
    sema.check(ast_type());
    sema.insert(this);
}

void TraitDecl::check(NameSema& sema) const {
    sema.push_scope();
    sema.insert(self_param());
    check_type_params(sema);
    for (auto t : super())
        sema.check(t);
    for (auto method : methods()) {
        method->check(sema);
        method_table_[method->symbol()] = method;
    }
    sema.pop_scope();
}

void Impl::check(NameSema& sema) const {
    sema.push_scope();
    check_type_params(sema);
    if (trait())
        sema.check(trait());
    sema.check(for_type());
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
    sema.push_scope();
    for (auto stmt : stmts()) {
        if (auto item_stmt = stmt->isa<ItemStmt>())
            sema.check_head(item_stmt->item());
    }
    for (auto stmt : stmts())
        stmt->check(sema);
    expr()->check(sema);
    sema.pop_scope();
}

void LiteralExpr::check(NameSema& sema) const {} 
void FnExpr::check(NameSema& sema) const { fn_check(sema); }

void PathElem::check(NameSema& sema) const {
    decl_ = sema.lookup(this, symbol());
    for (auto arg : args())
        sema.check(arg);
}

void Path::check(NameSema& sema) const {
    for (auto path_elem : path_elems())
        path_elem->check(sema);
}

void PathExpr::check(NameSema& sema) const {
    path()->check(sema);
    if (path()->decl()) {
        value_decl_ = path()->decl()->isa<ValueDecl>();
        if (!value_decl_)
            sema.error(this) << '\'' << path() << "' is not a value\n";
    }
}

void PrefixExpr::check(NameSema& sema) const  {                     rhs()->check(sema); }
void InfixExpr::check(NameSema& sema) const   { lhs()->check(sema); rhs()->check(sema); }
void PostfixExpr::check(NameSema& sema) const { lhs()->check(sema); }

void FieldExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    // don't check symbol here as it depends on lhs' type - must be done in TypeSema
    for (auto arg : path_elem()->args())
        sema.check(arg);
}

void CastExpr::check(NameSema& sema) const {
    lhs()->check(sema);
    sema.check(ast_type());
}

void DefiniteArrayExpr::check(NameSema& sema) const {
    for (auto elem : elems())
        elem->check(sema);
}

void RepeatedDefiniteArrayExpr::check(NameSema& sema) const {
    value()->check(sema);
    count()->check(sema);
}

void IndefiniteArrayExpr::check(NameSema& sema) const {
    size()->check(sema);
    sema.check(elem_type());
}

void TupleExpr::check(NameSema& sema) const {
    for (auto elem : elems())
        elem->check(sema);
}

void StructExpr::check(NameSema& sema) const {
    path()->check(sema);
    for (const auto& elem : elems()) {
        elem.expr()->check(sema);
        // TODO lookup elem.symbol()
    }
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
    expr()->check(sema);
    break_decl()->check(sema);
    fn_expr()->check(sema);
}

//------------------------------------------------------------------------------

/*
 * Stmt::check
 */

void ExprStmt::check(NameSema& sema) const { expr()->check(sema); }
void ItemStmt::check(NameSema& sema) const { sema.check(item()); }
void LetStmt::check(NameSema& sema) const {
    if (init())
        init()->check(sema);
    local()->check(sema);
}

//------------------------------------------------------------------------------

void ValueDecl::check(NameSema& sema) const {
    if (ast_type())
        sema.check(ast_type());
    sema.insert(this);
}

//------------------------------------------------------------------------------

bool name_analysis(const ModContents* mod) {
    NameSema sema;
    mod->check(sema);
    return sema.result();
}

//------------------------------------------------------------------------------

}
